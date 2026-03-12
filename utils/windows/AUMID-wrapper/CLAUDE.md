# AppWrapper — Windows Taskbar Icon / AUMID Override Wrapper

A C++/CMake project that produces self-contained Windows wrapper executables.
Each wrapper launches a target `.exe`, overrides its taskbar icon and
AppUserModelID (AUMID), and injects environment variables — all without
modifying the target application.

> **For AI assistants**: Keep this file up to date whenever you change the
> code, build system, or configuration schema.  This is the authoritative
> reference for the project — future sessions rely on it.

---

## How it works (runtime)

1. **Apply env vars** — modify the wrapper's own environment before spawning
   the child; the child inherits it automatically.
2. **Set process AUMID** — call `SetCurrentProcessExplicitAppUserModelID` so
   the OS uses our AUMID for the initial taskbar grouping.
3. **Launch target** — `CreateProcessW` with the baked-in path + arguments.
4. **Find main window** — poll `EnumWindows` against the entire process subtree
   (direct child + all descendants, rebuilt each poll via
   `CreateToolhelp32Snapshot`) until a visible, titled window appears (60-second
   timeout).  This handles launcher executables (e.g. `runemacs.exe`) that spawn
   the real GUI process as a child and then exit.
5. **Override window AUMID** — `SHGetPropertyStoreForWindow` + `IPropertyStore`
   to stamp `PKEY_AppUserModel_ID` on the window.
6. **Override window icon** — `LoadImageW` + `WM_SETICON` (both ICON_BIG and
   ICON_SMALL).  The icon is loaded from the exe's own embedded resource
   (`IDI_APPICON`, resource ID 101) via `GetModuleHandleW(nullptr)` +
   `MAKEINTRESOURCEW`; no external file is needed at runtime.
7. **Monitor loop** — `WaitForSingleObject` on the monitored process with
   2-second timeout; re-applies icon (and AUMID if the window is recreated)
   every cycle.  If the window belongs to a descendant process, that process's
   handle is used for monitoring (opened via `OpenProcess`) instead of the
   launcher's handle.

**Command-line forwarding** — any arguments passed to the wrapper at runtime
are appended after `CONFIG_ARGUMENTS` when launching the target:
`"<targetExe>" [CONFIG_ARGUMENTS] [runtime args]`.

**Debug logging** — set the environment variable `AUMID_WRAPPER_DEBUG` to any
non-empty value to write a `wrapper_debug.log` file next to the exe (in
addition to `OutputDebugStringW`).

---

## Directory structure

```
AppWrapper/
├── CLAUDE.md
├── CMakeLists.txt
├── main.cpp
├── config.h.in          ← format reference; NOT used by the build system
├── build.ps1
├── scripts/             ← utility scripts (not part of the build)
│   ├── Set-ShortcutAUMID.ps1
│   ├── Get-WindowAUMID.ps1
│   └── icons/
│       ├── make_icons.py
│       └── input/       ← source SVG/PNG files for icon generation
├── profiles/
│   └── *.json           ← one file per wrapper variant
├── build/               ← CMake build trees (gitignore)
└── out/
    └── <Name>/
        └── <Name>.exe   ← fully self-contained; icon embedded inside
```

---

## Configuration schema (`profiles/*.json`)

```json
{
  "targetExe":        "C:\\Path\\To\\app.exe",      // required
  "arguments":        "--flag value",                // optional
  "workingDirectory": "C:\\Path\\To",                // optional; defaults to targetExe's directory
  "appUserModelId":   "MyCompany.MyApp",             // required
  "iconPath":         "C:\\Path\\To\\icon.ico",      // required; must be a .ico file
  "monitorLoop": true,
  "environmentVariables": [
    { "name": "MY_VAR", "value": "val",              "mode": "set"     },
    { "name": "PATH",   "value": "C:\\MyApp\\bin",   "mode": "prepend", "separator": ";" },
    { "name": "LIBS",   "value": "/opt/lib",         "mode": "append",  "separator": ":" }
  ]
}
```

`mode` is one of `"set"` (overwrite), `"prepend"`, or `"append"`.
`separator` defaults to `";"` and is ignored for `"set"`.

---

## Build system

### Requirements

- Windows 10/11
- Visual Studio 2022 (or Build Tools) with:
  - MSVC C++ compiler
  - CMake component (`Microsoft.VisualStudio.Component.VC.CMake.Project`)
  - Windows 10 SDK (for `propkey.h`, `propvarutil.h`)

### Build a profile

```powershell
# All profiles in profiles\
.\build.ps1

# Single profile
.\build.ps1 -ProfilePath profiles\myapp.json
```

### What build.ps1 does per profile

1. Reads the JSON profile.
2. Writes `generated_config.h` into the source directory (wide-string constants
   + `EnvVarEntry` array).
3. Writes `app.rc` into the source directory with an absolute forward-slash icon
   path (avoids RC-compiler escaping issues).
4. Derives the exe name from the profile filename stem by stripping characters
   invalid in CMake target names, preserving hyphens and underscores
   (e.g. `emacs-win.json` → `emacs-win.exe`).
5. Runs `cmake -S . -B build\<Name> -G "Visual Studio 17 2022" -A x64
   -DAPP_NAME=<Name> -DICON_RC_FILE=app.rc` then `cmake --build ... --config Release`.
6. Copies `<Name>.exe` to `out\<Name>\` (icon is embedded; no `app.ico` copy needed).
7. Deletes `generated_config.h` and `app.rc`.

If `cmake` is not on PATH, `build.ps1` locates it via `vswhere.exe`.

### CMakeLists.txt highlights

- `add_executable(${APP_NAME} WIN32 ...)` sets `/SUBSYSTEM:WINDOWS` (no console).
- Links: `shell32`, `ole32`, `user32`, `propsys`.
- `APP_NAME` and `ICON_RC_FILE` are passed via `-D` from `build.ps1`.

---

## Key implementation notes

- **`#include <initguid.h>` before `<propkey.h>`** — required so
  `DEFINE_PROPERTYKEY` emits the actual `PKEY_AppUserModel_ID` definition
  (not just a declaration) in the single translation unit.
- **Empty env-var array** — when a profile has no `environmentVariables`,
  `build.ps1` emits a single dummy entry and sets `CONFIG_ENV_VAR_COUNT = 0`
  to keep MSVC happy (zero-length arrays are non-standard).
- **Launcher executables** — if the target (e.g. `runemacs.exe`) spawns the
  real GUI process as a child and itself has no window, the subtree search finds
  the child's window; the wrapper then monitors the child's process handle
  (not the launcher's) so it stays alive as long as the GUI does.
- **Icon handles** — `DestroyIcon` is intentionally NOT called while the window
  is alive; the window holds references to the icon handles.
- **Embedded icon** — `app.rc` embeds the icon as resource `IDI_APPICON` (ID 101);
  `generated_config.h` defines `#define IDI_APPICON 101` so `main.cpp` can
  reference it.  `OverrideWindowIcon` loads via `GetModuleHandleW(nullptr)` +
  `MAKEINTRESOURCEW(IDI_APPICON)` — no `app.ico` file needed alongside the exe.

---

## Utility scripts (`scripts/`)

### Set-ShortcutAUMID.ps1

Stamps a matching AUMID onto a `.lnk` file so Windows merges the shortcut's
taskbar button with the running wrapper instance:

```powershell
.\scripts\Set-ShortcutAUMID.ps1 `
    -ShortcutPath  "C:\Users\Me\Desktop\MyApp.lnk" `
    -AppUserModelID "MyCompany.MyApp"
```

Uses inline C# (`Add-Type`) to call `SHGetPropertyStoreFromParsingName` with
`GPS_READWRITE` and sets `PKEY_AppUserModel_ID` via `IPropertyStore::SetValue`.

### Get-WindowAUMID.ps1

Queries the AUMID currently set on a running application's main window.  Useful
for checking whether a target app sets its own explicit AUMID (which the wrapper
would need to override) or leaves it to Windows defaults.

```powershell
.\scripts\Get-WindowAUMID.ps1 notepad
.\scripts\Get-WindowAUMID.ps1 firefox
```

Uses inline C# (`Add-Type`) to call `SHGetPropertyStoreForWindow` and reads
`PKEY_AppUserModel_ID` via `IPropertyStore::GetValue`.  No admin privileges
required; the target app must be running with a visible window.

### icons/make_icons.py

Generates composite `.ico` files for the Emacs wrapper profiles by overlaying a
platform badge (MSys2, Cygwin, or Windows logo) onto the base Emacs icon.
Source images live in `scripts/icons/input/`; output `.ico` files are written to
`scripts/icons/`.

```bash
python scripts/icons/make_icons.py
```

Requires Python 3, Pillow (`pip install Pillow`), and `rsvg-convert` (from
librsvg) on PATH for SVG rendering.  Copy the generated `.ico` files into the
matching `profiles/<name>/` directories afterward.

---

## End-to-end usage

1. Author `profiles\myapp.json` with the correct paths and AUMID.
2. Run `.\build.ps1 -ProfilePath profiles\myapp.json`.
3. The output `out\MyApp\MyApp.exe` is fully self-contained — ship just the exe.
4. Create a Windows shortcut to `MyApp.exe`, set its icon, then run
   `scripts\Set-ShortcutAUMID.ps1` to stamp the AUMID on the `.lnk`.
5. Pin the shortcut to the taskbar.

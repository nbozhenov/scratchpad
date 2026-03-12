#include <windows.h>
#include <initguid.h>
#include <shellapi.h>
#include <shlobj.h>
#include <shobjidl.h>
#include <propsys.h>
#include <propkey.h>
#include <propvarutil.h>
#include <tlhelp32.h>
#include <string>
#include <vector>
#include "generated_config.h"

// ---------------------------------------------------------------------------
// Globals (debug mode)
// ---------------------------------------------------------------------------

static bool        g_debugMode = false;
static std::wstring g_logPath;

// ---------------------------------------------------------------------------
// Logging
// ---------------------------------------------------------------------------

static void DebugLog(const wchar_t* msg)
{
    OutputDebugStringW(msg);
    OutputDebugStringW(L"\n");

    if (!g_debugMode || g_logPath.empty())
        return;

    HANDLE hFile = CreateFileW(
        g_logPath.c_str(), FILE_APPEND_DATA, FILE_SHARE_READ,
        nullptr, OPEN_ALWAYS, FILE_ATTRIBUTE_NORMAL, nullptr);

    if (hFile == INVALID_HANDLE_VALUE)
        return;

    // Convert to UTF-8 for the log file
    int needed = WideCharToMultiByte(CP_UTF8, 0, msg, -1, nullptr, 0, nullptr, nullptr);
    if (needed > 0)
    {
        std::string utf8(needed - 1, '\0');
        WideCharToMultiByte(CP_UTF8, 0, msg, -1, &utf8[0], needed, nullptr, nullptr);
        utf8 += "\r\n";
        DWORD written = 0;
        WriteFile(hFile, utf8.c_str(), (DWORD)utf8.size(), &written, nullptr);
    }
    CloseHandle(hFile);
}

static void DebugLog(const std::wstring& msg) { DebugLog(msg.c_str()); }

// ---------------------------------------------------------------------------
// Helper: directory of this exe
// ---------------------------------------------------------------------------

static std::wstring GetExeDirectory()
{
    wchar_t path[MAX_PATH];
    DWORD len = GetModuleFileNameW(nullptr, path, MAX_PATH);
    if (len == 0)
        return L"";
    std::wstring full(path, len);
    size_t pos = full.rfind(L'\\');
    return (pos != std::wstring::npos) ? full.substr(0, pos) : L"";
}

// ---------------------------------------------------------------------------
// Step 1: Apply environment variables
// ---------------------------------------------------------------------------

static void ApplyEnvironmentVariables()
{
    for (int i = 0; i < CONFIG_ENV_VAR_COUNT; i++)
    {
        const auto& entry = CONFIG_ENV_VARS[i];
        std::wstring mode      = entry.mode;
        std::wstring separator = entry.separator;

        if (mode == L"set")
        {
            SetEnvironmentVariableW(entry.name, entry.value);
        }
        else
        {
            wchar_t buf[32768];
            DWORD len = GetEnvironmentVariableW(entry.name, buf, 32768);
            std::wstring existing(buf, len);

            std::wstring finalValue;
            if (mode == L"prepend")
            {
                finalValue = existing.empty()
                    ? entry.value
                    : std::wstring(entry.value) + separator + existing;
            }
            else // "append"
            {
                finalValue = existing.empty()
                    ? entry.value
                    : existing + separator + std::wstring(entry.value);
            }
            SetEnvironmentVariableW(entry.name, finalValue.c_str());
        }
    }
}

// ---------------------------------------------------------------------------
// Step 4: Find target window by PID (searches entire process subtree)
// ---------------------------------------------------------------------------

// Returns all PIDs in the process tree rooted at rootPid.
// Rebuilt on each poll so that child processes spawned after launch are found.
static std::vector<DWORD> GetProcessSubtree(DWORD rootPid)
{
    std::vector<DWORD> result;
    result.push_back(rootPid);

    HANDLE snap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    if (snap == INVALID_HANDLE_VALUE)
        return result;

    // Expand iteratively until no new PIDs are added
    bool changed = true;
    while (changed)
    {
        changed = false;
        PROCESSENTRY32W pe = { sizeof(pe) };
        if (!Process32FirstW(snap, &pe))
            break;
        do
        {
            DWORD thisPid   = pe.th32ProcessID;
            DWORD parentPid = pe.th32ParentProcessID;

            bool parentKnown = false;
            for (DWORD p : result)
                if (p == parentPid) { parentKnown = true; break; }
            if (!parentKnown)
                continue;

            bool alreadyKnown = false;
            for (DWORD p : result)
                if (p == thisPid) { alreadyKnown = true; break; }
            if (!alreadyKnown)
            {
                result.push_back(thisPid);
                changed = true;
            }
        } while (Process32NextW(snap, &pe));
    }

    CloseHandle(snap);
    return result;
}

struct FindWindowData
{
    const std::vector<DWORD>* pids;
    HWND  resultHwnd;
    DWORD foundPid;
};

static BOOL CALLBACK EnumWindowsCallback(HWND hwnd, LPARAM lParam)
{
    auto* data = reinterpret_cast<FindWindowData*>(lParam);
    DWORD windowPid = 0;
    GetWindowThreadProcessId(hwnd, &windowPid);

    if (IsWindowVisible(hwnd) && GetWindowTextLengthW(hwnd) > 0)
    {
        for (DWORD pid : *data->pids)
        {
            if (windowPid == pid)
            {
                data->resultHwnd = hwnd;
                data->foundPid   = windowPid;
                return FALSE; // stop enumeration
            }
        }
    }
    return TRUE;
}

// Searches the process subtree rooted at rootPid for a visible, titled window.
// outFoundPid receives the PID that owns the found window (may be a child).
static HWND FindTargetWindow(DWORD rootPid, DWORD* outFoundPid = nullptr, int timeoutMs = 60000)
{
    int elapsed = 0;
    while (elapsed < timeoutMs)
    {
        std::vector<DWORD> pids = GetProcessSubtree(rootPid);
        FindWindowData data = { &pids, nullptr, 0 };
        EnumWindows(EnumWindowsCallback, reinterpret_cast<LPARAM>(&data));

        if (data.resultHwnd)
        {
            // Brief pause then re-verify (guards against splash screens)
            Sleep(500);
            if (IsWindow(data.resultHwnd) && IsWindowVisible(data.resultHwnd))
            {
                if (outFoundPid)
                    *outFoundPid = data.foundPid;
                return data.resultHwnd;
            }
        }

        Sleep(200);
        elapsed += 200;
    }
    return nullptr;
}

// ---------------------------------------------------------------------------
// Step 5: Override window AUMID via IPropertyStore
// ---------------------------------------------------------------------------

static void OverrideWindowAUMID(HWND hwnd, const wchar_t* aumid)
{
    IPropertyStore* store = nullptr;
    HRESULT hr = SHGetPropertyStoreForWindow(hwnd, IID_PPV_ARGS(&store));
    if (FAILED(hr) || !store)
    {
        DebugLog(std::wstring(L"OverrideWindowAUMID: SHGetPropertyStoreForWindow failed hr=0x")
            + std::to_wstring(hr));
        return;
    }

    PROPVARIANT pv;
    hr = InitPropVariantFromString(aumid, &pv);
    if (SUCCEEDED(hr))
    {
        hr = store->SetValue(PKEY_AppUserModel_ID, pv);
        if (FAILED(hr))
            DebugLog(std::wstring(L"OverrideWindowAUMID: SetValue failed hr=0x") + std::to_wstring(hr));

        hr = store->Commit();
        if (FAILED(hr))
            DebugLog(std::wstring(L"OverrideWindowAUMID: Commit failed hr=0x") + std::to_wstring(hr));

        PropVariantClear(&pv);
    }
    else
    {
        DebugLog(std::wstring(L"OverrideWindowAUMID: InitPropVariantFromString failed hr=0x") + std::to_wstring(hr));
    }

    store->Release();
}

// ---------------------------------------------------------------------------
// Step 6: Override window icon via WM_SETICON
// ---------------------------------------------------------------------------

static void OverrideWindowIcon(HWND hwnd)
{
    int cxLarge = GetSystemMetrics(SM_CXICON);
    int cyLarge = GetSystemMetrics(SM_CYICON);
    int cxSmall = GetSystemMetrics(SM_CXSMICON);
    int cySmall = GetSystemMetrics(SM_CYSMICON);

    HMODULE hSelf = GetModuleHandleW(nullptr);
    HICON hLarge = (HICON)LoadImageW(
        hSelf, MAKEINTRESOURCEW(IDI_APPICON), IMAGE_ICON, cxLarge, cyLarge, LR_DEFAULTCOLOR);
    HICON hSmall = (HICON)LoadImageW(
        hSelf, MAKEINTRESOURCEW(IDI_APPICON), IMAGE_ICON, cxSmall, cySmall, LR_DEFAULTCOLOR);

    if (hLarge)
        SendMessageW(hwnd, WM_SETICON, ICON_BIG,   (LPARAM)hLarge);
    else
        DebugLog(L"OverrideWindowIcon: Failed to load large icon");

    if (hSmall)
        SendMessageW(hwnd, WM_SETICON, ICON_SMALL, (LPARAM)hSmall);
    else
        DebugLog(L"OverrideWindowIcon: Failed to load small icon");

    // Do NOT DestroyIcon — the window holds the references while alive.
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

int WINAPI wWinMain(HINSTANCE /*hInstance*/, HINSTANCE /*hPrevInstance*/,
                    LPWSTR lpCmdLine, int /*nCmdShow*/)
{
    // Enable debug logging when AUMID_WRAPPER_DEBUG is set in the environment
    {
        wchar_t buf[4];
        if (GetEnvironmentVariableW(L"AUMID_WRAPPER_DEBUG", buf, 4) > 0)
        {
            g_debugMode = true;
            g_logPath   = GetExeDirectory() + L"\\wrapper_debug.log";
        }
    }

    DebugLog(L"AppWrapper starting");

    // Initialize COM (needed for IPropertyStore)
    HRESULT hr = CoInitializeEx(nullptr, COINIT_APARTMENTTHREADED);
    if (FAILED(hr))
        DebugLog(std::wstring(L"CoInitializeEx failed hr=0x") + std::to_wstring(hr));

    // ------------------------------------------------------------------
    // Step 1: Apply environment variables
    // ------------------------------------------------------------------
    ApplyEnvironmentVariables();
    DebugLog(L"Environment variables applied");

    // ------------------------------------------------------------------
    // Step 2: Set process-level AUMID
    // ------------------------------------------------------------------
    hr = SetCurrentProcessExplicitAppUserModelID(CONFIG_AUMID);
    if (FAILED(hr))
        DebugLog(std::wstring(L"SetCurrentProcessExplicitAppUserModelID failed hr=0x") + std::to_wstring(hr));
    else
        DebugLog(std::wstring(L"Process AUMID set: ") + CONFIG_AUMID);

    // ------------------------------------------------------------------
    // Step 3: Launch target process
    // ------------------------------------------------------------------
    // Build child command line:
    //   "<targetExe>" [CONFIG_ARGUMENTS] [runtime args forwarded from wrapper]
    std::wstring cmdLine = std::wstring(L"\"") + CONFIG_TARGET_EXE + L"\"";
    if (CONFIG_ARGUMENTS[0] != L'\0')
    {
        cmdLine += L" ";
        cmdLine += CONFIG_ARGUMENTS;
    }
    if (lpCmdLine && lpCmdLine[0] != L'\0')
    {
        cmdLine += L" ";
        cmdLine += lpCmdLine;
    }

    // Working directory: explicit config, or derived from the target exe path
    std::wstring effectiveWorkDir;
    if (CONFIG_WORKING_DIR[0] != L'\0')
    {
        effectiveWorkDir = CONFIG_WORKING_DIR;
    }
    else
    {
        std::wstring target = CONFIG_TARGET_EXE;
        size_t pos = target.rfind(L'\\');
        if (pos != std::wstring::npos)
            effectiveWorkDir = target.substr(0, pos);
    }
    const wchar_t* workDir = effectiveWorkDir.empty() ? nullptr : effectiveWorkDir.c_str();

    DebugLog(std::wstring(L"Launching: ") + cmdLine);

    STARTUPINFOW       si = { sizeof(si) };
    PROCESS_INFORMATION pi = {};

    BOOL ok = CreateProcessW(
        nullptr,          // derive application name from cmdLine
        cmdLine.data(),   // must be writable
        nullptr, nullptr, // process / thread security attrs
        FALSE,            // don't inherit handles
        0,                // creation flags
        nullptr,          // inherit environment (already modified above)
        workDir,
        &si, &pi);

    if (!ok)
    {
        DWORD err = GetLastError();
        std::wstring errMsg = L"Failed to launch target executable.\nError code: "
            + std::to_wstring(err)
            + L"\nPath: " + CONFIG_TARGET_EXE;
        DebugLog(errMsg);
        MessageBoxW(nullptr, errMsg.c_str(), L"AppWrapper Error", MB_OK | MB_ICONERROR);
        CoUninitialize();
        return 1;
    }

    CloseHandle(pi.hThread); // we only need hProcess
    DebugLog(L"Target process launched");

    // ------------------------------------------------------------------
    // Step 4: Wait for the target's main window (search whole process subtree)
    // ------------------------------------------------------------------
    DebugLog(L"Waiting for target window...");
    DWORD windowPid  = 0;
    HWND  targetHwnd = FindTargetWindow(pi.dwProcessId, &windowPid, 60000);

    // If the window belongs to a child process (common for launcher executables
    // like runemacs.exe that spawn the real GUI process), open a handle to that
    // child so we monitor its lifetime instead of the launcher's.
    HANDLE hMonitorProcess = pi.hProcess;
    DWORD  monitorPid      = pi.dwProcessId;
    bool   ownMonitorHandle = false;

    if (targetHwnd && windowPid != 0 && windowPid != pi.dwProcessId)
    {
        HANDLE hChild = OpenProcess(SYNCHRONIZE | PROCESS_QUERY_INFORMATION, FALSE, windowPid);
        if (hChild)
        {
            hMonitorProcess  = hChild;
            monitorPid       = windowPid;
            ownMonitorHandle = true;
            DebugLog(std::wstring(L"Window owned by child PID ") + std::to_wstring(windowPid)
                + L" (launcher PID " + std::to_wstring(pi.dwProcessId) + L"); monitoring child");
        }
    }

    if (!targetHwnd)
    {
        DebugLog(L"Target window not found within timeout; monitoring process only");
    }
    else
    {
        DebugLog(L"Target window found");

        // Step 5 & 6: Override AUMID and icon
        OverrideWindowAUMID(targetHwnd, CONFIG_AUMID);
        DebugLog(L"Window AUMID overridden");

        OverrideWindowIcon(targetHwnd);
        DebugLog(L"Window icon overridden");
    }

    // ------------------------------------------------------------------
    // Step 7: Monitor and maintain until the child exits (optional)
    // ------------------------------------------------------------------
    if (CONFIG_MONITOR_LOOP)
    {
        DebugLog(L"Monitor loop enabled — re-applying overrides every 2 s");
        while (WaitForSingleObject(hMonitorProcess, 2000) == WAIT_TIMEOUT)
        {
            if (!targetHwnd || !IsWindow(targetHwnd) || !IsWindowVisible(targetHwnd))
            {
                // Window gone — app may have recreated it (e.g. splash → main)
                HWND newHwnd = FindTargetWindow(monitorPid, nullptr, 5000);
                if (newHwnd)
                {
                    targetHwnd = newHwnd;
                    OverrideWindowAUMID(targetHwnd, CONFIG_AUMID);
                    OverrideWindowIcon(targetHwnd);
                    DebugLog(L"Reapplied overrides to new/restored window");
                }
            }
            else
            {
                // Reapply icon in case the app reset it
                OverrideWindowIcon(targetHwnd);
            }
        }
    }
    else
    {
        DebugLog(L"Monitor loop disabled — waiting for process exit");
        WaitForSingleObject(hMonitorProcess, INFINITE);
    }

    DWORD exitCode = 0;
    GetExitCodeProcess(hMonitorProcess, &exitCode);
    if (ownMonitorHandle)
        CloseHandle(hMonitorProcess);
    CloseHandle(pi.hProcess);

    DebugLog(std::wstring(L"Target process exited with code ") + std::to_wstring(exitCode));
    CoUninitialize();
    return (int)exitCode;
}

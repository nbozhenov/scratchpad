# Get-WindowAumid.ps1
#
# PURPOSE:
#   Queries the Application User Model ID (AUMID) of a running application's
#   main window. This is useful when you want to know whether an app sets its
#   own explicit AUMID, which you may need to override with a wrapper.
#
# WHAT IS AN AUMID?
#   Windows uses AUMIDs to group taskbar buttons and associate windows with
#   shortcuts. An app can either:
#     1. Set an explicit AUMID by calling SetCurrentProcessExplicitAppUserModelID().
#        In this case, this script will return a string like "Company.AppName".
#     2. Not set one at all (the common case for traditional desktop apps).
#        Windows then groups windows by executable path internally.
#        This script will report "(no explicit AUMID set)" for such apps.
#
# HOW IT WORKS:
#   For each process matching the given name, the script finds the main window
#   handle (HWND), then calls the Win32 function SHGetPropertyStoreForWindow()
#   to obtain an IPropertyStore interface on that window. It reads the property
#   PKEY_AppUserModel_ID (a well-known property key defined by Windows).
#   If the property is set, that's the AUMID. If not, the app has no explicit one.
#
# USAGE:
#   .\Get-WindowAumid.ps1 <ProcessName>
#
#   ProcessName is the name of the process WITHOUT the .exe extension.
#   The target application must be running and have a visible window.
#
# EXAMPLES:
#   .\Get-WindowAumid.ps1 notepad
#   .\Get-WindowAumid.ps1 firefox
#   .\Get-WindowAumid.ps1 slack
#
# OUTPUT:
#   For each matching process with a window, prints a line like:
#     firefox (PID 12345): Mozilla.Firefox
#   or:
#     notepad (PID 6789): (no explicit AUMID set)
#
# REQUIREMENTS:
#   - Windows 7 or later (SHGetPropertyStoreForWindow is available since Win7)
#   - The target app must be running and have a visible main window
#   - No admin privileges required (queries your own desktop windows)
#
# NOTES:
#   - Some apps create multiple top-level windows. This script only checks
#     the MainWindowHandle reported by .NET's Process class. If the app's
#     AUMID is set on a different window, it may not be detected.
#   - UWP / Store apps will show their package-based AUMID
#     (e.g., "Microsoft.WindowsCalculator_8wekyb3d8bbwe!App").
#   - If an app has no explicit AUMID, your wrapper can freely assign any
#     AUMID string — there's nothing to "override" in that case.

param(
    # The process name to look up, without the .exe extension.
    # Example: "notepad", "firefox", "slack"
    [Parameter(Mandatory = $true, Position = 0,
               HelpMessage = "Process name without .exe (e.g. 'notepad')")]
    [string]$ProcessName
)

# ---------------------------------------------------------------------------
# Step 1: Define the C# interop code.
#
# We need to call two things that aren't natively available in PowerShell:
#   - SHGetPropertyStoreForWindow (from shell32.dll)
#     Takes a window handle, returns an IPropertyStore COM interface.
#   - IPropertyStore.GetValue (COM method)
#     Reads a property identified by a PROPERTYKEY struct.
#
# The PROPERTYKEY for AUMID is:
#   PKEY_AppUserModel_ID = { fmtid: 9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3, pid: 5 }
# This is defined in <propkey.h> in the Windows SDK.
#
# We use Add-Type to compile a small C# helper class inline. This is a
# standard PowerShell technique for calling Win32/COM APIs.
# ---------------------------------------------------------------------------

Add-Type @"
using System;
using System.Runtime.InteropServices;

/// <summary>
/// Helper class that reads the AppUserModel.ID property from a window handle.
/// </summary>
public class AumidReader
{
    // SHGetPropertyStoreForWindow retrieves an IPropertyStore for a given HWND.
    // This lets us read (or write) window-level properties, including the AUMID.
    // Defined in shell32.dll, available since Windows 7.
    [DllImport("shell32.dll")]
    private static extern int SHGetPropertyStoreForWindow(
        IntPtr hwnd,
        ref Guid iid,
        [MarshalAs(UnmanagedType.Interface)] out object propertyStore
    );

    /// <summary>
    /// Reads the AUMID from the given window handle.
    /// Returns the AUMID string, or a descriptive message if none is set / on error.
    /// </summary>
    public static string GetAumid(IntPtr hwnd)
    {
        // IID for IPropertyStore COM interface
        Guid IID_IPropertyStore = new Guid("886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99");

        object storeObj;
        int hr = SHGetPropertyStoreForWindow(hwnd, ref IID_IPropertyStore, out storeObj);
        if (hr != 0)
            return "(error: SHGetPropertyStoreForWindow returned 0x" + hr.ToString("X8") + ")";

        IPropertyStore store = (IPropertyStore)storeObj;

        // Build the PROPERTYKEY for System.AppUserModel.ID
        // GUID: {9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3}
        // PID:  5
        PROPERTYKEY key;
        key.fmtid = new Guid("9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3");
        key.pid = 5;

        object value;
        store.GetValue(ref key, out value);

        if (value == null)
            return "(no explicit AUMID set)";

        return value.ToString();
    }

    // ----- COM interface and struct definitions -----

    // IPropertyStore: a COM interface for reading/writing property values.
    // We only use GetValue here, but all methods must be declared to keep
    // the vtable layout correct (COM interfaces are vtable-based).
    [ComImport]
    [Guid("886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99")]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    public interface IPropertyStore
    {
        int GetCount(out uint cProps);
        int GetAt(uint iProp, out PROPERTYKEY pkey);
        int GetValue(ref PROPERTYKEY key, [MarshalAs(UnmanagedType.Struct)] out object pv);
        int SetValue(ref PROPERTYKEY key, ref object pv);
        int Commit();
    }

    // PROPERTYKEY: identifies a specific property.
    // Consists of a GUID (fmtid) that identifies the property group,
    // and a uint (pid) that identifies the specific property within that group.
    [StructLayout(LayoutKind.Sequential, Pack = 4)]
    public struct PROPERTYKEY
    {
        public Guid fmtid;
        public uint pid;
    }
}
"@

# ---------------------------------------------------------------------------
# Step 2: Find matching processes and query their windows.
# ---------------------------------------------------------------------------

$processes = Get-Process -Name $ProcessName -ErrorAction SilentlyContinue

if (-not $processes) {
    Write-Error "No running process found with name '$ProcessName'. Is the application running?"
    Write-Error "Hint: use the process name without .exe (e.g. 'notepad', not 'notepad.exe')."
    exit 1
}

$found = $false

foreach ($proc in $processes) {
    # MainWindowHandle is zero if the process has no visible window,
    # or if it's a background/service process.
    if ($proc.MainWindowHandle -ne [IntPtr]::Zero) {
        $aumid = [AumidReader]::GetAumid($proc.MainWindowHandle)
        Write-Host "$($proc.ProcessName) (PID $($proc.Id)): $aumid"
        $found = $true
    }
}

if (-not $found) {
    Write-Warning "Process '$ProcessName' is running but has no visible main window."
    Write-Warning "The application may still be loading, or it may be a background process."
    exit 1
}
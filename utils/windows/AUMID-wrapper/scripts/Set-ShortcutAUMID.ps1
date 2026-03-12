<#
.SYNOPSIS
    Stamps an AppUserModelID onto a Windows shortcut (.lnk) file.

.DESCRIPTION
    Sets the System.AppUserModel.ID property (PKEY_AppUserModel_ID) on the
    given .lnk file so that Windows merges the shortcut's taskbar button with
    the running process whose AUMID matches.

.PARAMETER ShortcutPath
    Absolute or relative path to the .lnk file.

.PARAMETER AppUserModelID
    The AUMID string to stamp (e.g. "MyCompany.MyApp").

.EXAMPLE
    .\Set-ShortcutAUMID.ps1 -ShortcutPath "C:\Users\Me\Desktop\MyApp.lnk" `
                             -AppUserModelID "MyCompany.MyApp"
#>
param(
    [Parameter(Mandatory = $true)]
    [string]$ShortcutPath,

    [Parameter(Mandatory = $true)]
    [string]$AppUserModelID
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

# Resolve to absolute path
$ShortcutPath = (Resolve-Path $ShortcutPath).Path

if (-not $ShortcutPath.EndsWith(".lnk", [System.StringComparison]::OrdinalIgnoreCase)) {
    Write-Warning "The file does not have a .lnk extension: $ShortcutPath"
}

# ---------------------------------------------------------------------------
# Inline C# that calls SHGetPropertyStoreFromParsingName + IPropertyStore
# to set PKEY_AppUserModel_ID = {9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3}, pid 5
# ---------------------------------------------------------------------------
$code = @'
using System;
using System.Runtime.InteropServices;

public static class ShortcutAUMID
{
    // -----------------------------------------------------------------------
    // Native declarations
    // -----------------------------------------------------------------------

    [DllImport("shell32.dll", CharSet = CharSet.Unicode, PreserveSig = true)]
    private static extern int SHGetPropertyStoreFromParsingName(
        string              pszPath,
        IntPtr              pbc,
        int                 flags,       // GETPROPERTYSTOREFLAGS
        [In] ref Guid       riid,
        out IntPtr          ppv);

    [DllImport("ole32.dll", PreserveSig = true)]
    private static extern int PropVariantClear(ref PROPVARIANT pvar);

    // -----------------------------------------------------------------------
    // IPropertyStore COM interface
    // -----------------------------------------------------------------------

    [ComImport]
    [Guid("886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99")]
    [InterfaceType(ComInterfaceType.InterfaceIsIUnknown)]
    private interface IPropertyStore
    {
        [PreserveSig] int GetCount(out uint cProps);
        [PreserveSig] int GetAt(uint iProp, out PROPERTYKEY pkey);
        [PreserveSig] int GetValue(ref PROPERTYKEY key, out PROPVARIANT pv);
        [PreserveSig] int SetValue(ref PROPERTYKEY key, ref PROPVARIANT pv);
        [PreserveSig] int Commit();
    }

    // -----------------------------------------------------------------------
    // PROPERTYKEY: a GUID + uint pid pair
    // -----------------------------------------------------------------------

    [StructLayout(LayoutKind.Sequential, Pack = 4)]
    private struct PROPERTYKEY
    {
        public Guid  fmtid;
        public uint  pid;
    }

    // -----------------------------------------------------------------------
    // PROPVARIANT: we only need VT_LPWSTR (vt=31) support
    // -----------------------------------------------------------------------

    [StructLayout(LayoutKind.Explicit, Size = 16)]
    private struct PROPVARIANT
    {
        [FieldOffset(0)] public ushort  vt;
        [FieldOffset(2)] public ushort  wReserved1;
        [FieldOffset(4)] public ushort  wReserved2;
        [FieldOffset(6)] public ushort  wReserved3;
        [FieldOffset(8)] public IntPtr  pszVal;   // used when vt == VT_LPWSTR
    }

    private const ushort VT_LPWSTR         = 31;
    private const int    GPS_READWRITE      = 0x00000002;

    private static readonly Guid IID_IPropertyStore =
        new Guid("886D8EEB-8CF2-4446-8D02-CDBA1DBDCF99");

    // PKEY_AppUserModel_ID = {9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3}, 5
    private static readonly Guid PKEY_AUMID_fmtid =
        new Guid("9F4C2855-9F79-4B39-A8D0-E1D42DE1D5F3");
    private const uint PKEY_AUMID_pid = 5;

    // -----------------------------------------------------------------------
    // Public entry point
    // -----------------------------------------------------------------------

    public static void Set(string shortcutPath, string aumid)
    {
        Guid iid = IID_IPropertyStore;
        IntPtr ppv;
        int hr = SHGetPropertyStoreFromParsingName(
            shortcutPath, IntPtr.Zero, GPS_READWRITE, ref iid, out ppv);

        if (hr < 0)
            throw new Exception(
                string.Format("SHGetPropertyStoreFromParsingName failed: 0x{0:X8}", (uint)hr));

        IPropertyStore store =
            (IPropertyStore)Marshal.GetObjectForIUnknown(ppv);
        Marshal.Release(ppv);

        PROPERTYKEY key;
        key.fmtid = PKEY_AUMID_fmtid;
        key.pid   = PKEY_AUMID_pid;

        PROPVARIANT pv = new PROPVARIANT();
        pv.vt     = VT_LPWSTR;
        pv.pszVal = Marshal.StringToCoTaskMemUni(aumid);

        try
        {
            hr = store.SetValue(ref key, ref pv);
            if (hr < 0)
                throw new Exception(
                    string.Format("IPropertyStore::SetValue failed: 0x{0:X8}", (uint)hr));

            hr = store.Commit();
            if (hr < 0)
                throw new Exception(
                    string.Format("IPropertyStore::Commit failed: 0x{0:X8}", (uint)hr));
        }
        finally
        {
            // Free the CoTaskMem string we allocated; then zero the pointer
            // so PropVariantClear does not attempt a second free.
            if (pv.pszVal != IntPtr.Zero)
            {
                Marshal.FreeCoTaskMem(pv.pszVal);
                pv.pszVal = IntPtr.Zero;
                pv.vt     = 0;
            }
            Marshal.ReleaseComObject(store);
        }
    }
}
'@

# Compile the helper class once per session
if (-not ([System.Management.Automation.PSTypeName]'ShortcutAUMID').Type) {
    Add-Type -TypeDefinition $code -Language CSharp
}

# ---------------------------------------------------------------------------
# Invoke
# ---------------------------------------------------------------------------
try {
    [ShortcutAUMID]::Set($ShortcutPath, $AppUserModelID)
    Write-Host "OK — AUMID '$AppUserModelID' stamped on '$ShortcutPath'" -ForegroundColor Green
}
catch {
    Write-Error "Failed to set AUMID: $_"
    exit 1
}

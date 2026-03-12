<#
.SYNOPSIS
    Builds one or all AppWrapper profile(s) into self-contained executables.

.DESCRIPTION
    For each JSON profile in profiles/ (or the single profile supplied via
    -Profile) the script:
      1. Reads the profile JSON.
      2. Generates generated_config.h in the source directory.
      3. Generates app.rc (embeds the icon at compile time).
      4. Runs CMake configure + build (Release).
      5. Copies the resulting exe and app.ico to out\<Name>\.
      6. Cleans up the generated files.

.PARAMETER ProfilePath
    Optional path to a single .json profile to build.  When omitted every
    *.json file found in the profiles\ directory is built.

.EXAMPLE
    .\build.ps1
    .\build.ps1 -ProfilePath profiles\myapp.json
#>
param(
    [string]$ProfilePath = ""
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
Set-Location $ScriptDir

# ---------------------------------------------------------------------------
# Locate CMake
# ---------------------------------------------------------------------------
function Find-CMake {
    # 1. Already on PATH?
    if (Get-Command cmake -ErrorAction SilentlyContinue) {
        return "cmake"
    }

    # 2. Look via vswhere
    $vswhere = Join-Path ${env:ProgramFiles(x86)} `
        "Microsoft Visual Studio\Installer\vswhere.exe"

    if (Test-Path $vswhere) {
        $vsPath = & $vswhere -latest -products * `
            -requires Microsoft.VisualStudio.Component.VC.CMake.Project `
            -property installationPath 2>$null

        if ($vsPath) {
            $candidate = Join-Path $vsPath `
                "Common7\IDE\CommonExtensions\Microsoft\CMake\CMake\bin\cmake.exe"
            if (Test-Path $candidate) {
                return $candidate
            }
        }
    }

    throw "CMake not found. Install Visual Studio with the CMake component or add cmake.exe to PATH."
}

$cmake = Find-CMake
Write-Host "Using CMake: $cmake" -ForegroundColor DarkGray

# ---------------------------------------------------------------------------
# Escape a string for use inside a C++ wide-string literal
# ---------------------------------------------------------------------------
function ConvertTo-CppString([string]$s) {
    # Backslash -> double-backslash, then double-quote -> \"
    $s = $s -replace '\\', '\\'
    $s = $s -replace '"',  '\"'
    return $s
}

# ---------------------------------------------------------------------------
# Derive a PascalCase exe name from a filename stem
# (e.g. "my_cool_app" -> "MyCoolApp", "notepad" -> "Notepad")
# ---------------------------------------------------------------------------
function ConvertTo-ExeName([string]$stem) {
    # Use the stem as-is; strip only characters invalid in CMake target names.
    $name = $stem -replace '[^a-zA-Z0-9_\-]', ''
    if ($name.Length -eq 0) { throw "Cannot derive a valid exe name from '$stem'" }
    return $name
}

# ---------------------------------------------------------------------------
# Collect profiles to build
# ---------------------------------------------------------------------------
if ($ProfilePath) {
    $profileFiles = @(Get-Item $ProfilePath)
} else {
    $profilesDir  = Join-Path $ScriptDir "profiles"
    if (-not (Test-Path $profilesDir)) {
        Write-Error "profiles\ directory not found at $profilesDir"
        exit 1
    }
    $profileFiles = @(Get-ChildItem -Path $profilesDir -Filter "*.json")
}

if ($profileFiles.Count -eq 0) {
    Write-Error "No .json profiles found."
    exit 1
}

# ---------------------------------------------------------------------------
# Build each profile
# ---------------------------------------------------------------------------
$built   = @()
$failed  = @()

foreach ($profileFile in $profileFiles) {
    $profilePath = $profileFile.FullName
    Write-Host "`n=== Building profile: $($profileFile.Name) ===" -ForegroundColor Cyan

    try {
        # ------------------------------------------------------------------
        # 1. Read JSON
        # ------------------------------------------------------------------
        $json = Get-Content $profilePath -Raw | ConvertFrom-Json

        # Mandatory fields
        $targetExe = $json.targetExe
        $aumid     = $json.appUserModelId
        $iconPath  = $json.iconPath

        if (-not $targetExe) { throw "Missing required field: targetExe" }
        if (-not $aumid)     { throw "Missing required field: appUserModelId" }
        if (-not $iconPath)  { throw "Missing required field: iconPath" }

        # Resolve icon to absolute path
        if (-not [System.IO.Path]::IsPathRooted($iconPath)) {
            $iconPath = Join-Path (Split-Path $profilePath) $iconPath
        }
        $iconPath = (Resolve-Path $iconPath -ErrorAction Stop).Path

        # Optional fields
        $arguments        = if ($json.PSObject.Properties['arguments'])        { $json.arguments }        else { "" }
        $workingDirectory = if ($json.PSObject.Properties['workingDirectory']) { $json.workingDirectory } else { "" }
        $monitorLoopVal   = if ($json.PSObject.Properties['monitorLoop'] -and ($null -ne $json.monitorLoop)) {
                                if ($json.monitorLoop) { 'true' } else { 'false' }
                            } else { 'true' }

        # ------------------------------------------------------------------
        # 2. Derive output name
        # ------------------------------------------------------------------
        $stem = [System.IO.Path]::GetFileNameWithoutExtension($profileFile.Name)
        $name = ConvertTo-ExeName $stem

        Write-Host "  Output name : $name"
        Write-Host "  Target exe  : $targetExe"
        Write-Host "  AUMID       : $aumid"
        Write-Host "  Icon        : $iconPath"

        # ------------------------------------------------------------------
        # 3. Generate generated_config.h
        # ------------------------------------------------------------------
        $targetExeC   = ConvertTo-CppString $targetExe
        $argumentsC   = ConvertTo-CppString $arguments
        $workDirC     = ConvertTo-CppString $workingDirectory

        # Build the env-var array entries
        $envLines = [System.Collections.Generic.List[string]]::new()
        $envCount = 0

        if ($json.PSObject.Properties['environmentVariables'] -and $json.environmentVariables) {
            foreach ($ev in $json.environmentVariables) {
                $evName  = ConvertTo-CppString $ev.name
                $evValue = ConvertTo-CppString $ev.value
                $evMode  = if ($ev.PSObject.Properties['mode'] -and $ev.mode)  { $ev.mode }      else { "set" }
                $evSep   = if ($ev.PSObject.Properties['separator'] -and $ev.separator) { ConvertTo-CppString $ev.separator } else { ";" }
                $envLines.Add("    { L`"$evName`", L`"$evValue`", L`"$evMode`", L`"$evSep`" },")
                $envCount++
            }
        }

        # Always emit at least one dummy entry so the array is never zero-length.
        # The count drives the runtime loop; the dummy is never executed.
        if ($envCount -eq 0) {
            $envLines.Add("    { L`"`", L`"`", L`"set`", L`";`" },  // dummy — CONFIG_ENV_VAR_COUNT is 0")
        }

        $envBody = $envLines -join "`n"

        $configHeader = @"
// Auto-generated by build.ps1 — do not edit by hand.
#pragma once
#include <windows.h>
#define IDI_APPICON 101

static const wchar_t* CONFIG_TARGET_EXE  = L"$targetExeC";
static const wchar_t* CONFIG_ARGUMENTS   = L"$argumentsC";
static const wchar_t* CONFIG_WORKING_DIR = L"$workDirC";
static const wchar_t* CONFIG_AUMID       = L"$aumid";
static const bool     CONFIG_MONITOR_LOOP = $monitorLoopVal;

struct EnvVarEntry {
    const wchar_t* name;
    const wchar_t* value;
    const wchar_t* mode;
    const wchar_t* separator;
};

static const EnvVarEntry CONFIG_ENV_VARS[] = {
$envBody
};
static const int CONFIG_ENV_VAR_COUNT = $envCount;
"@

        $configHeaderPath = Join-Path $ScriptDir "generated_config.h"
        Set-Content -Path $configHeaderPath -Value $configHeader -Encoding UTF8
        Write-Host "  Generated: generated_config.h"

        # ------------------------------------------------------------------
        # 4. Generate app.rc with absolute icon path
        #    Use forward slashes to avoid RC-compiler escaping issues.
        # ------------------------------------------------------------------
        $iconForRc = $iconPath -replace '\\', '/'
        $rcContent = @"
#include <windows.h>
#define IDI_APPICON 101
IDI_APPICON ICON "$iconForRc"
"@

        $rcPath = Join-Path $ScriptDir "app.rc"
        Set-Content -Path $rcPath -Value $rcContent -Encoding UTF8
        Write-Host "  Generated: app.rc"

        # ------------------------------------------------------------------
        # 5. CMake configure + build
        # ------------------------------------------------------------------
        $buildDir = Join-Path $ScriptDir "build\$name"

        Write-Host "  CMake configure..." -ForegroundColor DarkGray
        & $cmake -S $ScriptDir -B $buildDir `
            -G "Visual Studio 17 2022" -A x64 `
            "-DAPP_NAME=$name" `
            "-DICON_RC_FILE=app.rc"

        if ($LASTEXITCODE -ne 0) { throw "CMake configure failed (exit $LASTEXITCODE)" }

        Write-Host "  CMake build (Release)..." -ForegroundColor DarkGray
        & $cmake --build $buildDir --config Release

        if ($LASTEXITCODE -ne 0) { throw "CMake build failed (exit $LASTEXITCODE)" }

        # ------------------------------------------------------------------
        # 6. Copy outputs to out\<Name>\
        # ------------------------------------------------------------------
        $outDir    = Join-Path $ScriptDir "out\$name"
        $builtExe  = Join-Path $buildDir  "Release\$name.exe"

        if (-not (Test-Path $builtExe)) {
            throw "Built exe not found at: $builtExe"
        }

        New-Item -ItemType Directory -Force -Path $outDir | Out-Null
        Copy-Item $builtExe  -Destination (Join-Path $outDir "$name.exe") -Force

        Write-Host "  Output      : $outDir\$name.exe" -ForegroundColor Green

        $built += $name
    }
    catch {
        Write-Warning "FAILED to build $($profileFile.Name): $_"
        $failed += $profileFile.Name
    }
    finally {
        # ------------------------------------------------------------------
        # 7. Clean up app.rc (generated_config.h is kept as the IntelliSense
        #    stub; build.ps1 overwrites it on every run anyway)
        # ------------------------------------------------------------------
        $rcFile = Join-Path $ScriptDir "app.rc"
        if (Test-Path $rcFile) { Remove-Item $rcFile -Force -ErrorAction SilentlyContinue }
    }
}

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
Write-Host ""
if ($built.Count -gt 0) {
    Write-Host "Built successfully: $($built -join ', ')" -ForegroundColor Green
}
if ($failed.Count -gt 0) {
    Write-Host "Failed: $($failed -join ', ')" -ForegroundColor Red
    exit 1
}

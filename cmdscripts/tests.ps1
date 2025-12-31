# SPDX-License-Identifier: Apache-2.0
Param(
  [ValidateSet("smoke","deliberate","all")]
  [string]$Suite = "smoke",
  [Alias("Subskill")]
  [string]$Module = "",
  [string]$Root = "",
  [int]$Keep = 0,
  [switch]$Clean,
  [switch]$AllowWeb,
  [switch]$ForceBash,
  [switch]$ForceWindows
)

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$RepoRoot = Resolve-Path (Join-Path $ScriptDir "..")
$TestsYml = Join-Path $RepoRoot "tests/tests.yml"
$TestsSh = Join-Path $RepoRoot "cmdscripts/tests.sh"
$ConfigYml = Join-Path $RepoRoot "scripts/config.yml"

if (-not (Test-Path $TestsYml)) {
  Write-Error "Missing tests.yml at $TestsYml"
  exit 2
}

function Get-TestValue([string]$Pattern) {
  $line = Select-String -Path $TestsYml -Pattern "^\s*${Pattern}:" | Select-Object -First 1
  if (-not $line) { return $null }
  $value = $line.Line -replace "^\s*${Pattern}:\s*", ""
  $value = $value.Trim().Trim('"')
  return $value
}

function Get-ConfigValue([string]$Pattern) {
  if (-not (Test-Path $ConfigYml)) { return $null }
  $line = Select-String -Path $ConfigYml -Pattern "^\s*${Pattern}:" | Select-Object -First 1
  if (-not $line) { return $null }
  $value = $line.Line -replace "^\s*${Pattern}:\s*", ""
  $value = $value.Trim().Trim('"')
  return $value
}

function Get-GitBash {
  $candidates = @(
    (Join-Path $env:ProgramFiles "Git\bin\bash.exe"),
    (Join-Path $env:ProgramFiles "Git\usr\bin\bash.exe"),
    (Join-Path $env:ProgramFiles "Git\mingw64\bin\bash.exe"),
    (Join-Path $env:ProgramFiles "Git\mingw64\usr\bin\bash.exe"),
    (Join-Path ${env:ProgramFiles(x86)} "Git\bin\bash.exe"),
    (Join-Path ${env:ProgramFiles(x86)} "Git\usr\bin\bash.exe"),
    (Join-Path ${env:ProgramFiles(x86)} "Git\mingw64\bin\bash.exe"),
    (Join-Path ${env:ProgramFiles(x86)} "Git\mingw64\usr\bin\bash.exe")
  ) | Where-Object { $_ -and (Test-Path $_) }
  if ($candidates.Count -gt 0) {
    return $candidates[0]
  }

  $cmd = Get-Command bash -ErrorAction SilentlyContinue
  if ($cmd) {
    $path = $cmd.Source
    if ($path -match "\\Windows\\System32\\bash\.exe$") { return $null }
    if ($path -match "\\Windows\\System32\\wsl\.exe$") { return $null }
    return $path
  }

  return $null
}

function Get-RscriptPath {
  $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
  if ($cmd) {
    return $cmd.Source
  }
  return $null
}

function Get-PythonPath {
  $envCandidates = @($env:PYTHON_BIN, $env:PYTHON) | Where-Object { $_ -and (Test-Path $_) }
  if ($envCandidates.Count -gt 0) {
    return $envCandidates[0]
  }

  $cmd = Get-Command python -ErrorAction SilentlyContinue
  if ($cmd -and $cmd.Source -and ($cmd.Source -notmatch "\\\\WindowsApps\\\\")) {
    return $cmd.Source
  }

  $cmd3 = Get-Command python3 -ErrorAction SilentlyContinue
  if ($cmd3 -and $cmd3.Source -and ($cmd3.Source -notmatch "\\\\WindowsApps\\\\")) {
    return $cmd3.Source
  }

  $candidates = @()
  $candidates += Get-ChildItem "$env:LocalAppData\\Programs\\Python\\Python*\\python.exe" -ErrorAction SilentlyContinue
  $candidates += Get-ChildItem "$env:ProgramFiles\\Python*\\python.exe" -ErrorAction SilentlyContinue
  $candidates += Get-ChildItem "$env:ProgramFiles\\Python\\Python*\\python.exe" -ErrorAction SilentlyContinue
  $candidates += Get-ChildItem "${env:ProgramFiles(x86)}\\Python*\\python.exe" -ErrorAction SilentlyContinue

  if ($candidates.Count -gt 0) {
    return ($candidates | Sort-Object FullName | Select-Object -Last 1).FullName
  }

  return $null
}

function Remove-TestRuns([string]$Root, [int]$KeepCount) {
  if ($KeepCount -le 0) { return }
  if (-not (Test-Path $Root)) { return }
  $dirs = Get-ChildItem -Path $Root -Directory | Sort-Object Name
  if ($dirs.Count -le $KeepCount) { return }
  $removeCount = $dirs.Count - $KeepCount
  $dirs | Select-Object -First $removeCount | ForEach-Object {
    Remove-Item -Recurse -Force $_.FullName
  }
}

$outputDir = Get-TestValue "output_dir"
$dataDir = Get-TestValue "data_dir"
$goldenDataset = Get-TestValue "golden_dataset"
$templateDir = Get-TestValue "template_dir"
$templateMarker = Get-TestValue "template_marker"
$keepDefault = Get-TestValue "keep_runs_default"

if (-not $outputDir -or -not $dataDir -or -not $goldenDataset -or -not $templateDir) {
  Write-Error "Invalid tests.yml content."
  exit 2
}

$outputDir = Join-Path $RepoRoot $outputDir
$dataSetPath = Join-Path $RepoRoot $goldenDataset
$templatePath = Join-Path $RepoRoot $templateDir

if (-not (Test-Path $dataSetPath)) {
  Write-Error "Golden dataset not found: $dataSetPath"
  exit 2
}

if (-not $Keep -or $Keep -le 0) {
  if ($env:NLSS_KEEP_RUNS) {
    [int]$Keep = $env:NLSS_KEEP_RUNS
  } elseif ($keepDefault) {
    [int]$Keep = $keepDefault
  } else {
    $Keep = 0
  }
}

$rscript = Get-RscriptPath
if (-not $rscript) {
  Write-Error "Rscript not found. Install R and ensure Rscript is on PATH."
  exit 127
}

$rscriptDir = Split-Path -Parent $rscript
if ($env:PATH -notmatch [regex]::Escape($rscriptDir)) {
  $env:PATH = "$rscriptDir;$env:PATH"
}

$pythonExe = Get-PythonPath
if ($pythonExe) {
  $pythonDir = Split-Path -Parent $pythonExe
  if ($env:PATH -notmatch [regex]::Escape($pythonDir)) {
    $env:PATH = "$pythonDir;$env:PATH"
  }
  if (-not $env:PYTHON_BIN) {
    $env:PYTHON_BIN = "python"
  }
} else {
  Write-Warning "Python not found on PATH. Set PYTHON_BIN or install Python to run test helpers."
}

if ($ForceWindows.IsPresent) {
  if ($Module) {
    Write-Error "Module runs are not supported with -ForceWindows. Use Git Bash without -ForceWindows."
    exit 2
  }
  if ($Suite -ne "smoke") {
    Write-Error "ForceWindows only supports the smoke suite. Use Git Bash for deliberate/all."
    exit 2
  }
  if (-not $Root) {
    $timestamp = Get-Date -Format "yyyyMMddHHmmss"
    $Root = Join-Path $outputDir $timestamp
  }

  if ($Clean.IsPresent) {
    Remove-TestRuns -Root $outputDir -KeepCount $Keep
    Write-Host "Cleaned old runs in $outputDir (keep=$Keep)."
    exit 0
  }

  New-Item -ItemType Directory -Path $Root -Force | Out-Null
  $tmpDir = Join-Path $Root "tmp"
  New-Item -ItemType Directory -Path $tmpDir -Force | Out-Null

  $env:NLSS_TEST_ROOT = $Root
  $env:NLSS_TEST_REPO_ROOT = $RepoRoot
  $env:NLSS_TEST_DATASET = $dataSetPath
  $env:NLSS_TEST_TEMPLATE_DIR = $templatePath
  $env:NLSS_TEST_TEMPLATE_MARKER = $templateMarker
  $env:TEMP = $tmpDir
  $env:TMP = $tmpDir

  $datasetName = [System.IO.Path]::GetFileNameWithoutExtension($dataSetPath)
  $workspaceRoot = Join-Path $Root "workspace"
  $manifestName = Get-ConfigValue "workspace_manifest"
  if (-not $manifestName) { $manifestName = "nlss-workspace.yml" }
  New-Item -ItemType Directory -Path $workspaceRoot -Force | Out-Null
  New-Item -ItemType File -Path (Join-Path $workspaceRoot $manifestName) -Force | Out-Null

  $datasetDir = Join-Path $workspaceRoot $datasetName
  $plotsDir = Join-Path $datasetDir "plots"

  Set-Location $workspaceRoot

  $failures = 0

  function Write-Fail([string]$Message) {
    Write-Error $Message
    $script:failures++
  }

  function Invoke-TestCommand([string]$Label, [string]$Exe, [string[]]$CommandArgs) {
    Write-Host "==> $Label"
    & $Exe @CommandArgs
    if ($LASTEXITCODE -ne 0) {
      Write-Fail "FAIL: $Label"
    } else {
      Write-Host "PASS: $Label"
    }
  }

  function Assert-File([string]$Path) {
    if (Test-Path $Path) {
      Write-Host "PASS: file exists: $Path"
    } else {
      Write-Fail "missing file: $Path"
    }
  }

  function Assert-Pattern([string]$Pattern, [string]$Path) {
    if (Select-String -Path $Path -Pattern $Pattern -Quiet -SimpleMatch) {
      Write-Host "PASS: found '$Pattern' in $Path"
    } else {
      Write-Fail "missing pattern '$Pattern' in $Path"
    }
  }

  function Assert-Glob([string]$Pattern) {
    $foundFiles = Get-ChildItem -Path $Pattern -ErrorAction SilentlyContinue
    if ($foundFiles) {
      Write-Host "PASS: found files for pattern: $Pattern"
    } else {
      Write-Fail "no files for pattern: $Pattern"
    }
  }

  Invoke-TestCommand "init_workspace" $rscript @(
    (Join-Path $RepoRoot "scripts/R/init_workspace.R"),
    "--csv", $dataSetPath
  )
  Assert-File (Join-Path $datasetDir "report_canonical.md")
  Assert-File (Join-Path $datasetDir "analysis_log.jsonl")

  Invoke-TestCommand "descriptive_stats" $rscript @(
    (Join-Path $RepoRoot "scripts/R/descriptive_stats.R"),
    "--csv", $dataSetPath,
    "--vars", "age,score",
    "--group", "condition"
  )
  Assert-Pattern "# Descriptive Statistics" (Join-Path $datasetDir "report_canonical.md")

  Invoke-TestCommand "plot_histogram" $rscript @(
    (Join-Path $RepoRoot "scripts/R/plot.R"),
    "--csv", $dataSetPath,
    "--type", "histogram",
    "--vars", "age",
    "--title", "Age distribution"
  )
  Assert-Glob (Join-Path $plotsDir "figure-001-*.png")
  Assert-Pattern "Figure 1" (Join-Path $datasetDir "report_canonical.md")

  Invoke-TestCommand "plot_bar_percent" $rscript @(
    (Join-Path $RepoRoot "scripts/R/plot.R"),
    "--csv", $dataSetPath,
    "--type", "bar",
    "--vars", "gender",
    "--group", "condition",
    "--stat", "percent",
    "--percent-base", "group",
    "--na-action", "keep"
  )
  Assert-Glob (Join-Path $plotsDir "figure-002-*.png")
  Assert-Pattern "Figure 2" (Join-Path $datasetDir "report_canonical.md")

  Remove-TestRuns -Root $outputDir -KeepCount $Keep

  if ($failures -gt 0) {
    Write-Error "Smoke tests completed with $failures failure(s)."
    exit 1
  }

  Write-Host "Smoke tests completed successfully."
  exit 0
}

$bash = Get-GitBash
if (-not $bash) {
  Write-Error "Git Bash not found. Install Git for Windows or use -ForceWindows for smoke-only."
  exit 2
}

if (-not (Test-Path $TestsSh)) {
  Write-Error "Missing test harness: $TestsSh"
  exit 2
}

if ($AllowWeb.IsPresent) {
  $env:NLSS_TEST_ALLOW_WEB = "1"
}

$argsList = @("--suite", $Suite)
if ($Module) {
  $argsList = @("--module", $Module)
}
if ($Root) { $argsList += @("--root", $Root) }
if ($Keep -gt 0) { $argsList += @("--keep", $Keep) }
if ($Clean.IsPresent) { $argsList += "--clean" }

& $bash $TestsSh @argsList
exit $LASTEXITCODE

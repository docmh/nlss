param(
    [string]$RunRoot = ""
)

$ErrorActionPreference = "Stop"

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$rootDir = Resolve-Path (Join-Path $scriptDir "..\..") | Select-Object -ExpandProperty Path
$configPath = Join-Path $rootDir "core-stats\scripts\config.yml"
$runRscriptPs = Join-Path $rootDir "core-stats\scripts\run_rscript.ps1"
$dataPath = Join-Path $rootDir "outputs\tests\golden_dataset.csv"
$rScriptDir = Join-Path $rootDir "core-stats\scripts\R"
$checkRPackageScript = Join-Path $rootDir "outputs\tests\check_r_package.R"
$mixedModelsPrepScript = Join-Path $rootDir "outputs\tests\mixed_models_prep.R"

function Write-Log {
    param([string]$Message)
    $Message | Out-File -FilePath $script:logPath -Append
    Write-Host $Message
}

function Get-ConfigValue {
    param([string]$Key)
    if (-not (Get-Command python -ErrorAction SilentlyContinue)) {
        throw "python is required to read config.yml."
    }
    $script = @'
import sys
path, key = sys.argv[1], sys.argv[2]
parts = key.split(".")
lines = open(path, "r", encoding="utf-8").read().splitlines()
stack = []
for line in lines:
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        continue
    indent = len(line) - len(line.lstrip(" "))
    key_name, _, rest = stripped.partition(":")
    while stack and stack[-1][1] >= indent:
        stack.pop()
    stack.append((key_name, indent))
    if [k for k, _ in stack] == parts:
        value = rest.strip()
        if value.startswith(("'", '"')) and value.endswith(("'", '"')) and len(value) >= 2:
            value = value[1:-1]
        print(value)
        sys.exit(0)
sys.exit(0)
'@
    $value = $script | python - $configPath $Key
    if ($LASTEXITCODE -ne 0) {
        return ""
    }
    return ($value | Select-Object -First 1)
}

function Set-ConfigValue {
    param([string]$Key, [string]$Value)
    if (-not (Get-Command python -ErrorAction SilentlyContinue)) {
        throw "python is required to update config.yml."
    }
    $script = @'
import sys
path, key, value = sys.argv[1], sys.argv[2], sys.argv[3]
parts = key.split(".")
lines = open(path, "r", encoding="utf-8").read().splitlines()
stack = []
for idx, line in enumerate(lines):
    stripped = line.strip()
    if not stripped or stripped.startswith("#"):
        continue
    indent = len(line) - len(line.lstrip(" "))
    key_name, _, _ = stripped.partition(":")
    while stack and stack[-1][1] >= indent:
        stack.pop()
    stack.append((key_name, indent))
    if [k for k, _ in stack] == parts:
        lines[idx] = (" " * indent) + f'{key_name}: "{value}"'
        break
else:
    sys.exit(1)
with open(path, "w", encoding="utf-8", newline="") as handle:
    handle.write("\n".join(lines) + "\n")
'@
    $script | python - $configPath $Key $Value | Out-Null
    if ($LASTEXITCODE -ne 0) {
        throw "Failed to set config value for $Key."
    }
}

function Normalize-ConfigPath {
    param([string]$Path)
    return ($Path -replace "\\", "/")
}

function Resolve-RootPath {
    param([string]$Path)
    if ([System.IO.Path]::IsPathRooted($Path)) {
        return $Path
    }
    return (Join-Path $rootDir $Path)
}

function Resolve-RunPath {
    param([string]$Path)
    if ([System.IO.Path]::IsPathRooted($Path)) {
        return $Path
    }
    return (Join-Path $script:runRoot $Path)
}

$runsBaseCfg = Get-ConfigValue "tests.output_dir"
if (-not $runsBaseCfg) {
    $runsBaseCfg = "outputs/test-runs"
}
$templateOverrideCfg = Get-ConfigValue "tests.template_dir"
if (-not $templateOverrideCfg) {
    $templateOverrideCfg = "templates"
}
$templateMarker = Get-ConfigValue "tests.template_marker"
if (-not $templateMarker) {
    $templateMarker = "TEMPLATE_SMOKE_TEST"
}

$runId = Get-Date -Format "yyyyMMddHHmmss"
$runsBase = Resolve-RootPath $runsBaseCfg
if (-not $RunRoot) {
    if ($env:CORE_STATS_TEST_ROOT) {
        $RunRoot = $env:CORE_STATS_TEST_ROOT
    } else {
        $RunRoot = Join-Path $runsBase $runId
    }
}
$script:runRoot = $RunRoot
$workspaceDir = Join-Path $RunRoot "workspace"
$templateOverrideDir = Resolve-RunPath $templateOverrideCfg
$tmpBase = Join-Path $RunRoot "tmp"
$logPath = Join-Path $RunRoot "smoke_test.log"

$datasetLabel = [System.IO.Path]::GetFileNameWithoutExtension($dataPath)
$datasetDir = Join-Path $workspaceDir $datasetLabel
$parquetPath = Join-Path $datasetDir "$datasetLabel.parquet"
$apaReportPath = Join-Path $datasetDir "apa_report.md"
$scratchpadPath = Join-Path $datasetDir "scratchpad.md"
$analysisLogPath = Join-Path $datasetDir "analysis_log.jsonl"

$mixedLabel = "mixed_models_long"
$mixedDataPath = Join-Path $tmpBase "$mixedLabel.csv"
$mixedDatasetDir = Join-Path $workspaceDir $mixedLabel
$mixedParquetPath = Join-Path $mixedDatasetDir "$mixedLabel.parquet"
$mixedApaReportPath = Join-Path $mixedDatasetDir "apa_report.md"
$mixedAnalysisLogPath = Join-Path $mixedDatasetDir "analysis_log.jsonl"

New-Item -ItemType Directory -Force -Path $RunRoot, $workspaceDir, $datasetDir, $templateOverrideDir, $tmpBase | Out-Null

[System.Environment]::SetEnvironmentVariable("TMPDIR", $tmpBase, "Process")
[System.Environment]::SetEnvironmentVariable("TMP", $tmpBase, "Process")
[System.Environment]::SetEnvironmentVariable("TEMP", $tmpBase, "Process")

$configBak = Join-Path $tmpBase "config-backup.yml"
$configBase = Join-Path $tmpBase "config-base.yml"
Copy-Item $configPath $configBak -Force

function Reset-ToBase {
    Copy-Item $configBase $configPath -Force
}

try {
    $configOut = Normalize-ConfigPath $workspaceDir
    Set-ConfigValue "defaults.output_dir" $configOut
    Copy-Item $configPath $configBase -Force

    "" | Out-File -FilePath $logPath

    if (-not (Test-Path $dataPath)) {
        Write-Log "[FAIL] missing dataset: $dataPath"
        exit 1
    }

    Set-Location $rootDir

    if (Test-Path $apaReportPath) { Remove-Item $apaReportPath -Force }
    if (Test-Path $scratchpadPath) { Remove-Item $scratchpadPath -Force }
    if (Test-Path $analysisLogPath) { Remove-Item $analysisLogPath -Force }

    function Invoke-RScript {
        param([string]$ScriptPath, [string[]]$ScriptArgs)
        $prevErrorPreference = $ErrorActionPreference
        $ErrorActionPreference = "Continue"
        $output = & $runRscriptPs $ScriptPath @ScriptArgs 2>&1
        $exitCode = $LASTEXITCODE
        $ErrorActionPreference = $prevErrorPreference
        return [PSCustomObject]@{
            Output = ($output | Out-String)
            ExitCode = $exitCode
        }
    }

    function Run-OkR {
        param([string]$Label, [string]$ScriptPath, [string[]]$ScriptArgs)
        Write-Log "[RUN] $Label"
        $result = Invoke-RScript $ScriptPath $ScriptArgs
        if ($result.Output) {
            $result.Output | Out-File -FilePath $logPath -Append
        }
        if ($result.ExitCode -ne 0) {
            Write-Log "[FAIL] $Label (exit $($result.ExitCode))"
            exit 1
        }
        Write-Log "[PASS] $Label"
    }

    function Run-ExpectFailR {
        param([string]$Label, [string]$ScriptPath, [string[]]$ScriptArgs)
        Write-Log "[RUN-FAIL] $Label"
        $result = Invoke-RScript $ScriptPath $ScriptArgs
        if ($result.Output) {
            $result.Output | Out-File -FilePath $logPath -Append
        }
        if ($result.ExitCode -eq 0) {
            Write-Log "[FAIL] $Label (expected failure)"
            exit 1
        }
        Write-Log "[PASS] $Label (failed as expected)"
    }

    function Get-LogCount {
        param([string]$Path)
        if (-not (Test-Path $Path)) {
            return 0
        }
        return (Get-Content $Path | Where-Object { $_.Trim() -ne "" }).Count
    }

    function Log-HasExpected {
        param([string]$Path, [int]$StartCount, [string]$Module, [string]$Status)
        if (-not (Test-Path $Path)) {
            return $false
        }
        $index = 0
        foreach ($line in Get-Content $Path) {
            if (-not $line.Trim()) { continue }
            $index++
            if ($index -le $StartCount) { continue }
            try {
                $entry = $line | ConvertFrom-Json
            } catch {
                continue
            }
            if ($entry.module -ne $Module) { continue }
            if ($null -eq $entry.results) { continue }
            if ($entry.results.status -ne $Status) { continue }
            return $true
        }
        return $false
    }

    function Run-ExpectLogR {
        param([string]$Label, [string]$LogFile, [string]$Module, [string]$Status, [string]$ScriptPath, [string[]]$ScriptArgs)
        $fallbackLog = Join-Path $rootDir "core-stats-workspace\$datasetLabel\analysis_log.jsonl"
        Write-Log "[RUN-EXPECT] $Label"
        $startCount = Get-LogCount $LogFile
        $fallbackStart = $startCount
        if ($fallbackLog -ne $LogFile) {
            $fallbackStart = Get-LogCount $fallbackLog
        }
        $result = Invoke-RScript $ScriptPath $ScriptArgs
        if ($result.Output) {
            $result.Output | Out-File -FilePath $logPath -Append
        }
        if ($result.ExitCode -ne 0) {
            Write-Log "[PASS] $Label (failed as expected)"
            return
        }
        $attempts = 10
        for ($i = 0; $i -lt $attempts; $i++) {
            if (Log-HasExpected $LogFile $startCount $Module $Status) {
                Write-Log "[PASS] $Label (informational log)"
                return
            }
            if ((Test-Path $fallbackLog) -and (Log-HasExpected $fallbackLog $fallbackStart $Module $Status)) {
                Write-Log "[PASS] $Label (informational log fallback)"
                return
            }
            Start-Sleep -Milliseconds 300
        }
        Write-Log "[FAIL] $Label (expected failure or $Status log)"
        exit 1
    }

    function Resolve-LogPath {
        $outDir = Get-ConfigValue "defaults.output_dir"
        if (-not $outDir) {
            $outDir = $workspaceDir
        }
        $outDir = $outDir -replace "/", "\"
        if (-not [System.IO.Path]::IsPathRooted($outDir)) {
            $outDir = Join-Path $rootDir $outDir
        }
        return (Join-Path (Join-Path $outDir $datasetLabel) "analysis_log.jsonl")
    }

    function Assert-Marker {
        param([string]$Marker, [string]$File, [bool]$AllowMissing = $false)
        $attempts = 10
        for ($i = 0; $i -lt $attempts; $i++) {
            if (Test-Path $File) {
                if (Select-String -Path $File -SimpleMatch -Quiet $Marker) {
                    return
                }
            }
            Start-Sleep -Milliseconds 300
        }
        if ($AllowMissing) {
            if (-not (Test-Path $File)) {
                Write-Log "[WARN] missing output file (optional): $File"
            } else {
                Write-Log "[WARN] marker not found (optional): $Marker"
            }
            return
        }
        if (-not (Test-Path $File)) {
            Write-Log "[FAIL] missing output file: $File"
        } else {
            Write-Log "[FAIL] marker not found: $Marker"
        }
        exit 1
    }

    function Reset-Parquet {
        if (Test-Path $parquetPath) {
            Remove-Item $parquetPath -Force
        }
    }

    function Run-InitWorkspace {
        param([string]$Label)
        Reset-Parquet
        Run-OkR $Label (Join-Path $rScriptDir "init_workspace.R") @("--csv", $dataPath)
    }

    function Run-InitWorkspaceCustom {
        param([string]$Label, [string]$CustomPath)
        Run-OkR $Label (Join-Path $rScriptDir "init_workspace.R") @("--csv", $CustomPath)
    }

    function Has-RPackage {
        param([string]$Package)
        $result = Invoke-RScript $checkRPackageScript @($Package)
        return ($result.ExitCode -eq 0)
    }

    function Prepare-MixedModelsData {
        Run-OkR "mixed_models prep" $mixedModelsPrepScript @($dataPath, $mixedDataPath)
    }

    function Resolve-TemplateSource {
        param([string]$Key)
        $value = Get-ConfigValue "templates.$Key"
        if (-not $value) {
            throw "Missing templates.$Key in config."
        }
        if ([System.IO.Path]::IsPathRooted($value)) {
            return $value
        }
        $value = $value -replace "/", "\"
        return (Join-Path $rootDir ("core-stats\assets\" + $value))
    }

    function Template-TestBase {
        param([bool]$AllowMissing, [string]$Label, [string]$Key, [string]$OutputFile, [string]$ScriptPath, [string[]]$ScriptArgs)
        Reset-ToBase
        if ($Key -like "init_workspace.*") {
            Reset-Parquet
        }
        $source = Resolve-TemplateSource $Key
        if (-not (Test-Path $source)) {
            Write-Log "[FAIL] missing template: $source"
            exit 1
        }
        $marker = "$templateMarker`_$runId" + "_" + ($Key -replace "\.", "_")
        $target = Join-Path $templateOverrideDir ($Key -replace "\.", "_") + ".md"
        Copy-Item $source $target -Force
        Add-Content -Path $target -Value "`n`n$marker"
        Set-ConfigValue "templates.$Key" (Normalize-ConfigPath $target)
        Run-OkR $Label $ScriptPath $ScriptArgs
        Assert-Marker $marker $OutputFile $AllowMissing
        Reset-ToBase
    }

    function Template-Test {
        param([string]$Label, [string]$Key, [string]$OutputFile, [string]$ScriptPath, [string[]]$ScriptArgs)
        Template-TestBase $false $Label $Key $OutputFile $ScriptPath $ScriptArgs
    }

    function Template-TestOptional {
        param([string]$Label, [string]$Key, [string]$OutputFile, [string]$ScriptPath, [string[]]$ScriptArgs)
        Template-TestBase $true $Label $Key $OutputFile $ScriptPath $ScriptArgs
    }

    Run-InitWorkspace "init workspace"

    $hasLavaan = Has-RPackage "lavaan"

    Run-OkR "data_explorer clean" (Join-Path $rScriptDir "data_explorer.R") @("--parquet", $parquetPath, "--vars", "id,site,group3,gender,education,cat_var2,ordinal_var")
    Run-OkR "descriptive_stats clean" (Join-Path $rScriptDir "descriptive_stats.R") @("--parquet", $parquetPath, "--vars", "outcome_anova,x1,x2,x3,mediator", "--group", "group3", "--digits", "3")
    Run-OkR "frequencies clean" (Join-Path $rScriptDir "frequencies.R") @("--parquet", $parquetPath, "--vars", "gender,education,cat_var2,ordinal_var", "--group", "group3")
    Run-OkR "crosstabs clean" (Join-Path $rScriptDir "crosstabs.R") @("--parquet", $parquetPath, "--row", "gender", "--col", "group3", "--percent", "row", "--chisq", "TRUE", "--expected", "TRUE", "--residuals", "TRUE")
    Run-OkR "correlations clean" (Join-Path $rScriptDir "correlations.R") @("--parquet", $parquetPath, "--vars", "outcome_anova,x1,x2,x3,mediator", "--method", "pearson", "--missing", "pairwise")
    Run-OkR "scale clean" (Join-Path $rScriptDir "scale.R") @("--parquet", $parquetPath, "--vars", "f1_1,f1_2,f1_3_rev,f1_4", "--reverse", "f1_3_rev", "--reverse-min", "1", "--reverse-max", "5", "--score", "mean", "--omega", "TRUE")
    Run-OkR "assumptions clean" (Join-Path $rScriptDir "assumptions.R") @("--parquet", $parquetPath, "--analysis", "regression", "--dv", "outcome_anova", "--ivs", "x1,x2,x3,mediator")
    Run-OkR "regression clean" (Join-Path $rScriptDir "regression.R") @("--parquet", $parquetPath, "--dv", "outcome_reg", "--blocks", "x1,x2;x3,mediator", "--interactions", "x1:mediator", "--center", "mean", "--standardize", "predictors")
    if ($hasLavaan) {
        Run-OkR "sem clean cfa" (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "cfa", "--factors", "F1=f1_1,f1_2,f1_3_rev,f1_4;F2=f2_1,f2_2,f2_3,f2_4_rev;F3=f3_1,f3_2,f3_3,f3_4")
        Run-OkR "sem clean mediation" (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "mediation", "--x", "x1", "--m", "mediator", "--y", "outcome_reg", "--bootstrap", "FALSE")
        Run-ExpectLogR "sem missing vars" (Resolve-LogPath) "sem" "invalid_input" (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "cfa", "--factors", "F1=missing_item1,missing_item2")
    } else {
        Run-ExpectLogR "sem missing lavaan" (Resolve-LogPath) "sem" "missing_dependency" (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "cfa", "--factors", "F1=f1_1,f1_2")
    }
    Run-OkR "anova clean between" (Join-Path $rScriptDir "anova.R") @("--parquet", $parquetPath, "--dv", "outcome_anova", "--between", "group3")
    Run-OkR "t_test clean one-sample" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--vars", "x1", "--mu", "0")
    Run-OkR "t_test clean independent" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--vars", "outcome_anova", "--group", "group2")
    Run-OkR "t_test clean paired" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--x", "f1_1", "--y", "f1_2")

    Prepare-MixedModelsData
    Run-InitWorkspaceCustom "init workspace (mixed models)" $mixedDataPath

    $hasLme4 = Has-RPackage "lme4"
    $hasEmmeans = $false
    if ($hasLme4) {
        $hasEmmeans = Has-RPackage "emmeans"
    }

    if ($hasLme4) {
        Run-OkR "mixed_models clean" (Join-Path $rScriptDir "mixed_models.R") @("--parquet", $mixedParquetPath, "--dv", "score", "--fixed", "time,group3,x1", "--random", "id", "--reml", "TRUE", "--df-method", "satterthwaite")
        Run-OkR "mixed_models emmeans" (Join-Path $rScriptDir "mixed_models.R") @("--parquet", $mixedParquetPath, "--dv", "score", "--fixed", "time*group3,x1", "--random", "id", "--emmeans", "time*group3", "--contrasts", "pairwise", "--p-adjust", "holm")
        Run-ExpectLogR "mixed_models missing random" $mixedAnalysisLogPath "mixed_models" "invalid_input" (Join-Path $rScriptDir "mixed_models.R") @("--parquet", $mixedParquetPath, "--dv", "score", "--fixed", "time,group3")
    } else {
        Run-ExpectLogR "mixed_models missing lme4" $mixedAnalysisLogPath "mixed_models" "missing_dependency" (Join-Path $rScriptDir "mixed_models.R") @("--parquet", $mixedParquetPath, "--formula", "score ~ time + (1|id)")
    }

    Run-InitWorkspace "init workspace (edge)"
    Run-OkR "data_explorer edge" (Join-Path $rScriptDir "data_explorer.R") @("--parquet", $parquetPath, "--vars", "age,income,cat_var,high_missing_var,all_missing_var,zero_var,near_zero_var")
    Run-OkR "descriptive_stats edge" (Join-Path $rScriptDir "descriptive_stats.R") @("--parquet", $parquetPath, "--vars", "skewed_var,outlier_var,zero_var,near_zero_var,high_missing_var", "--group", "group2", "--digits", "3")
    Run-OkR "frequencies edge" (Join-Path $rScriptDir "frequencies.R") @("--parquet", $parquetPath, "--vars", "cat_var", "--group", "group2")
    Run-OkR "crosstabs edge" (Join-Path $rScriptDir "crosstabs.R") @("--parquet", $parquetPath, "--row", "cat_var", "--col", "group2", "--fisher", "TRUE", "--fisher-simulate", "TRUE", "--fisher-b", "200")
    Run-OkR "correlations edge" (Join-Path $rScriptDir "correlations.R") @("--parquet", $parquetPath, "--x", "skewed_var,outlier_var", "--y", "age,income", "--method", "spearman", "--missing", "complete", "--p-adjust", "BH")
    Run-OkR "scale edge" (Join-Path $rScriptDir "scale.R") @("--parquet", $parquetPath, "--vars", "f2_1,f2_2,f2_3,f2_4_rev", "--reverse", "f2_4_rev", "--reverse-min", "1", "--reverse-max", "5", "--missing", "complete", "--omega", "FALSE")
    Run-OkR "assumptions edge" (Join-Path $rScriptDir "assumptions.R") @("--parquet", $parquetPath, "--analysis", "anova", "--dv", "outcome_anova", "--between", "group3", "--within", "pre_score,mid_score,post_score", "--subject-id", "id")
    Run-OkR "regression edge bootstrap" (Join-Path $rScriptDir "regression.R") @("--parquet", $parquetPath, "--dv", "outcome_reg", "--ivs", "x1,x2,x3", "--bootstrap", "TRUE", "--bootstrap-samples", "200", "--seed", "42")
    if ($hasLavaan) {
        Run-OkR "sem edge path" (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "path", "--dv", "outcome_reg", "--ivs", "skewed_var,outlier_var")
    }
    Run-OkR "anova edge mixed" (Join-Path $rScriptDir "anova.R") @("--parquet", $parquetPath, "--within", "pre_score,mid_score,post_score", "--between", "group3", "--subject-id", "id", "--posthoc", "pairwise")
    Run-OkR "t_test edge independent" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--vars", "pre_score", "--group", "group2", "--var-equal", "TRUE")
    Run-OkR "t_test edge bootstrap paired" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--x", "pre_score", "--y", "post_score", "--bootstrap", "TRUE", "--bootstrap-samples", "200", "--seed", "42")

    Run-InitWorkspace "init workspace (missings clean)"
    Run-OkR "missings clean" (Join-Path $rScriptDir "missings.R") @("--parquet", $parquetPath, "--vars", "outcome_anova,x1,x2,x3,mediator", "--method", "listwise")

    Run-InitWorkspace "init workspace (missings edge)"
    Run-OkR "missings edge" (Join-Path $rScriptDir "missings.R") @("--parquet", $parquetPath, "--vars", "age,income,satisfaction,outcome_reg,high_missing_var,all_missing_var", "--method", "indicator", "--indicator-threshold", "0.2", "--drop-threshold", "0.5")

    Run-InitWorkspace "init workspace (transform clean)"
    Run-OkR "data_transform clean" (Join-Path $rScriptDir "data_transform.R") @("--parquet", $parquetPath, "--calc", "score_avg=(f1_1+f1_2+f1_4)/3", "--standardize", "x1")

    Run-InitWorkspace "init workspace (transform edge)"
    Run-OkR "data_transform edge" (Join-Path $rScriptDir "data_transform.R") @("--parquet", $parquetPath, "--transform", "skewed_var=log", "--percentile-bins", "outlier_var=4", "--recode", "group3=A:1,B:2,C:3", "--drop", "zero_var", "--confirm-drop", "TRUE")

    Template-Test "template init_workspace apa" "init_workspace.default" $apaReportPath (Join-Path $rScriptDir "init_workspace.R") @("--csv", $dataPath)
    Template-Test "template init_workspace scratchpad" "init_workspace.scratchpad" $scratchpadPath (Join-Path $rScriptDir "init_workspace.R") @("--csv", $dataPath)
    Template-Test "template descriptive_stats" "descriptive_stats.default" $apaReportPath (Join-Path $rScriptDir "descriptive_stats.R") @("--parquet", $parquetPath, "--vars", "outcome_anova,x1,x2")
    Template-Test "template frequencies default" "frequencies.default" $apaReportPath (Join-Path $rScriptDir "frequencies.R") @("--parquet", $parquetPath, "--vars", "gender,education")
    Template-Test "template frequencies grouped" "frequencies.grouped" $apaReportPath (Join-Path $rScriptDir "frequencies.R") @("--parquet", $parquetPath, "--vars", "gender", "--group", "group3")
    Template-Test "template crosstabs default" "crosstabs.default" $apaReportPath (Join-Path $rScriptDir "crosstabs.R") @("--parquet", $parquetPath, "--row", "gender", "--col", "group3")
    Template-Test "template crosstabs grouped" "crosstabs.grouped" $apaReportPath (Join-Path $rScriptDir "crosstabs.R") @("--parquet", $parquetPath, "--row", "gender", "--col", "group3", "--group", "site")
    Template-Test "template correlations default" "correlations.default" $apaReportPath (Join-Path $rScriptDir "correlations.R") @("--parquet", $parquetPath, "--vars", "x1,x2,x3")
    Template-Test "template correlations cross" "correlations.cross" $apaReportPath (Join-Path $rScriptDir "correlations.R") @("--parquet", $parquetPath, "--x", "x1,x2", "--y", "x3,mediator")
    Template-Test "template scale" "scale.default" $apaReportPath (Join-Path $rScriptDir "scale.R") @("--parquet", $parquetPath, "--vars", "f1_1,f1_2,f1_3_rev,f1_4", "--reverse", "f1_3_rev", "--reverse-min", "1", "--reverse-max", "5")
    Template-Test "template data_explorer" "data_explorer.default" $apaReportPath (Join-Path $rScriptDir "data_explorer.R") @("--parquet", $parquetPath, "--vars", "id,site,group3")
    Template-TestOptional "template data_transform" "data_transform.default" $apaReportPath (Join-Path $rScriptDir "data_transform.R") @("--parquet", $parquetPath, "--calc", "score_avg=(f1_1+f1_2+f1_4)/3")
    Run-InitWorkspace "init workspace (templates after transform)"
    Template-TestOptional "template missings" "missings.default" $apaReportPath (Join-Path $rScriptDir "missings.R") @("--parquet", $parquetPath, "--vars", "age,income,satisfaction", "--method", "listwise")
    Run-InitWorkspace "init workspace (templates after missings)"
    Template-Test "template assumptions ttest" "assumptions.ttest" $apaReportPath (Join-Path $rScriptDir "assumptions.R") @("--parquet", $parquetPath, "--analysis", "ttest", "--vars", "x1", "--group", "group2")
    Template-Test "template assumptions anova" "assumptions.anova" $apaReportPath (Join-Path $rScriptDir "assumptions.R") @("--parquet", $parquetPath, "--analysis", "anova", "--dv", "outcome_anova", "--between", "group3")
    Template-Test "template assumptions regression" "assumptions.regression" $apaReportPath (Join-Path $rScriptDir "assumptions.R") @("--parquet", $parquetPath, "--analysis", "regression", "--dv", "outcome_anova", "--ivs", "x1,x2,x3")
    Template-Test "template regression" "regression.default" $apaReportPath (Join-Path $rScriptDir "regression.R") @("--parquet", $parquetPath, "--dv", "outcome_reg", "--ivs", "x1,x2,x3")
    if ($hasLavaan) {
        Template-Test "template sem default" "sem.default" $apaReportPath (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "path", "--dv", "outcome_reg", "--ivs", "x1,x2")
        Template-Test "template sem mediation" "sem.mediation" $apaReportPath (Join-Path $rScriptDir "sem.R") @("--parquet", $parquetPath, "--analysis", "mediation", "--x", "x1", "--m", "mediator", "--y", "outcome_reg")
    } else {
        Write-Log "[WARN] skipping sem templates (lavaan not installed)"
    }

    if ($hasLme4) {
        Template-Test "template mixed_models default" "mixed_models.default" $mixedApaReportPath (Join-Path $rScriptDir "mixed_models.R") @("--parquet", $mixedParquetPath, "--formula", "score ~ time + group3 + x1 + (1|id)")
        if ($hasEmmeans) {
            Template-Test "template mixed_models emmeans" "mixed_models.emmeans" $mixedApaReportPath (Join-Path $rScriptDir "mixed_models.R") @("--parquet", $mixedParquetPath, "--formula", "score ~ time * group3 + x1 + (1|id)", "--emmeans", "time*group3", "--contrasts", "pairwise")
        } else {
            Write-Log "[WARN] skipping mixed_models emmeans template (emmeans not installed)"
        }
    } else {
        Write-Log "[WARN] skipping mixed_models templates (lme4 not installed)"
    }

    Template-Test "template anova default" "anova.default" $apaReportPath (Join-Path $rScriptDir "anova.R") @("--parquet", $parquetPath, "--dv", "outcome_anova", "--between", "group3")
    Template-Test "template anova posthoc" "anova.posthoc" $apaReportPath (Join-Path $rScriptDir "anova.R") @("--parquet", $parquetPath, "--dv", "outcome_anova", "--between", "group3")
    Template-Test "template t_test" "t_test.default" $apaReportPath (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--vars", "x1", "--mu", "0")

    Run-ExpectLogR "t_test invalid group levels" (Resolve-LogPath) "t_test" "expected_invalid_input" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--vars", "outcome_anova", "--group", "group3", "--expect-two-groups", "TRUE")
    Run-ExpectLogR "t_test paired with group" (Resolve-LogPath) "t_test" "invalid_input" (Join-Path $rScriptDir "t_test.R") @("--parquet", $parquetPath, "--x", "pre_score", "--y", "post_score", "--group", "group3")
    Run-ExpectLogR "anova missing subject-id" (Resolve-LogPath) "anova" "invalid_input" (Join-Path $rScriptDir "anova.R") @("--parquet", $parquetPath, "--within", "pre_score,post_score")
    Run-ExpectLogR "regression missing dv" (Resolve-LogPath) "regression" "invalid_input" (Join-Path $rScriptDir "regression.R") @("--parquet", $parquetPath, "--ivs", "x1,x2")

    Write-Log "[DONE] smoke tests finished"

    function Cleanup-Runs {
        param([int]$Keep)
        if (-not (Test-Path $runsBase)) { return }
        $runDirs = Get-ChildItem -Path $runsBase -Directory | Where-Object { $_.Name -match '^\d{14}$' } | Sort-Object Name
        if ($runDirs.Count -le $Keep) { return }
        $remove = $runDirs | Select-Object -First ($runDirs.Count - $Keep)
        foreach ($dir in $remove) {
            Remove-Item -Recurse -Force $dir.FullName
        }
    }

    $keepRuns = 10
    if ($env:CORE_STATS_KEEP_RUNS -and ($env:CORE_STATS_KEEP_RUNS -as [int])) {
        $keepRuns = [int]$env:CORE_STATS_KEEP_RUNS
    }
    Cleanup-Runs $keepRuns
} finally {
    if (Test-Path $configBak) {
        Copy-Item $configBak $configPath -Force
        Remove-Item $configBak -Force
    }
    if (Test-Path $configBase) {
        Remove-Item $configBase -Force
    }
}

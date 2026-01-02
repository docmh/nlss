$ErrorActionPreference = "Stop"

$ScriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$CodexCwd = (Resolve-Path $ScriptDir).Path
$BaseDir = $CodexCwd
$PromptsCsv = Join-Path $ScriptDir "prompts.csv"
$OutputRoot = Join-Path $ScriptDir "..\\..\\outputs\\prompt-robustness-runs"
$RunTimestamp = Get-Date -Format "yyyyMMdd_HHmmss"
$RunDir = Join-Path $OutputRoot $RunTimestamp
$CursorFile = Join-Path $RunDir "prompts.cursor"
$CodexOut = Join-Path $RunDir "codex_last.out"
$CodexErr = Join-Path $RunDir "codex_last.err"
$WorkspaceBackupDir = Join-Path $RunDir "workspace_backup"
$Effort = "low"
$ExtraArgs = @()
$PromptCursorOverride = $null

for ($i = 0; $i -lt $args.Length; $i++) {
  $arg = $args[$i]
  if ($arg -eq "--cd") {
    if ($i + 1 -lt $args.Length) {
      $CodexCwd = $args[$i + 1]
      if (-not (Test-Path $CodexCwd)) {
        throw "Path not found for --cd: $CodexCwd"
      }
      $BaseDir = (Resolve-Path $CodexCwd).Path
      $i++
    }
    continue
  }
  if ($arg -eq "--prompt-cursor") {
    if ($i + 1 -lt $args.Length) {
      $PromptCursorOverride = $args[$i + 1]
      $i++
    }
    continue
  }
  if ($arg -eq "--effort") {
    if ($i + 1 -lt $args.Length) {
      $Effort = $args[$i + 1]
      $i++
    }
    continue
  }
  if ($arg -like "--effort=*") {
    $Effort = $arg.Split("=", 2)[1]
    continue
  }
  $ExtraArgs += $arg
}

if (-not $Effort -or $Effort.Trim().Length -eq 0) {
  $Effort = "low"
}

$Effort = $Effort.ToLowerInvariant()

if (-not (Test-Path $PromptsCsv)) {
  Write-Error "prompts.csv not found at $PromptsCsv"
  exit 1
}

if ($PromptCursorOverride) {
  if ($PromptCursorOverride -notmatch "^[0-9]+$") {
    throw "Invalid --prompt-cursor value (expected integer): $PromptCursorOverride"
  }
  Set-Content -Path $CursorFile -Value $PromptCursorOverride
}

New-Item -ItemType Directory -Force -Path $RunDir | Out-Null

if (-not (Test-Path $OutputRoot)) {
  New-Item -ItemType Directory -Force -Path $OutputRoot | Out-Null
}

if (Test-Path $OutputRoot) {
  $runDirs = Get-ChildItem -Directory $OutputRoot | Sort-Object Name -Descending
  if ($runDirs.Count -gt 10) {
    $runDirs | Select-Object -Skip 10 | ForEach-Object { Remove-Item -Recurse -Force $_.FullName }
  }
}

if (-not (Get-Command codex -ErrorAction SilentlyContinue)) {
  Write-Error "codex CLI not found in PATH."
  exit 1
}

function Resolve-WorkspaceRoot {
  if ($env:NLSS_WORKSPACE) {
    $ws = Resolve-Path $env:NLSS_WORKSPACE
    if (Test-Path (Join-Path $ws "nlss-workspace.yml")) { return $ws }
    throw "NLSS_WORKSPACE does not contain nlss-workspace.yml: $ws"
  }

  $parent = Split-Path -Parent $BaseDir
  if (Test-Path (Join-Path $parent "nlss-workspace.yml")) { return $parent }

  if (Test-Path (Join-Path $BaseDir "nlss-workspace.yml")) { return $BaseDir }

  $childCandidates = Get-ChildItem -Directory $BaseDir | Where-Object {
    Test-Path (Join-Path $_.FullName "nlss-workspace.yml")
  }
  if ($childCandidates.Count -eq 1) { return $childCandidates[0].FullName }
  if ($childCandidates.Count -gt 1) { throw "Multiple nlss-workspace.yml files found in child directories; set NLSS_WORKSPACE explicitly." }

  $candidates = Get-ChildItem -Directory $parent | Where-Object {
    Test-Path (Join-Path $_.FullName "nlss-workspace.yml")
  }

  if ($candidates.Count -eq 1) { return $candidates[0].FullName }
  if ($candidates.Count -eq 0) { throw "No nlss-workspace.yml found in parent or sibling directories. Use --cd or set NLSS_WORKSPACE." }
  throw "Multiple nlss-workspace.yml files found; set NLSS_WORKSPACE explicitly."
}

function Resolve-DatasetDir {
  param([string]$WorkspaceRoot)

  $manifest = Join-Path $WorkspaceRoot "nlss-workspace.yml"
  if (-not (Test-Path $manifest)) { throw "Missing workspace manifest: $manifest" }

  $active = $null
  $manifestText = Get-Content $manifest -Raw
  if ($manifestText -match '(?m)^\s*active_dataset:\s*"?([^"\r\n]+)"?\s*$') {
    $active = $Matches[1].Trim()
  }

  if ($active) {
    $candidate = Join-Path $WorkspaceRoot $active
    if (Test-Path $candidate) { return $candidate }
    throw "active_dataset directory not found: $candidate"
  }

  $candidates = Get-ChildItem -Directory $WorkspaceRoot | Where-Object {
    (Test-Path (Join-Path $_.FullName "analysis_log.jsonl")) -or
    (Test-Path (Join-Path $_.FullName "report_canonical.md"))
  }

  if ($candidates.Count -eq 1) { return $candidates[0].FullName }
  if ($candidates.Count -eq 0) { throw "No dataset folder found in workspace." }
  throw "Multiple dataset folders found; set active_dataset in nlss-workspace.yml."
}

$WorkspaceRoot = Resolve-WorkspaceRoot
$DatasetDir = Resolve-DatasetDir -WorkspaceRoot $WorkspaceRoot

if (Test-Path $WorkspaceBackupDir) {
  Remove-Item -Recurse -Force $WorkspaceBackupDir
}
New-Item -ItemType Directory -Force -Path $WorkspaceBackupDir | Out-Null
Copy-Item -Path (Join-Path $DatasetDir "*") -Destination $WorkspaceBackupDir -Recurse -Force

function Restore-Workspace {
  Get-ChildItem -Force $DatasetDir | ForEach-Object { Remove-Item -Recurse -Force $_.FullName }
  Copy-Item -Path (Join-Path $WorkspaceBackupDir "*") -Destination $DatasetDir -Recurse -Force
}

function Invoke-Codex {
  param(
    [string]$Effort,
    [string]$Prompt,
    [string[]]$BaseArgs,
    [string]$StdOut,
    [string]$StdErr
  )

  $configArgs = @()
  if ($Effort) { $configArgs = @("--config", "model_reasoning_effort=$Effort") }
  $fullArgs = $BaseArgs + $configArgs + "-"

  $prevErrAction = $ErrorActionPreference
  $ErrorActionPreference = "Continue"
  $Prompt | & codex @fullArgs 1>$StdOut 2>$StdErr
  $exitCode = $LASTEXITCODE
  $ErrorActionPreference = $prevErrAction
  return $exitCode
}

while ($true) {
  $rows = Import-Csv $PromptsCsv
  if ($rows.Count -eq 0) { throw "prompts.csv is empty." }

  $cursor = 1
  if (Test-Path $CursorFile) {
    $cursorText = (Get-Content $CursorFile -TotalCount 1).Trim()
    if ($cursorText -match "^[0-9]+$") { $cursor = [int]$cursorText }
  }

  if ($cursor -lt 1 -or $cursor -gt $rows.Count) { $cursor = 1 }

  $subskill = $rows[$cursor - 1].subskill
  $prompt = $rows[$cursor - 1].nl_prompt
  $rowCsvLine = ($rows[$cursor - 1] | ConvertTo-Csv -NoTypeInformation)[1]
  if (-not $prompt) { throw "nl_prompt missing at row $cursor." }

  $next = $cursor + 1
  if ($next -gt $rows.Count) { $next = 1 }

  $finalPrompt = "`$nlss $prompt No follow-ups please."
  Write-Output $finalPrompt

  $codexBaseArgs = @(
    "exec",
    "--cd", $CodexCwd,
    "--sandbox", "workspace-write",
    "--skip-git-repo-check"
  ) + $ExtraArgs

  $exitCode = 1
  if ($Effort) {
    if ($Effort -in @("xhigh", "extra_high", "extra-high", "extra high")) {
      $exitCode = Invoke-Codex -Effort "xhigh" -Prompt $finalPrompt -BaseArgs $codexBaseArgs -StdOut $CodexOut -StdErr $CodexErr
      if ($exitCode -ne 0) {
        Write-Output "info: reasoning effort 'xhigh' failed; falling back to 'high'."
        $exitCode = Invoke-Codex -Effort "high" -Prompt $finalPrompt -BaseArgs $codexBaseArgs -StdOut $CodexOut -StdErr $CodexErr
      }
    } else {
      $exitCode = Invoke-Codex -Effort $Effort -Prompt $finalPrompt -BaseArgs $codexBaseArgs -StdOut $CodexOut -StdErr $CodexErr
    }
  } else {
    $exitCode = Invoke-Codex -Effort "" -Prompt $finalPrompt -BaseArgs $codexBaseArgs -StdOut $CodexOut -StdErr $CodexErr
  }

  if ($exitCode -ne 0) {
    Write-Error "codex exec failed; see $CodexOut and $CodexErr"
    Restore-Workspace
    exit 1
  }

  $analysisLog = Join-Path $DatasetDir "analysis_log.jsonl"
  $protocolLog = Join-Path $RunDir "protocol_log.jsonl"

  if (Test-Path $analysisLog) {
    $lines = Get-Content $analysisLog
    if ($lines.Count -gt 0) {
      $header = [ordered]@{
        prompt_csv_line = $rowCsvLine
        target_subskill = $subskill
        target_prompt   = $prompt
      }
      Add-Content -Path $protocolLog -Value ($header | ConvertTo-Json -Compress)
      foreach ($line in $lines) {
        if (-not $line) { continue }
        try {
          $obj = $line | ConvertFrom-Json -ErrorAction Stop
          if ($obj.module -eq "init_workspace") { continue }
          $record = [ordered]@{
            target_subskill = $subskill
            target_prompt = $prompt
          }
          foreach ($prop in $obj.PSObject.Properties) {
            if (-not $record.Contains($prop.Name)) {
              $record[$prop.Name] = $prop.Value
            }
          }
          $outLine = $record | ConvertTo-Json -Compress
          Add-Content -Path $protocolLog -Value $outLine
        } catch {
          Add-Content -Path $protocolLog -Value ("target_subskill={0}`ttarget_prompt={1}`t{2}" -f $subskill, $prompt, $line)
        }
      }
    }
  }

  Restore-Workspace

  Set-Content -Path $CursorFile -Value $next
}

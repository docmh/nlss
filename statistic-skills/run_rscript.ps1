function Get-RscriptPath {
    if ($env:RSCRIPT -and (Test-Path $env:RSCRIPT)) {
        return $env:RSCRIPT
    }

    $cmd = Get-Command Rscript -ErrorAction SilentlyContinue
    if ($cmd) {
        return $cmd.Source
    }

    $candidates = @()
    $candidates += Get-ChildItem "C:\Program Files\R\R-*\bin\Rscript.exe" -ErrorAction SilentlyContinue
    $candidates += Get-ChildItem "C:\Program Files\R\R-*\bin\x64\Rscript.exe" -ErrorAction SilentlyContinue

    if ($candidates.Count -gt 0) {
        return ($candidates | Sort-Object FullName | Select-Object -Last 1).FullName
    }

    return $null
}

$rscript = Get-RscriptPath
if (-not $rscript) {
    Write-Error "Rscript not found. Install R or set RSCRIPT to the full path of Rscript.exe."
    exit 127
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$defaultScript = Join-Path $scriptDir "descriptive-stats-r\scripts\descriptive_stats.R"

$allArgs = @($args)
$target = $defaultScript

if ($allArgs.Count -gt 0) {
    $first = $allArgs[0]
    $candidate = $null

    if ($first -match '\.R$' -or (Test-Path $first -PathType Leaf)) {
        if (Test-Path $first -PathType Leaf) {
            $candidate = (Resolve-Path $first).Path
        } elseif (Test-Path (Join-Path $scriptDir $first) -PathType Leaf) {
            $candidate = (Resolve-Path (Join-Path $scriptDir $first)).Path
        }
    }

    if ($candidate) {
        $target = $candidate
        if ($allArgs.Count -gt 1) {
            $allArgs = $allArgs[1..($allArgs.Count - 1)]
        } else {
            $allArgs = @()
        }
    }
}

if (-not (Test-Path $target -PathType Leaf)) {
    Write-Error "R script not found: $target"
    exit 2
}

& $rscript $target @allArgs
exit $LASTEXITCODE

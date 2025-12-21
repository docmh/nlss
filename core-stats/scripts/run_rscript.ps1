function Get-WslExe {
    $cmd = Get-Command wsl.exe -ErrorAction SilentlyContinue
    if ($cmd) {
        return $cmd.Source
    }
    return $null
}

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

function Test-PathLike {
    param([string]$Value)

    if ($Value -match '^/mnt/[a-z]/' -or $Value -match '^/') {
        return $true
    }
    if ($Value -match '^[A-Za-z]:\\' -or $Value -match '^\\\\') {
        return $true
    }
    if ($Value -match '^[.]{1,2}[\\/]' -or $Value -match '[\\/]+') {
        return $true
    }
    if ($Value -match '\.(csv|tsv|txt|md|rds|rdata|rda|sav|xlsx|R)$') {
        return $true
    }
    return $false
}

function Convert-ToWslPath {
    param(
        [string]$Path,
        [string]$WslExe
    )

    if ($Path -match '^/') {
        return $Path
    }

    $fullPath = $Path
    if (-not [System.IO.Path]::IsPathRooted($fullPath)) {
        $fullPath = [System.IO.Path]::GetFullPath($fullPath, $PWD.Path)
    }

    $converted = & $WslExe "wslpath" "-a" "-u" $fullPath 2>$null
    if ($LASTEXITCODE -eq 0 -and $converted) {
        return $converted.Trim()
    }

    return $Path
}

function Convert-ArgsToWsl {
    param(
        [string[]]$InputArgs,
        [string]$WslExe
    )

    $pathFlags = @("--csv", "--rds", "--rdata", "--out", "--file", "--input", "--data")
    $converted = @()

    for ($i = 0; $i -lt $InputArgs.Count; $i++) {
        $arg = $InputArgs[$i]

        if ($pathFlags -contains $arg -and ($i + 1) -lt $InputArgs.Count) {
            $converted += $arg
            $converted += Convert-ToWslPath -Path $InputArgs[$i + 1] -WslExe $WslExe
            $i++
            continue
        }

        if (Test-PathLike -Value $arg) {
            $converted += Convert-ToWslPath -Path $arg -WslExe $WslExe
            continue
        }

        $converted += $arg
    }

    return $converted
}

$wslExe = Get-WslExe
$rscript = Get-RscriptPath

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$defaultScript = Join-Path $scriptDir "R\descriptive_stats.R"

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

if ($wslExe) {
    $wslTarget = Convert-ToWslPath -Path $target -WslExe $wslExe
    $wslArgs = Convert-ArgsToWsl -InputArgs $allArgs -WslExe $wslExe
    & $wslExe "Rscript" $wslTarget @wslArgs
    if ($LASTEXITCODE -eq 0) {
        exit 0
    }
    Write-Warning "WSL execution failed; falling back to Windows Rscript."
}

if (-not $rscript) {
    Write-Error "Rscript not found. Install R or set RSCRIPT to the full path of Rscript.exe."
    exit 127
}

& $rscript $target @allArgs
exit $LASTEXITCODE

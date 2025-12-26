function Get-WslExe {
    $cmd = Get-Command wsl.exe -ErrorAction SilentlyContinue
    if ($cmd) {
        return $cmd.Source
    }
    return $null
}

function Set-ConsoleUtf8 {
    try {
        $utf8 = New-Object System.Text.UTF8Encoding $false
        [Console]::InputEncoding = $utf8
        [Console]::OutputEncoding = $utf8
        $OutputEncoding = $utf8
    } catch {
        # Ignore encoding failures; fallback to default console encoding.
    }
}

Set-ConsoleUtf8

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
    if ($Value -match '\.(csv|tsv|txt|md|rds|rdata|rda|sav|xlsx|parquet|R)$') {
        return $true
    }
    return $false
}

function Test-NeedsPathRepair {
    param([string]$Value)

    if ($null -eq $Value -or $Value -eq "") {
        return $false
    }
    if ($Value -match '[^\x00-\x7F]') {
        return $true
    }
    if ($Value -match '\?' -or $Value -match '\uFFFD') {
        return $true
    }
    return $false
}

function Test-HasNonAscii {
    param([string[]]$Values)

    foreach ($value in $Values) {
        if ($null -ne $value -and $value -match '[^\x00-\x7F]') {
            return $true
        }
    }
    return $false
}

function Get-MatchKey {
    param([string]$Value)

    if ($null -eq $Value) {
        return ""
    }
    $normalized = $Value.Normalize([System.Text.NormalizationForm]::FormD)
    $sb = New-Object System.Text.StringBuilder
    foreach ($ch in $normalized.ToCharArray()) {
        $cat = [Globalization.CharUnicodeInfo]::GetUnicodeCategory($ch)
        if ($cat -ne [Globalization.UnicodeCategory]::NonSpacingMark) {
            [void]$sb.Append($ch)
        }
    }
    $clean = $sb.ToString().Normalize([System.Text.NormalizationForm]::FormC)
    $clean = $clean -replace '\u00DF', 'ss'
    $clean = $clean -replace '\u1E9E', 'ss'
    $clean = $clean -replace '\u00E6', 'ae'
    $clean = $clean -replace '\u00C6', 'ae'
    $clean = $clean -replace '\u0153', 'oe'
    $clean = $clean -replace '\u0152', 'oe'
    $clean.ToLowerInvariant()
}

function Get-FuzzyRegex {
    param([string]$Value)

    $normalized = Get-MatchKey -Value $Value
    if (-not $normalized) {
        return ""
    }
    $token = "__WILDCARD__"
    $normalized = $normalized -replace '\?', $token
    $normalized = $normalized -replace '\uFFFD', $token
    $escaped = [Regex]::Escape($normalized)
    $escaped = $escaped -replace [Regex]::Escape($token), "."
    "^$escaped$"
}

function Resolve-PathSegment {
    param(
        [string]$BasePath,
        [string]$Segment,
        [bool]$PreferContainer
    )

    if (-not (Test-Path -LiteralPath $BasePath)) {
        return $null
    }
    $entries = Get-ChildItem -LiteralPath $BasePath -Force -ErrorAction SilentlyContinue
    if (-not $entries) {
        return $null
    }
    $exact = $entries | Where-Object { $_.Name -ieq $Segment }
    if ($PreferContainer) {
        $exact = $exact | Where-Object { $_.PSIsContainer }
    }
    if ($exact.Count -eq 1) {
        return $exact[0].Name
    }

    $pattern = Get-FuzzyRegex -Value $Segment
    if (-not $pattern) {
        return $null
    }
    $matchCandidates = @()
    foreach ($entry in $entries) {
        if ($PreferContainer -and -not $entry.PSIsContainer) {
            continue
        }
        $candidate = Get-MatchKey -Value $entry.Name
        if ($candidate -cmatch $pattern) {
            $matchCandidates += $entry
        }
    }
    if ($matchCandidates.Count -eq 1) {
        return $matchCandidates[0].Name
    }
    return $null
}

function Resolve-FuzzyPath {
    param([string]$Path)

    if (-not $Path) {
        return $Path
    }
    if ($Path -match '^/') {
        return $Path
    }
    $expanded = [Environment]::ExpandEnvironmentVariables($Path)
    if (-not [System.IO.Path]::IsPathRooted($expanded)) {
        $expanded = [System.IO.Path]::GetFullPath((Join-Path (Get-Location).Path $expanded))
    }
    $root = [System.IO.Path]::GetPathRoot($expanded)
    if (-not $root) {
        return $Path
    }
    $relative = $expanded.Substring($root.Length)
    $segments = $relative -split '[\\/]' | Where-Object { $_ -ne "" }
    $current = $root
    for ($i = 0; $i -lt $segments.Count; $i++) {
        $segment = $segments[$i]
        $isLast = ($i -eq ($segments.Count - 1))
        $resolvedName = Resolve-PathSegment -BasePath $current -Segment $segment -PreferContainer (-not $isLast)
        if (-not $resolvedName) {
            return $Path
        }
        $current = Join-Path $current $resolvedName
    }
    if (Test-Path -LiteralPath $current) {
        return (Resolve-Path -LiteralPath $current).Path
    }
    $Path
}

function Resolve-InputPath {
    param([string]$Value)

    if ($null -eq $Value -or $Value -eq "") {
        return $Value
    }
    if ($Value -match '^/') {
        return $Value
    }
    if (Test-Path -LiteralPath $Value) {
        return (Resolve-Path -LiteralPath $Value).Path
    }
    if (-not (Test-NeedsPathRepair -Value $Value)) {
        return $Value
    }
    $resolved = Resolve-FuzzyPath -Path $Value
    if ($resolved -and $resolved -ne $Value) {
        Write-Warning "Recovered a path with non-ASCII characters: $Value -> $resolved"
        return $resolved
    }
    $Value
}

function Resolve-ArgsPaths {
    param([string[]]$InputArgs)

    $pathFlags = @("--csv", "--rds", "--rdata", "--file", "--input", "--data", "--parquet", "--sav")
    $resolved = @()

    for ($i = 0; $i -lt $InputArgs.Count; $i++) {
        $arg = $InputArgs[$i]
        if ($pathFlags -contains $arg -and ($i + 1) -lt $InputArgs.Count) {
            $resolved += $arg
            $resolved += Resolve-InputPath -Value $InputArgs[$i + 1]
            $i++
            continue
        }
        if (Test-PathLike -Value $arg) {
            $resolved += Resolve-InputPath -Value $arg
            continue
        }
        $resolved += $arg
    }
    return $resolved
}

function Get-PathArgs {
    param([string[]]$InputArgs)

    $pathFlags = @("--csv", "--rds", "--rdata", "--file", "--input", "--data", "--parquet", "--sav")
    $paths = @()
    for ($i = 0; $i -lt $InputArgs.Count; $i++) {
        $arg = $InputArgs[$i]
        if ($pathFlags -contains $arg -and ($i + 1) -lt $InputArgs.Count) {
            $paths += $InputArgs[$i + 1]
            $i++
            continue
        }
        if (Test-PathLike -Value $arg) {
            $paths += $arg
        }
    }
    return $paths
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
        $fullPath = [System.IO.Path]::Combine($PWD.Path, $fullPath)
        $fullPath = [System.IO.Path]::GetFullPath($fullPath)
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

    $pathFlags = @("--csv", "--rds", "--rdata", "--file", "--input", "--data", "--parquet", "--sav")
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
if ($env:NLSS_SKIP_WSL) {
    $wslExe = $null
}

$scriptDir = Split-Path -Parent $MyInvocation.MyCommand.Path
$defaultScript = Join-Path $scriptDir "R\descriptive_stats.R"

$allArgs = @($args)
$target = $defaultScript

if ($allArgs.Count -gt 0) {
    $first = $allArgs[0]
    $candidate = $null

    if ($first -match '\.R$' -or (Test-Path -LiteralPath $first -PathType Leaf)) {
        if (Test-Path -LiteralPath $first -PathType Leaf) {
            $candidate = (Resolve-Path -LiteralPath $first).Path
        } elseif (Test-Path -LiteralPath (Join-Path $scriptDir $first) -PathType Leaf) {
            $candidate = (Resolve-Path -LiteralPath (Join-Path $scriptDir $first)).Path
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

$target = Resolve-InputPath -Value $target
$allArgs = Resolve-ArgsPaths -InputArgs $allArgs

if (-not (Test-Path -LiteralPath $target -PathType Leaf)) {
    Write-Error "R script not found: $target"
    exit 2
}

$pathArgs = Get-PathArgs -InputArgs $allArgs
$hasUnicodeArgs = Test-HasNonAscii -Values (@($target) + $pathArgs)
if (-not $hasUnicodeArgs) {
    $hasUnicodeArgs = Test-NeedsPathRepair -Value $target
    if (-not $hasUnicodeArgs) {
        foreach ($pathArg in $pathArgs) {
            if (Test-NeedsPathRepair -Value $pathArg) {
                $hasUnicodeArgs = $true
                break
            }
        }
    }
}
if ($wslExe -and $rscript -and $hasUnicodeArgs -and -not $env:NLSS_FORCE_WSL) {
    $wslExe = $null
}

$envCwd = $env:NLSS_RSCRIPT_CWD
$useEnvCwd = $false
if ($envCwd -and (Test-Path -LiteralPath $envCwd)) {
    $useEnvCwd = $true
}

$prevLocation = Get-Location

if ($wslExe) {
    if ($useEnvCwd) {
        Set-Location $envCwd
    }
    $wslTarget = Convert-ToWslPath -Path $target -WslExe $wslExe
    $wslArgs = Convert-ArgsToWsl -InputArgs $allArgs -WslExe $wslExe
    & $wslExe "Rscript" $wslTarget @wslArgs
    if ($LASTEXITCODE -eq 0) {
        if ($useEnvCwd) {
            Set-Location $prevLocation
        }
        exit 0
    }
    if ($useEnvCwd) {
        Set-Location $prevLocation
    }
    Write-Warning "WSL execution failed; falling back to Windows Rscript."
}

if (-not $rscript) {
    Write-Error "Rscript not found. Install R or set RSCRIPT to the full path of Rscript.exe."
    exit 127
}

try {
    $runDir = $prevLocation.Path
    if ($useEnvCwd) {
        $runDir = $envCwd
    }
    if ($runDir) {
        Set-Location $runDir
    }
    & $rscript $target @allArgs
    exit $LASTEXITCODE
} finally {
    Set-Location $prevLocation
}

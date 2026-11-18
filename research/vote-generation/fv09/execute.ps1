# Exact manifest executor. Dry-run is the default; no recursive deletion.
param([switch]$Apply)
$ErrorActionPreference = 'Stop'
$workspace = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$manifestPath = Join-Path $PSScriptRoot 'manifest.json'
$manifest = Get-Content -LiteralPath $manifestPath -Raw | ConvertFrom-Json
if ($manifest.status -ne 'dry-run') { throw 'Manifest has already been executed.' }
function LocalPath([string]$relativePath) {
    $target = [System.IO.Path]::GetFullPath((Join-Path $workspace $relativePath))
    if (-not $target.StartsWith($workspace + [System.IO.Path]::DirectorySeparatorChar, [System.StringComparison]::OrdinalIgnoreCase)) { throw "Outside workspace: $relativePath" }
    if ($relativePath.Replace('\','/').StartsWith('docs/model-drafts/')) { throw 'Protected authoring directory' }
    return $target
}
function VerifyOriginal($entry) {
    $target = LocalPath $entry.path
    $item = Get-Item -LiteralPath $target
    if ($item.PSIsContainer -or $item.LinkType) { throw "Not an ordinary file: $target" }
    if ($item.Length -ne $entry.bytes -or (Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash.ToLowerInvariant() -ne $entry.sha256) { throw "Identity changed: $target" }
    return $target
}
$moves = @($manifest.entries | Where-Object action -eq 'archive')
$deletions = @($manifest.entries | Where-Object action -eq 'delete')
# Preflight the whole mutation set before changing any file.
foreach ($entry in @($moves) + @($deletions)) {
    $null = VerifyOriginal $entry
    if ($entry.action -eq 'archive') {
        $destination = LocalPath $entry.destination
        $archiveRoot = [System.IO.Path]::GetFullPath((Join-Path $workspace 'research/vote-generation')) + [System.IO.Path]::DirectorySeparatorChar
        if (-not $destination.StartsWith($archiveRoot, [System.StringComparison]::OrdinalIgnoreCase)) { throw 'Invalid archive destination' }
        if (Test-Path -LiteralPath $destination) { throw "Destination exists: $destination" }
    } else {
        $target = LocalPath $entry.path
        $valid = $false
        foreach ($family in @('generation','drift','party-drift','filing-aware-research','legacy-comparison','current-camera-comparison')) {
            $allowedRoot = [System.IO.Path]::GetFullPath((Join-Path $workspace "data/derived/$family")) + [System.IO.Path]::DirectorySeparatorChar
            if ($target.StartsWith($allowedRoot, [System.StringComparison]::OrdinalIgnoreCase)) { $valid = $true }
        }
        $cacheRoot = [System.IO.Path]::GetFullPath((Join-Path $workspace 'web/scripts/data-preparation/stan/__pycache__')) + [System.IO.Path]::DirectorySeparatorChar
        if ($entry.path -match '^web/scripts/data-preparation/stan/__pycache__/[^/]+\.pyc$' -and $target.StartsWith($cacheRoot, [System.StringComparison]::OrdinalIgnoreCase)) { $valid = $true }
        if (-not $valid) { throw 'Invalid resolved deletion root' }
    }
}
$bytes = ($deletions | Measure-Object bytes -Sum).Sum
Write-Output "Validated dry run: $($moves.Count) moves; $($deletions.Count) deletions; $bytes bytes."
if (-not $Apply) { return }
# Evidence copies must already exist and pass the optional archive verifier's hashes.
foreach ($entry in $manifest.entries | Where-Object action -eq 'preserve') {
    if (-not (Test-Path -LiteralPath (LocalPath $entry.destination))) { throw "Evidence absent: $($entry.destination)" }
}
foreach ($entry in $moves) {
    $source = VerifyOriginal $entry
    $destination = LocalPath $entry.destination
    $null = New-Item -ItemType Directory -Path (Split-Path -Parent $destination) -Force
    Move-Item -LiteralPath $source -Destination $destination
}
# Deletions run separately after archive/evidence checks. This prevents a partially
# successful move from discarding the original evidence needed to diagnose it.
$receipt = [ordered]@{ status = 'archived'; movedFiles = $moves.Count; deletedFiles = 0; deletedBytes = 0 }
$receipt | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $PSScriptRoot 'execution.json') -Encoding utf8
Write-Output 'Archive moves completed. Verify evidence before applying delete.ps1.'

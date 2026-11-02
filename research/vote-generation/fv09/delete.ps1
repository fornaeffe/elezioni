# Invoked only after check.mjs verifies every archived source and retained summary.
param([switch]$Apply)
$ErrorActionPreference = 'Stop'
$workspace = [System.IO.Path]::GetFullPath((Join-Path $PSScriptRoot '../../..'))
$manifest = Get-Content -LiteralPath (Join-Path $PSScriptRoot 'manifest.json') -Raw | ConvertFrom-Json
$receiptPath = Join-Path $PSScriptRoot 'execution.json'
$receipt = Get-Content -LiteralPath $receiptPath -Raw | ConvertFrom-Json
if ($receipt.status -ne 'archived') { throw 'Archive phase must complete first.' }
& node (Join-Path $PSScriptRoot 'check.mjs')
if ($LASTEXITCODE -ne 0) { throw 'Archive verification failed.' }
$selected = @($manifest.entries | Where-Object action -eq 'delete')
foreach ($entry in $selected) {
    $target = [System.IO.Path]::GetFullPath((Join-Path $workspace $entry.path))
    $valid = $false
    foreach ($family in @('generation','drift','party-drift','filing-aware-research','legacy-comparison','current-camera-comparison')) {
        $allowedRoot = [System.IO.Path]::GetFullPath((Join-Path $workspace "data/derived/$family")) + [System.IO.Path]::DirectorySeparatorChar
        if ($target.StartsWith($allowedRoot, [System.StringComparison]::OrdinalIgnoreCase)) { $valid = $true }
    }
    $cacheRoot = [System.IO.Path]::GetFullPath((Join-Path $workspace 'web/scripts/data-preparation/stan/__pycache__')) + [System.IO.Path]::DirectorySeparatorChar
    if ($entry.path -match '^web/scripts/data-preparation/stan/__pycache__/[^/]+\.pyc$' -and $target.StartsWith($cacheRoot, [System.StringComparison]::OrdinalIgnoreCase)) { $valid = $true }
    if (-not $valid) { throw 'Outside intended resolved research directory' }
    $item = Get-Item -LiteralPath $target
    if ($item.PSIsContainer -or $item.LinkType -or $item.Length -ne $entry.bytes -or (Get-FileHash -LiteralPath $target -Algorithm SHA256).Hash.ToLowerInvariant() -ne $entry.sha256) { throw "Identity changed: $target" }
}
Write-Output "Validated $($selected.Count) exact file deletions."
if (-not $Apply) { return }
foreach ($entry in $selected) {
    # Already resolved and checked above; use the same native shell end-to-end.
    Remove-Item -LiteralPath ([System.IO.Path]::GetFullPath((Join-Path $workspace $entry.path)))
    $receipt.deletedFiles++
    $receipt.deletedBytes += $entry.bytes
    $receipt | ConvertTo-Json | Set-Content -LiteralPath $receiptPath -Encoding utf8
}
$receipt.status = 'executed'
$receipt | ConvertTo-Json | Set-Content -LiteralPath $receiptPath -Encoding utf8
$manifest.status = 'executed'
$manifest | ConvertTo-Json -Depth 32 | Set-Content -LiteralPath (Join-Path $PSScriptRoot 'manifest.json') -Encoding utf8
Write-Output "Deleted $($receipt.deletedFiles) files / $($receipt.deletedBytes) bytes."

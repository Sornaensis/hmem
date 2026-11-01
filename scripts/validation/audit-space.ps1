param([ValidateSet("preflight","signal-preflight","qualify")][string]$Mode = "preflight")
$ErrorActionPreference = "Stop"
$repo = "D:\Projects\hmem"
$owned = Join-Path $repo ".tmp\audit-qualification-df58e815"
if ((Get-Location).Path -ne $repo) { throw "exact repository working directory required" }
New-Item -ItemType Directory -Force -Path $owned | Out-Null
$binary = Join-Path $owned "audit-space.exe"
$coreDepends = (& ghc-pkg field hmem-core depends --simple-output) -split '\s+'
$hasqlUnit = @($coreDepends | Where-Object { $_ -like 'hasql-1.9.*' })
if ($hasqlUnit.Count -ne 1) { throw "expected one registered Hasql dependency" }
& ghc -fforce-recomp -threaded -rtsopts -O0 -package hmem-core -package process -package directory -package filepath -package aeson -package bytestring -package text -package-id $hasqlUnit[0] -package resource-pool -package crypton -package time -package uuid -package async -package http-client -package http-types -package websockets -package network -liphlpapi -outputdir $owned scripts/validation/AuditSpace.hs -luser32 -o $binary
if ($LASTEXITCODE -ne 0) { throw "qualification runner compilation failed" }
& $binary "--$Mode" $owned
if ($LASTEXITCODE -ne 0) { throw "qualification $Mode failed: $LASTEXITCODE" }


param (
  [parameter(mandatory = $false)][Arch]$Arch,
  [parameter(mandatory = $false)][switch]$NoPATH,
  [parameter(mandatory = $false)][string]$NwjsPath,
  [parameter(mandatory = $false)][switch]$WhatIf,
  [parameter(mandatory = $false)][string]$DownloadPath
)


Add-Type -TypeDefinition @"
   public enum Arch
   { x64
   , x86
   }
"@




Add-Type -AssemblyName System.IO.Compression.FileSystem
function Unzip
{
    param([string]$zipfile, [string]$outpath)

    [System.IO.Compression.ZipFile]::ExtractToDirectory($zipfile, $outpath)
}
function Push-Env($path) {
  [Environment]::SetEnvironmentVariable
     ("Path", $env:Path + ";$path", [System.EnvironmentVariableTarget]::Machine)
}

function Log($what){
  Write-Output "[haskell-editor-setup]: $what"
}


if($WhatIf){
  Log "WhatIf mode activated. No commands will be executed"
}
if([string]::IsNullOrEmpty($NwjsPath)){
  $NwjsPath = "$env:APPDATA\NWJS\"
}
if([string]::IsNullOrEmpty($DownloadPath)){
  $DownloadPath=$env:TEMP
}

# install nwjs
# add it to path if !$NoPATH

if($Arch -eq [Arch]::x64){
  $nwjs = "https://dl.nwjs.io/v0.46.2/nwjs-sdk-v0.46.2-win-x64.zip"
} else {
  $nwjs = "https://dl.nwjs.io/v0.46.2/nwjs-sdk-v0.46.2-win-ia32.zip"
}
Log "downloading nwjs..."
if(!($WhatIf)){
  curl -o "$DownloadPath\nwjs.zip" $nwjs
}
Log "nwjs downloaded in $DownloadPath"
Log "unzipping nwjs"
if(!($WhatIf)){
  Unzip "$DownloadPath\nwjs.zip" $NwjsPath 
} 
Log "nwjs unzipped in $NwjsPath"
if(!($NoPATH)){
  if(!($WhatIf)){
    $ActualNwjsPath = (gci $NwjsPath)[0]
    Log "$ActualNwjsPath will be added to PATH"
    Push-Env $ActualNwjsPath
  }
} else {
  Log "PATH will not be modified"
}
# install choco
Log "installing choco..."
if(!($WhatIf)){
  Set-ExecutionPolicy Bypass -Scope Process -Force
  [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; 
  iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1')) 
}
Log "choco installed"

# install ghc
Log "installing ghc 8.6.5 through choco..."
if(!($WhatIf)){
  choco install ghc --version=8.6.5
}
Log "ghc 8.6.5 installed"

# install cabal
Log "installing cabal 3.0.0.0 through choco..."
if(!($WhatIf)){
  choco install cabal --version=3.0.0.0
}
Log "cabal 3.0.0.0 installed"
# check that cabal version is 3.0.0.0
Log "printing cabal version..."
if(!($WhatIf)){
  cabal --version 
}
Log "setup done!"

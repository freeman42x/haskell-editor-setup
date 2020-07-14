### Build instructions

Editor setup:

Install in your editor any required [editorconfig](https://editorconfig.org/) extension so that we will have consistent coding style practices.

## Linux

Environment setup:

1. Follow [Instructions for setting up Haskell Editors / IDE](https://github.com/fairy-tale-agi-solutions/haskell-editor-setup/blob/master/README.md#instructions-for-setting-up-haskell-editorside)
2. 💯 VERY IMPORTANT - Configure Nix to use the binary caches for this project
    * Install the nix package `cachix`
    * Run: `sudo cachix use fairy-tale-agi-solutions` and follow the instructions
3. ⛔ OPTIONAL - Install ONLY if you know what you are doing:
    * [lorri](https://github.com/target/lorri)
    * [direnv](https://github.com/direnv/direnv)

Project setup:

* Run `./setup` to setup the project
* Run `nix-shell` and in it run `./hes` to start the application

## Windows

1. Download the NWJS Windows 64-bit SDK from https://nwjs.io/downloads/
2. Extract the above archive
3. Add to your system environment PATH variable the archive folder containing `nw.exe`
4. Install Chocolatey: https://chocolatey.org/install
5. Install GHC 8.6.5 with Chocolatey: `choco install ghc --version=8.6.5`
6. Install Cabal 3.0.0.0 using Chocolatey: `choco install cabal --version=3.0.0.0`
7. Double check that you have `cabal 3.0.0.0` installed using `cabal --version`
8. Run `cabal run` to build and run the project

## Getting HIE to work

0. Following steps are not required if you are patient enough for the editor to do the bellow for you on start
1. Run `hie` in the project root, it will install `cabal 3.0.0.0` and then error out (this is fine)
2. Open the project using your favorite HIE powered editor and everything should just work

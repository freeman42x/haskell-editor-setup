### Build instructions

## Linux

Environment setup:

1. Follow [Instructions for setting up Haskell Editors / IDE](https://github.com/fairy-tale-agi-solutions/haskell-editor-setup/blob/master/README.md#instructions-for-setting-up-haskell-editorside)
2. 💯 VERY IMPORTANT - Configure Nix to use the binary caches for this project
    * Install the nix package `cachix`
    * Run: `cachix use fairy-tale-agi-solutions` to set 
2. ⛔ OPTIONAL - Install ONLY if you know what you are doing:
    * [lorri](https://github.com/target/lorri)
    * [direnv](https://github.com/direnv/direnv)

Project setup:

0. install the Nix package `nwjs-sdk`
1. git clone this repository
2. git clone https://github.com/fairy-tale-agi-solutions/miso in same folder as the git clone from previous step (HES folder and Miso folder should be in same folder)
3. git checkout `fix` branch of Miso repository
4. `cd` to haskell editor setup folder
5. run `nix-shell`
6. run `cabal update`
7. run `cabal install --dependencies only`
8. run `cabal run` to start the GUI

## Windows

Project setup:

0. Install nwjs sdk for your specific Windows version: https://nwjs.io/downloads/
1. Follow [Instructions for setting up Haskell Editors / IDE](https://github.com/fairy-tale-agi-solutions/haskell-editor-setup#windows)
2. git clone this repository
3. git clone https://github.com/fairy-tale-agi-solutions/miso in same folder as the git clone from previous step (HES folder and Miso folder should be in same folder)
4. git checkout `fix` branch of Miso repository
5. `cd` to haskell editor setup folder
6. run `cabal update`
7. run `cabal install --dependencies only`
8. run `cabal run` to start the GUI
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

## Getting HIE to work

1. Run `hie` in the project root, it will install `cabal 3.0.0.0` and then error out (this is fine)
2. Open the project using you favorite HIE powered editor and everything should just work
# Haskell editor / IDE setup

Instructions for setting up Haskell editors/IDEs

Steps:

* Install Nix on your operating system
* Install GHC and cabal-install
* Install the editor / IDE of your choice
* Install Haskell IDE Engine executable
* Install the extensions for your editor / IDE that help with Haskell development

## Install Nix on your operating system

[Nix](https://nixos.org/nix/) is a package manager that is very good at doing successful installs.

If you require any system level libraries to use in the project you are working on it is recommended to use Nix to provide them and then do your development inside a `nix-shell`.

Doing Haskell development on Windows is not recommended since many Haskell packages have issues building on Windows. For doing development while using Widnows as your OS the recommended options are to use either a VMWare virtual machine (preferred) or Windows Subsystem for Linux (unrecommended since the build times are much slower).

Developing on Linux or MacOS should work fine.

To install Nix in your OS run:

```shell
curl https://nixos.org/nix/install | sh
```

## Install GHC and cabal-install

Create the file `~/.nixpkgs/config.nix` and copy paste this into it:

```nix
let
  config = {
    allowUnfree = true;

    packageOverrides = pkgs: with pkgs;
      let jdk = openjdk11; in rec {
      unstable = import <nixpkgs> { inherit config; };

      all = pkgs.buildEnv {
        name = "all";

        paths = [
          haskell.compiler.ghc864
          haskellPackages.cabal-install
          binutils.bintools # required on WSL
        ];
      };
    };
  };
in config
```

And run following command to install the `GHC` and `cabal-install` packages:

```shell
nix-env -i all
```

## Install the editor / IDE of your choice

Recommended editors for beginners Atom or Visual Studio code.

* Find your editor / IDE Nix package using [https://nixos.org/nixos/packages.html](https://nixos.org/nixos/packages.html)
* add the package to the packages list inside `~/.nixpkgs/config.nix`
* run `nix-env -i all` to install it

## Install Haskell IDE Engine executable

In `~/.nixpkgs/config.nix` add to the `let` variables:

 ```nix
all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
```

and add following to the list of packages:

 ```nix
(all-hies.selection { selector = p: { inherit (p) ghc864; }; })
```

then run `nix-env -i all` to install it.

## Install the extensions for your editor / IDE that help with Haskell development

### Atom

* nix
* language-haskell
* atom-ide-ui
* ide-haskell-hie
* ide-haskell-repl
* autocomplete-haskell
* hasklig
* ide-haskell-hoogle
* ide-haskell-cabal
* ide-haskell-hasktags

### Visual Studio Code (VSCode)

* Nix
* Haskell Syntax Highlighting
* Haskell Language Server (Haskell IDE Engine)
* haskell-ghcid
* hoogle-vscode
* [Hasklig](https://github.com/i-tu/Hasklig) - ligatures for Haskell code

### Emacs

* Haskell Mode

### Spacemacs

### Neovim

* coc.nvim

### Vim

### Leksah

### Sublime Text

* SublimeHaskell

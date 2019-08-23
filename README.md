# Haskell editor / IDE setup

Instructions for setting up Haskell editors/IDEs

The most stable and full of features plugin for developing Haskell is Haskell IDE Engine.

To see a list of Haskell IDE Engine features see: [HIE features](https://github.com/haskell/haskell-ide-engine#features)

Setup step:

* Install Nix on your operating system
* Install GHC and cabal-install
* Install the editor / IDE of your choice
* Install Haskell IDE Engine executable
* Install the extensions for your editor / IDE that help with Haskell development

For a list of editor / IDE features see: [rainbyte/haskell-ide-chart](https://github.com/rainbyte/haskell-ide-chart)

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

and add following to the list of packages - change the GHC list to the ones you will want to have available (eg. `ghc864` or `ghc864 ghc863 ghc843`):

 ```nix
(all-hies.selection { selector = p: { inherit (p) ghc864; }; })
```

if you wish to install HIE for all GHC versions because you switch between projects with different GHC versions a lot then you can use this instead:

```nix
(all-hies.selection { selector = p: p; })
```

installing all HIE versions will take a long time to install

after adding HIE your configuration should look something like the following:

```nix
let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  config = {
    allowUnfree = true;

    packageOverrides = pkgs: with pkgs;
      let jdk = openjdk11; in rec {
      unstable = import <nixpkgs> { inherit config; };

      all = pkgs.buildEnv {
        name = "all";

        paths = [
          binutils.bintools
          haskell.compiler.ghc864
          haskellPackages.cabal-install
          unstable.haskellPackages.stack
          unstable.haskellPackages.cabal2nix
          haskellPackages.hoogle
          haskellPackages.ghcid
          (all-hies.selection { selector = p: { inherit (p) ghc864; }; })
        ];
      };
    };
  };
in config
```

run `nix-env -i all` to install HIE

## Install the extensions for your editor / IDE that help with Haskell development

### [Atom](https://atom.io/) (setup difficulty trivial)

* nix
* language-haskell
* atom-ide-ui
* ide-haskell-hie (Haskell IDE Engine)
* ide-haskell-repl
* autocomplete-haskell
* hasklig
* ide-haskell-hoogle
* ide-haskell-cabal
* ide-haskell-hasktags

### [Visual Studio Code (VSCode)](https://code.visualstudio.com/) (setup difficulty trivial)

* Nix
* Haskell Syntax Highlighting
* Haskell Language Server (Haskell IDE Engine)
* haskell-ghcid
* hoogle-vscode
* [Hasklig](https://github.com/i-tu/Hasklig) - ligatures for Haskell code

Alternatives to HIE:

* Haskelly
* Haskero

### [Emacs](https://www.gnu.org/software/emacs/) (setup difficulty hard)

* [emacs-lsp/lsp-haskell (Haskell IDE Engine)](https://github.com/emacs-lsp/lsp-haskell)
* [emacs-lsp/lsp-ui](https://github.com/emacs-lsp/lsp-ui)
* [emacs-lsp/lsp-mode](https://github.com/emacs-lsp/lsp-mode)

Alternatives to HIE:

* jyp/dante (intero fork)
* chrisdone/intero
* flycheck/flycheck-haskell
* haskell/haskell-mode
* nominolo/scion
* DanielG/ghc-mod

### [Spacemacs](http://spacemacs.org/)

* emacs-lsp/lsp-haskell (Haskell IDE Engine)
* haskell/haskell-mode

Alternatives to HIE:

* Haskell layer (uses intero)

### [Neovim](https://neovim.io/) (setup difficulty hard)

* [neovimhaskell/haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [neoclide/coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Language-servers#haskell)
* [autozimu/LanguageClient-neovim (Haskell IDE Engine)](https://github.com/haskell/haskell-ide-engine#using-hie-with-vim-or-neovim)

Alternatives to HIE:

* chrisdone/intero
* parsonsmatt/intero-neovim

### [Vim](https://www.vim.org/) (setup difficulty hard)

* neovimhaskell/haskell-vim
* [neoclide/coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Language-servers#haskell)
* [autozimu/LanguageClient-neovim (Haskell IDE Engine)](https://github.com/haskell/haskell-ide-engine#using-hie-with-vim-or-neovim)

Alternatives to HIE:

* vim-syntastic/syntastic

### [Leksah](http://leksah.org/) (setup difficulty hard)

### [IntelliJ IDEA Community](https://www.jetbrains.com/idea/download/) (setup difficulty easy)

* IntelliJ-Haskell (intero)
* HaskForce
* HoogleIt

### [Sublime Text](https://www.sublimetext.com/) (setup difficulty normal)

* LSP (Haskell IDE Engine)

Alternatives to HIE:

* SublimeHaskell
* dariusf/sublime-intero

# Haskell Editor / IDE Setup

## Automated: Haskell Editor Setup GUI program

Work on creating a Haskell Editor Setup GUI program is in process, you can track progress here: [HES Project Kanban](https://github.com/fairy-tale-agi-solutions/haskell-editor-setup/projects/1)

How Haskell Editor Setup will work - One click install:

1. (optional) choose which to install: GHC, cabal, stack, nix, git, hie
2. (optional) choose which editors / IDEs to install
3. (optional) choose which editor / IDE extensions to install
4. Click `Configure` button and your system will be configured to have installed / set up all the things you chose. If all of them are defaults you will get everything recommended: editor, extensions etc.

The goal of this software is mainly to allow easy editor / IDE setup for whoever tries Haskell for the first time.

Usefulness and features of the project

* one click install or pick your own
* customizable settings with persistent settings
* notify via: sounds, toast messages, etc.
* hours saved: in the order of millions
* great UX for new Haskell learners mainly because of f

"Everything that can be automated should be automated" - Future Robot Overlords

### Implementation progress for Atom editor:

| **OS / Distribution** | **Implemented** | **Packaged** | **Published** |
| --- | --- | --- | --- |
| Linux - NixOS | ✔️ | ❌ | ❌ |
| Linux - Linux Mint | ❌ | ❌ | ❌ |
| Linux - Ubuntu | ❌ | ❌ | ❌ |
| Linux - Debian | ✔️ | ❌ | ❌ |
| Linux - Arch | ❌ | ❌ | ❌ |
| MacOS X | ❌ | ❌ | ❌ |
| Windows 10 | ❌ | ❌ | ❌ |

### Donations

You can donate to support work on Haskell Editor Setup using: https://opencollective.com/fairy-tale-agi-solutions

All the incoming and outgoing expenses for the organization can be seen at the page above.

The donation money will be used for funding the payment for the bounties for tickets on this project.

### Bounties

A bounty is a sum of money which will be rewarded on succesful completion of an issue.

The bounty issues are marked with `bounty` label and will contain somewhre in the issue the bounty reward.

After your PR has been merged to claim a bounty reward, submit an expense using the OpenCollective link above and mention:

1. `Haskell Edito Setup Bounty` for expense name
2. issue number and title in the line item description

See [this expense](https://opencollective.com/fairy-tale-agi-solutions/expenses/19020) as an example.

I will start by paying developers the following amounts for their work on issues:

* 5 Euro for first 1 hour task they would work on
* 3 Euro for successive 1 hour task they would work on
* 1.5 Euro per hour for all other cases
* for special cases I can pay bigger bounties, this depends on the person taking the task, the task, etc.

At the beginning the payments will be mainly symbolic for the developed countries yet better than average pay for non-developed ones.

Over time as this project becomes more successful and money starts flowing in through donations we will increase the pay / hour a lot and record the progress here:

| **Begin date** | **Pay: Euro / hour**|
| --- | --- |
|26-05-2020|1.5|
|01-06-2020|?|

## Manual instructions for setting up Haskell editors / IDE

HIE together with compatible plugins usually offers more features than the average Haskell multi-feature plugin + compatible plugins. Some exceptions are: IntelliJ IDEA which has a very good set of features (and HIE does not have an IDEA plugin yet) and Leksah (which also does not have a HIE plugin but offers quite a lot of features itself).

To see a list of Haskell IDE Engine features see: [HIE features](https://github.com/haskell/haskell-ide-engine#features)

For a list of editor / IDE features see: [rainbyte/haskell-ide-chart](https://github.com/rainbyte/haskell-ide-chart)

Choose your operating system and continue the steps required for it:

* [Windows](#Windows)
* Windows using WSL
* Windows using VMWare with Linux virtual machine
* [Linux](#Install-Nix-on-your-operating-system)
* [MacOS](#Install-Nix-on-your-operating-system)

## Windows

### Install Git

* download Git from [here](https://git-scm.com/download/win)
* install it

### Enable Win32 long paths

#### On Windows Professional

* Type <kbd>Windows</kbd> key, type `gpedit.msc` and press <kbd>Enter</kbd>.
* Navigate to `Local Computer Policy > Computer Configuration > Administrative Templates > System > Filesystem`
* Double click on `Enable Win32 long paths`
* Click on `Enabled`
* Click on `OK`
* Close `Local Group Policy Editor`
* Reboot your operating system

#### On Windows Home or Starter

* Open the [registry patch file](https://raw.githubusercontent.com/fairy-tale-agi-solutions/haskell-editor-setup/master/Remove%20260%20Character%20Path%20Limit.reg)
* Right click in the opened registry file and click on `Save as...` and save it with extension `.reg`
* Double click it and choose `Yes`
* Reboot your operating system

### Install the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/)

* download [Haskell Stack installer](https://get.haskellstack.org/stable/windows-x86_64-installer.exe)
* install Haskell Stack through the installer, the default settings should be fine

### Install Haskell IDE Engine

* press <kbd>Win</kbd>+<kbd>E</kbd>
* open the root of one of your drives where you want to clone Haskell IDE Engine
* right click in the folder and click on `Git Bash Here`
* clone Haskell IDE Engine with: `git clone https://github.com/haskell/haskell-ide-engine.git`
* run `cd haskell-ide-engine`
* run `stack ./install.hs hie-8.6.5` asynchronously (go graba cup of coffee, or do something else while it installs, etc. - it will take a while, took  44m 43s for me)
* run `stack ./install.hs data`

### [Continue with installing the editor / IDE of your choice](#Install-the-editor-/-IDE-of-your-choice)

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

Create the file `~/.config/nixpkgs/config.nix` and copy paste this into it:

```nix
with import <nixpkgs> {};

{
  allowUnfree = true;

  packageOverrides = pkgs: rec {
    all = pkgs.buildEnv {
      name = "all";

      paths = [
        haskell.compiler.ghc865
        haskellPackages.cabal-install
        binutils.bintools # required on WSL
      ];
    };
  };
}
```

And run following command to install the `GHC` and `cabal-install` packages:

```shell
nix-env -i all
```

## Install the editor / IDE of your choice

Editors that are easy to set up are Atom, Visual Studio Code, IntelliJ IDEA Community or Sublime Text 3.

Editors / IDEs list and their Nix package name:

* Atom - atom
* Visual Studio Code - vscode
* Emacs - emacs
* Spacemacs - emacs (and then install the spacemacs layer)
* Neovim - neovim
* Vim - vim
* Leksah - [install instructions](https://github.com/leksah/leksah#getting-leksah)
* IntelliJ IDEA Community - jetbrains.idea-community
* Sublime Text 3 - sublime3

To install:

* pick your editor(s) from the list above
* add the editor(s) Nix package name(s) to the packages list inside `~/.nixpkgs/config.nix`
* run `nix-env -i all` to install the editors you chose

Continue with:

* [using Atom, VSCode, Emacs, Spacemacs, Neovim, Vim or Sublime Text 3](#Install-Haskell-IDE-Engine-executable)
* [using IntelliJ IDEA Community](#IntelliJ-IDEA-Community-%28setup-difficulty-easy%29)
* [using Leksah](#Leksah-%28setup-difficulty-hard%29)

## Install Haskell IDE Engine executable

In `~/.config/nixpkgs/config.nix` add to the `let` variables:

 ```nix
all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
```

and add following to the list of packages - change the GHC list to the ones you will want to have available (eg. `ghc865` or `ghc865 ghc864 ghc843`):

 ```nix
(all-hies.selection { selector = p: { inherit (p) ghc865; }; })
```

if you wish to install HIE for all GHC versions because you switch between projects with different GHC versions a lot then you can use this instead:

```nix
(all-hies.selection { selector = p: p; })
```

installing all HIE versions will take a long time to install

after adding HIE your configuration should look something like the following:

```nix
with import <nixpkgs> {};

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
  unstable = import <nixpkgs> { inherit config; };
in
  {
    allowUnfree = true;

    packageOverrides = pkgs: rec {
      all = pkgs.buildEnv {
        name = "all";

        paths = [
          binutils.bintools # required on WSL
          haskell.compiler.ghc865
          haskellPackages.cabal-install
          unstable.haskellPackages.stack
          unstable.haskellPackages.cabal2nix
          haskellPackages.hoogle
          haskellPackages.ghcid
          (all-hies.selection { selector = p: { inherit (p) ghc865; }; })
        ];
      };
    };
  }
```

run `nix-env -i all` to install HIE

## Install the extensions for your editor / IDE that help with Haskell development

### [Atom (setup difficulty trivial)](https://atom.io/)

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

### [Visual Studio Code (VSCode) (setup difficulty trivial)](https://code.visualstudio.com/)

* Nix
* Haskell Syntax Highlighting
* Haskell Language Server (Haskell IDE Engine)
* haskell-ghcid
* hoogle-vscode
* [Hasklig](https://github.com/i-tu/Hasklig) - ligatures for Haskell code

Installing Hasklig fonts on Windows:

* press <kbd>Win</kbd>+<kbd>X</kbd>
* press <kbd>A</kbd> to start cmd as administrator
* run `@"%SystemRoot%\System32\WindowsPowerShell\v1.0\powershell.exe" -NoProfile -InputFormat None -ExecutionPolicy Bypass -Command "iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))" && SET "PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin"` to install the package manager Chocolatey
* run `choco install hasklig` to install the Hasklig fonts on your operating system

Enabling Hasklig fonts:

* open Visual Studio Code
* type <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>P</kbd>
* type `setting`
* open `Preferences: Open Settings (JSON)`
* make sure you have the following in your settings:

  * `"editor.fontFamily": "Hasklig"`
  * `"editor.fontLigatures": true`

Alternatives to HIE:

* Haskelly
* Haskero
* Simple GHC

### [Emacs (setup difficulty hard)](https://www.gnu.org/software/emacs/)

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

### [Neovim (setup difficulty hard)](https://neovim.io/)

* [neovimhaskell/haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [neoclide/coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Language-servers#haskell)
* [autozimu/LanguageClient-neovim (Haskell IDE Engine)](https://github.com/haskell/haskell-ide-engine#using-hie-with-vim-or-neovim)

Alternatives to HIE:

* chrisdone/intero
* parsonsmatt/intero-neovim

### [Vim (setup difficulty hard)](https://www.vim.org/)

* [neovimhaskell/haskell-vim](https://github.com/neovimhaskell/haskell-vim)
* [neoclide/coc.nvim](https://github.com/neoclide/coc.nvim/wiki/Language-servers#haskell)
* [autozimu/LanguageClient-neovim (Haskell IDE Engine)](https://github.com/haskell/haskell-ide-engine#using-hie-with-vim-or-neovim)

Alternatives to HIE:

* vim-syntastic/syntastic
* dense-analysis/ale

### [Leksah (setup difficulty hard)](http://leksah.org/)

### [IntelliJ IDEA Community (setup difficulty easy)](https://www.jetbrains.com/idea/download/)

* IntelliJ-Haskell
* HaskForce
* HoogleIt

### [Sublime Text 3 (setup difficulty normal)](https://www.sublimetext.com/)

* LSP (Haskell IDE Engine)

Alternatives to HIE:

* SublimeHaskell
* dariusf/sublime-intero

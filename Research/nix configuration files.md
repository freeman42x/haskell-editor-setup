# Nix Configuration files

## `nix.conf`

[source](https://nixos.org/nix/manual/#sec-conf-file)

`nix.conf` files are used to configure the behaviour of nix.

There are two files

- `/etc/nix/nix.conf`: contains the system level configuration
- `~/.config/nix/nix.conf`: contains the user level configuration

## `.nix` files

### NixOS global package configuration

[source](https://nixos.org/nixos/manual/index.html#ch-configuration)

The file `~/etc/nixos/configuration.nix` contains the system level packages

### Nix user package configuration

[source](https://nixos.org/nixpkgs/manual/#chap-packageconfig)

The file  `~/.config/nixpkgs/config.nix` contains the user packages

### Home Manager

[source](https://nixos.wiki/wiki/Home_Manager)

The Home Manager is a system for managing the user environment together with the nix package manager.

The file `~/.config/nixpkgs/home.nix` contains the configuration for the Home Manager

### Overlays

[source](https://nixos.wiki/wiki/Overlays)

Overlays are a way to extend and change nixpkgs.

The file `~/.config/nixpkgs/overlays.nix` contains the configuration for Overlays 
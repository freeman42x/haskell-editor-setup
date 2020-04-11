with (import ../miso {});
with pkgs.haskell.packages;
(pkgs.haskell.lib.overrideCabal (ghc865.callCabal2nix "haskell-editor-setup" ./. { miso = ghc865.miso-jsaddle; }) (drv: {
    libraryPkgconfigDepends =
      [ pkgs.webkitgtk
        pkgs.glib-networking
      ];
  })
).overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.wrapGAppsHook ];
    libraryFrameworkDepends = [ pkgs.webkitgtk pkgs.glib-networking ];
  })
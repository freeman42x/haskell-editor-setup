let
  ghcVersion = "ghc865";
  misoPkgs = import (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  }) {};
in

with misoPkgs.pkgs;
with haskell.packages."${ghcVersion}";
let app = callCabal2nix "haskell-editor-setup" ./. { miso = miso-jsaddle; };
in  app.overrideAttrs (old: {
  miso = miso-jsaddle;
  buildInputs = old.buildInputs ++ [ nwjs-sdk ];
})

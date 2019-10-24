with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
with pkgs.haskell.packages;
ghc865.callCabal2nix "haskell-editor-setup" ./. { miso = ghc865.miso-jsaddle; }

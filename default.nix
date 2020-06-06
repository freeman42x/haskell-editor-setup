{ ghcVersion ? "ghc865"
, misoPkgs ? import (builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
    sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
  }) {}
}:

with misoPkgs.pkgs;
with haskell.packages."${ghcVersion}";
let runtimeDeps = [ nwjs ];
    app = callCabal2nix "haskell-editor-setup" ./. { miso = miso-jsaddle; };
in

app.overrideAttrs (old: {
  miso = miso-jsaddle;
  # needs nw in RPATH in order to use propagatedBuildInputs -> useless for runtime
  buildInputs = old.buildInputs ++ [ binutils makeWrapper ];
  postFixup = ''
    mv $out/bin/haskell-editor-setup $out/bin/.haskell-editor-setup-wrapped
    makeWrapper $out/bin/.haskell-editor-setup-wrapped $out/bin/haskell-editor-setup \
                --prefix PATH : ${lib.makeBinPath runtimeDeps}
  '';
})

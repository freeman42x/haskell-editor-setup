let pkgs = import <nixpkgs> { };
in (import ./.).env.overrideAttrs (_: {
  shellHook = ''
    PATH="$PATH:${pkgs.ghc}/bin:${pkgs.cabal-install}/bin:${pkgs.ghcid}/bin:${
      ((import
        (fetchTarball "https://github.com/infinisil/all-hies/tarball/master")
        { }).selection { selector = p: { inherit (p) ghc865; }; })
    }/bin"'';
})
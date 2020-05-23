with import <nixpkgs> { };

vscode-with-extensions.override {
  vscodeExtensions = [
    vscode-extensions.bbenoist.Nix
  ] ++ vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "language-haskell";
      publisher = "justusadam";
      version = "3.2.1";
      sha256 = "0lxp8xz17ciy93nj4lzxqvz71vw1zdyamrnh2n792yair8890rr6";
    }
    {
      name = "vscode-hie-server";
      publisher = "alanz";
      version = "0.0.40";
      sha256 = "1cmlgidjma41s5zq5161gcxxmk5lfzcm8dvznls04y5l7q9b0gca";
    }
    {
      name = "haskell-ghcid";
      publisher = "ndmitchell";
      version = "0.3.1";
      sha256 = "1rivzlk32x7vq84ri426nhd6a4nv3h7zp7xcsq31d0kp8bqczvi9";
    }
    {
      name = "hoogle-vscode";
      publisher = "jcanero";
      version = "0.0.7";
      sha256 = "0ndapfrv3j82792hws7b3zki76m2s1bfh9dss1xjgcal1aqajka1";
    }
  ];
}

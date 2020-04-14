module OS.Linux.Debian where

import           Prelude                        ( IO )
import           Data.Text.IO                   ( putStrLn )
import           Data.Maybe                     ( isNothing )
import           Turtle                         ( home
                                                , which
                                                , shell
                                                , empty
                                                , die
                                                , repr
                                                , testfile
                                                , fromText
                                                , readTextFile
                                                , writeTextFile
                                                , ExitCode(..)
                                                , FilePath
                                                )
import           Util                           ( asUser
                                                , installAtomExtension
                                                )


debianAtom :: IO ()
debianAtom = do
  installNix

  putStrLn $ T.intercalate
    "\n"
    [ "These packages will be added to nix configuration and installed:"
    , "GHC"
    , "cabal-install"
    , "stack"
    , "cabal2nix"
    , "hoogle"
    , "ghcid"
    , "Atom"
    , "HIE"
    ]

  nixConfig <- (<> fromText ".config/nixpkgs/config.nix") <$> home
  unlessM (testfile nixConfig) $ writeTextFile nixConfig "{\n  \n}"

  addToLet
    nixConfig
    "all-hies = import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\") {};"
  addToLet nixConfig "unstable = import <nixpkgs> { inherit config; };"

  importNixpkgs nixConfig

  addToConfig nixConfig "allowUnfree = true;\n\n"
  addToConfig nixConfig $ T.intercalate
    "\n"
    [ "packageOverrides = pkgs: rec {"
    , "  all = pkgs.buildEnv {"
    , "    name = \"all\";"
    , ""
    , "    paths = ["
    , "      haskell.compiler.ghc865"
    , "      haskellPackages.cabal-install"
    , "      unstable.haskellPackages.stack"
    , "      unstable.haskellPackages.cabal2nix"
    , "      haskellPackages.hoogle"
    , "      haskellPackages.ghcid"
    , "      atom"
    , "      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })"
    , "    ];"
    , "  };"
    , "};"
    ]

  shell (asUser "nix-env -i all") empty >>= \case
    ExitSuccess   -> putStrLn "Installation complete"
    ExitFailure n -> die $ "Installation failed with exit code: " <> repr n

  let atomExtensions =
        [ "nix"
        , "atom-ide-ui"
        , "autocomplete-haskell"
        , "hasklig"
        , "ide-haskell-cabal"
        , "ide-haskell-hasktags"
        , "ide-haskell-hie"
        , "ide-haskell-hoogle"
        , "ide-haskell-repl"
        , "language-haskell"
        ]

  putStrLn
    $  "These Atom extensions will be installed:\n"
    <> T.intercalate "\n" atomExtensions

  mapM_ installAtomExtension atomExtensions


installNix :: IO ()
installNix =
  whenM (isNothing <$> which "nix")
    $   putStrLn "Installing nix"
    >>  shell
          (T.intercalate
            " && "
            [ "install -d -m755 -o $(id -u) -g $(id -g) /nix"
            , asUser "curl https://nixos.org/nix/install | sh"
            ]
          )
          empty
    >>= \case
          ExitSuccess -> putStrLn "nix successfully installed"
          ExitFailure n ->
            die $ "nix installation failed with exit code: " <> repr n

importNixpkgs :: FilePath -> IO ()
importNixpkgs path = do
  config <- readTextFile path
  unless ("with import <nixpkgs> {};" `T.isInfixOf` config)
    $  writeTextFile path
    $  "with import <nixpkgs> {};\n\n"
    <> config

addToLet :: FilePath -> T.Text -> IO ()
addToLet path content = do
  config <- readTextFile path
  unless (content `T.isInfixOf` config)
    $ writeTextFile path
    $ if "let" `T.isInfixOf` config
        then T.replace "\nin" ("\n  " <> content <> "\nin") config
        else "let\n  " <> content <> "\nin\n" <> config

addToConfig :: FilePath -> T.Text -> IO ()
addToConfig path content = do
  config <- readTextFile path
  unless (content `T.isInfixOf` config) $ do
    let w = "in\n{\n  "
    writeTextFile path $ T.replace w (w <> content) config

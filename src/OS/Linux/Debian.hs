module OS.Linux.Debian where

import           OS.Common
import qualified Data.Text                     as T
import           Data.Text.IO                   ( putStrLn )
import           Data.Maybe                     ( isNothing )
import           Prelude                        ( IO
                                                , (<$>)
                                                , ($)
                                                , (>>=)
                                                , (<>)
                                                , (>>)
                                                , mapM_
                                                , whenM
                                                , unless
                                                , unlessM
                                                )
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

debianAtom :: IO ()
debianAtom = do
  installNix

  putStrLn
    "These packages will be added to nix configuration and installed:\n\
    \  GHC                                                           \n\
    \  cabal-install                                                 \n\
    \  stack                                                         \n\
    \  cabal2nix                                                     \n\
    \  hoogle                                                        \n\
    \  ghcid                                                         \n\
    \  Atom                                                          \n\
    \  HIE"

  nixConfig <- (<> fromText ".config/nixpkgs/config.nix") <$> home
  unlessM (testfile nixConfig) $ writeTextFile nixConfig "{\n  \n}"

  addToLet
    nixConfig
    "all-hies = import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\") {};"
  addToLet nixConfig "unstable = import <nixpkgs> { inherit config; };"

  importNixpkgs nixConfig

  addToConfig
    nixConfig
    "allowUnfree = true;                                                  \n\
    \                                                                     \n\
    \packageOverrides = pkgs: rec {                                       \n\
    \  all = pkgs.buildEnv {                                              \n\
    \    name = \"all\";                                                  \n\
    \                                                                     \n\
    \    paths = [                                                        \n\
    \      haskell.compiler.ghc865                                        \n\
    \      haskellPackages.cabal-install                                  \n\
    \      unstable.haskellPackages.stack                                 \n\
    \      unstable.haskellPackages.cabal2nix                             \n\
    \      haskellPackages.hoogle                                         \n\
    \      haskellPackages.ghcid                                          \n\
    \      atom                                                           \n\
    \      (all-hies.selection { selector = p: { inherit (p) ghc865; }; })\n\
    \    ];                                                               \n\
    \  };                                                                 \n\
    \};"

  shell (runAsUserPrefix "nix-env -i all") empty >>= \case
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

  mapM_ installAtomPackage atomExtensions


installNix :: IO ()
installNix =
  whenM (isNothing <$> which "nix")
    $   putStrLn "Installing nix"
    >>  shell
          (T.intercalate
            " && "
            [ "install -d -m755 -o $(id -u) -g $(id -g) /nix"
            , runAsUserPrefix "curl https://nixos.org/nix/install | sh"
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
module OS.Linux.Debian where

import           OS.Common
import qualified Data.Text                     as T
import           Data.Text.IO                   ( putStrLn )
import           Data.Maybe                     ( isNothing )
import           NeatInterpolation
import           Prelude                        ( IO
                                                , (<$>)
                                                , ($)
                                                , (>>=)
                                                , (<>)
                                                , (>>)
                                                , mapM_
                                                , whenM
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
                                                , writeTextFile
                                                , ExitCode(..)
                                                )

debianAtom :: IO ()
debianAtom = do
  installNix

  nixConfigFilePath <- (<> fromText ".config/nixpkgs/config.nix") <$> home
  unlessM (testfile nixConfigFilePath) $ writeTextFile nixConfigFilePath configNixContent

  shell (runAsUserPrefix "nix-env -i all") empty >>= \case
    ExitSuccess   -> putStrLn "Installation complete"
    ExitFailure n -> die $ "Installation failed with exit code: " <> repr n

  let atomPackages =
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

  putStrLn $ T.unlines $ "These Atom extensions will be installed:" : atomPackages
  mapM_ installAtomPackage atomPackages


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

configNixContent :: T.Text
configNixContent =
  [text|
    with import <nixpkgs> {};

    {
      allowUnfree = true;

      packageOverrides = pkgs: rec {
        all = pkgs.buildEnv {
          name = "all";

          paths = [
            haskell.compiler.ghc865
            haskellPackages.cabal-install
            ((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\") {}).selection { selector = p: { inherit (p) ghc865; }; })
            haskellPackages.hoogle
            atom
          ];
        };
      };
    }
  |]
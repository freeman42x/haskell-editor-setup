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

  nixConfigFilePath <- (<> fromText ".config/nixpkgs/config.nix") <$> home
  unlessM (testfile nixConfigFilePath) $ writeTextFile nixConfigFilePath "{\n  \n}"

  addToConfig
    nixConfigFilePath
    "allowUnfree = true;                                                  \n\
    \                                                                     \n\
    \packageOverrides = pkgs: rec {                                       \n\
    \  all = pkgs.buildEnv {                                              \n\
    \    name = \"all\";                                                  \n\
    \                                                                     \n\
    \    paths = [                                                        \n\
    \      haskell.compiler.ghc865                                        \n\
    \      haskellPackages.cabal-install                                  \n\
    \      haskellPackages.hoogle                                         \n\
    \      atom                                                           \n\
    \      ((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\") {}).selection { selector = p: { inherit (p) ghc865; }; })\n\
    \    ];                                                               \n\
    \  };                                                                 \n\
    \};"
    
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

addToConfig :: FilePath -> T.Text -> IO ()
addToConfig path content = do
  config <- readTextFile path
  unless (content `T.isInfixOf` config) $ do
    let w = "in\n{\n  "
    writeTextFile path $ T.replace w (w <> content) config
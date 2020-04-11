module OS.Linux.NixOS where

import           Prelude                        ( IO
                                                , String
                                                , foldl
                                                , return
                                                , ($)
                                                , (<>)
                                                )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( putStrLn
                                                , readFile
                                                , writeFile
                                                )
import           Turtle                         ( shell
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )


nixOsAtom :: IO ()
nixOsAtom = do
  putStrLnGreen "Adding Haskell GHC and cabal-install to configuration.nix"
  config <- readFile configurationNix
  let newConfig = foldl
        addToConfigurationIfDoesNotExist
        config
        ["haskell.compiler.ghc865", "haskellPackages.cabal-install", "atom"]
  writeFile configurationNix newConfig
  putStrLnGreen
    "Finished adding Haskell GHC and cabal-install to configuration.nix"

  putStrLnGreen "Installing GHC, cabal-install and Atom"
  exitCode <- shell "nixos-rebuild switch" empty
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure n ->
      die ("nixos-rebuild switch failed with exit code: " <> repr n)
  putStrLnGreen "Finished installing GHC, cabal-install and Atom"

  putStrLnGreen "Adding Haskell IDE Engine to configuration.nix"
  config2 <- readFile configurationNix
  let
    newConfig2 = addToConfigurationIfDoesNotExist
      config2
      "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
          \ {}).selection { selector = p: { inherit (p) ghc865 ghc864; }; })"
  writeFile configurationNix newConfig2
  putStrLnGreen "Finished adding Haskell IDE Engine to configuration.nix"

  putStrLnGreen "Installing Haskell IDE Engine"
  exitCode2 <- shell "nixos-rebuild switch" empty
  case exitCode2 of
    ExitSuccess -> return ()
    ExitFailure n ->
      die ("nixos-rebuild switch failed with exit code: " <> repr n)
  putStrLnGreen "Finished installing Haskell IDE Engine"

  installAtomPackage "nix"
  installAtomPackage "atom-ide-ui"
  installAtomPackage "autocomplete-haskell"
  installAtomPackage "hasklig"
  installAtomPackage "ide-haskell-cabal"
  installAtomPackage "ide-haskell-hasktags"
  installAtomPackage "ide-haskell-hie"
  installAtomPackage "ide-haskell-hoogle"
  installAtomPackage "ide-haskell-repl"
  installAtomPackage "language-haskell"


putStrLnGreen :: T.Text -> IO ()
putStrLnGreen str = putStrLn $ "\x1b[32m" <> str <> "\x1b[0m"

configurationNix :: String
configurationNix = "/etc/nixos/configuration.nix"

environmentSystemPackages :: T.Text
environmentSystemPackages = "environment.systemPackages = with pkgs; ["

addToConfigurationIfDoesNotExist :: T.Text -> T.Text -> T.Text
addToConfigurationIfDoesNotExist configNix package = if isPackageInstalled
  then configNix
  else T.replace
    environmentSystemPackages
    (environmentSystemPackages <> "\n\
         \    " <> package)
    configNix
  where isPackageInstalled = package `T.isInfixOf` configNix

installAtomPackage :: T.Text -> IO ()
installAtomPackage package = do
  putStrLnGreen $ "Installing " <> package
  exitCode <- shell ("sudo -u $SUDO_USER apm install " <> package) empty
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("apm install failed with exit code: " <> repr n)
  putStrLnGreen $ "Finished installing " <> package

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Maybe (isNothing, Maybe(..))
import Data.Text (Text, replace, isInfixOf)
import Data.Text.IO (putStrLn, readFile, writeFile)
import Prelude(IO, String, foldl, return, ($), (<>))
import System.Directory (findExecutable)
import Turtle (shell, empty, die, repr, ExitCode(..))

main :: IO ()
main = do
  maybeFilePath <- findExecutable "nixos-version"
  putStrLn $ case maybeFilePath of
    Just _ -> "NixOS operating system detected"
    _      -> "NixOS operating system not found"
  unless (isNothing maybeFilePath) $ do
    putStrLn "Adding Haskell GHC and cabal-install to configuration.nix"
    config <- readFile configurationNix
    let newConfig = foldl addToConfigurationIfDoesNotExist config ["haskell.compiler.ghc865", "haskellPackages.cabal-install", "atom"]
    writeFile configurationNix newConfig
    putStrLn "Finished adding Haskell GHC and cabal-install to configuration.nix"

    putStrLn "Installing GHC, cabal-install and Atom"
    exitCode <- shell "nixos-rebuild switch" empty
    case exitCode of
        ExitSuccess   -> return ()
        ExitFailure n -> die ("nixos-rebuild switch failed with exit code: " <> repr n)
    putStrLn "Finished installing GHC, cabal-install and Atom"

    putStrLn "Adding Haskell IDE Engine to configuration.nix"
    config2 <- readFile configurationNix
    let newConfig2 =
          addToConfigurationIfDoesNotExist
            config2
            "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
            \ {}).selection { selector = p: { inherit (p) ghc865 ghc864; }; })"
    writeFile configurationNix newConfig2
    putStrLn "Finished adding Haskell IDE Engine to configuration.nix"

    putStrLn "Installing Haskell IDE Engine"
    exitCode2 <- shell "nixos-rebuild switch" empty
    case exitCode2 of
        ExitSuccess   -> return ()
        ExitFailure n -> die ("nixos-rebuild switch failed with exit code: " <> repr n)
    putStrLn "Finished installing Haskell IDE Engine"



configurationNix :: String
configurationNix = "/etc/nixos/configuration.nix"

environmentSystemPackages :: Text
environmentSystemPackages = "environment.systemPackages = with pkgs; ["

addToConfigurationIfDoesNotExist :: Text -> Text -> Text
addToConfigurationIfDoesNotExist configNix package =
  if isPackageInstalled then configNix else
       replace
         environmentSystemPackages
         (environmentSystemPackages <> "\n\
         \    " <> package)
         configNix
  where
    isPackageInstalled = package `isInfixOf` configNix

-- TODO install the Atom extensions if not already installed

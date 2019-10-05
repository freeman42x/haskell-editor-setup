{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Maybe (isNothing, Maybe(..))
import Data.Text (Text, replace, isInfixOf)
import Data.Text.IO (putStrLn, readFile, writeFile)
import Prelude(IO, String, foldl, ($), (<>))
import System.Directory (findExecutable)

main :: IO ()
main = do
  maybeFilePath <- findExecutable "nixos-version"
  putStrLn $ case maybeFilePath of
    Just _ -> "NixOS operating system detected"
    _      -> "NixOS operating system not found"
  unless (isNothing maybeFilePath) $ do
    putStrLn "Installing Haskell GHC and cabal-install"
    config <- readFile configurationNix
    let newConfig = foldl addToConfigurationIfDoesNotExist config ["haskell.compiler.ghc865", "haskellPackages.cabal-install", "atom"]
    writeFile configurationNix newConfig

    -- putStrLn "Installing GHC, cabal-install and Atom"
    -- exitCode <- shell "nixos-rebuild switch" empty
    -- case exitCode of
    --     ExitSuccess   -> return ()
    --     ExitFailure n -> die ("`direnv allow` failed with exit code: " <> repr n)
    -- putStrLn "Finished installing GHC, cabal-install and Atom"

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

-- TODO install GHC, cabal-install, Atom if not already installed

-- TODO install HIE if not already installed

-- TODO install the Atom extensions if not already installed

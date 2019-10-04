{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Maybe (isNothing, Maybe(..))
import Data.Text (Text, replace, isInfixOf)
import Data.Text.IO (putStrLn, readFile, writeFile)
import Prelude(IO, String, ($), (<>))
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

    -- TODO how do I make a monoid instance for this? vvv
    let configurationNix2 = addToConfigurationIfDoesNotExist config "haskell.compiler.ghc865"
    let configurationNix3 = addToConfigurationIfDoesNotExist configurationNix2 "haskellPackages.cabal-install"
    let configurationNix4 = addToConfigurationIfDoesNotExist configurationNix3 "atom"

    writeFile configurationNix configurationNix4

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

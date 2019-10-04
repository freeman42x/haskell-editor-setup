{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.List (isInfixOf)
import Data.Maybe (isNothing, Maybe(..))
import Data.Text (pack, replace)
import Data.Text.IO (putStrLn)
import Prelude(IO, ($))
import System.Directory (findExecutable)
import System.IO (readFile, writeFile)

main :: IO ()
main = do
  maybeFilePath <- findExecutable "nixos-version"
  putStrLn $ case maybeFilePath of
    Just _ -> "NixOS operating system detected"
    _      -> "NixOS operating system not found"
  unless (isNothing maybeFilePath) $ do
    putStrLn "Installing Haskell GHC and cabal-install"
    configurationNix <- readFile "/etc/nixos/configuration.nix"
    let isGhcInstalled = "haskell.compiler.ghc865" `isInfixOf` configurationNix
    let configurationNixWithGhc = if isGhcInstalled then pack configurationNix else
         replace
           "environment.systemPackages = with pkgs; ["
           "environment.systemPackages = with pkgs; [\n\
           \    haskell.compiler.ghc865"
           (pack configurationNix)
    -- let newConfigurationNix =
    --      replace
    --        "environment.systemPackages = with pkgs; ["
    --        "environment.systemPackages = with pkgs; [\n\
    --        \    haskell.compiler.ghc865\n\
    --        \    haskellPackages.cabal-install\n\
    --        \    atom"
    --        (pack configurationNix)
    putStrLn configurationNixWithGhc

-- TODO install GHC, cabal-install, Atom if not already installed

-- TODO install HIE if not already installed

-- TODO install the Atom extensions if not already installed

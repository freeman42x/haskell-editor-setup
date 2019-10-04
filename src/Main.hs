module Main where

import Control.Monad (unless)
import Data.Maybe (isNothing)
import System.Directory (findExecutable)

main :: IO ()
main = do
  maybeFilePath <- findExecutable "nixos-version"
  putStrLn $ case maybeFilePath of
    Just _ -> "NixOS operating system detected"
    _      -> "NixOS operating system not found"
  unless (isNothing maybeFilePath) $ do
    putStrLn "Installing Haskell GHC and cabal-install"

-- TODO install GHC and cabal-install if not already installed

-- TODO install Atom if not already installed

-- TODO install HIE if not already installed

-- TODO install the Atom extensions if not already installed

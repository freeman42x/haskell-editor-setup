module OS.Prerequisites
  ( NixConfiguration (..)
  , isGhcInstalled
  , isCabalInstalled
  , isStackInstalled
  , getExistingNixConfigurations
  ) where

import qualified Relude.Unsafe as RU
import System.Directory (findExecutable, doesFileExist, getHomeDirectory)
import Data.Maybe (isJust)
import Data.List (isPrefixOf)
import Control.Monad (filterM)

isExecutableInstalled :: String -> IO Bool
isExecutableInstalled name = isJust <$> findExecutable name

isGhcInstalled :: IO Bool
isGhcInstalled = isExecutableInstalled "ghc"

isCabalInstalled :: IO Bool
isCabalInstalled = isExecutableInstalled "cabal"

isStackInstalled :: IO Bool
isStackInstalled = isExecutableInstalled "stack"

-- see Research/nix configuration files.md
data NixConfiguration
  = System
  | User
  | Nixos
  | Packages
  | HomeManager
  | Overlays
  deriving (Show)

doesFileExist' :: FilePath -> IO Bool
doesFileExist' path
  | "~" `isPrefixOf` path = do
    homepath <- getHomeDirectory
    doesFileExist $ homepath ++ pathTail
  | otherwise = doesFileExist path
  where
    pathTail = tail $ RU.fromJust $ nonEmpty path -- TODO

getExistingNixConfigurations :: IO [NixConfiguration]
getExistingNixConfigurations = map fst
  <$> filterM (\(_, f) -> doesFileExist' f)
    [ (System,      "/etc/nix/nix.conf")
    , (User,        "~/.config/nix/nix.conf")
    , (Nixos,       "/etc/nixos/configuration.nix")
    , (Packages,    "~/.config/nixpkgs/config.nix")
    , (HomeManager, "~/.config/nixpkgs/home.nix")
    , (Overlays,    "~/.config/nixpkgs/overlays.nix")]
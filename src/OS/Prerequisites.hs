module OS.Prerequisites
  ( NixConfiguration (..)
  , isGhcInstalled
  , isCabalInstalled
  , isStackInstalled
  , getExistingNixConfigurations
  ) where

import System.Directory (findExecutable, doesFileExist, getHomeDirectory)
import Data.Maybe (isJust)
import Data.List (isPrefixOf)
import Control.Monad (filterM)

isGhcInstalled = fmap isJust $ findExecutable "ghc"

isCabalInstalled = fmap isJust $ findExecutable "cabal"

isStackInstalled = fmap isJust $ findExecutable "stack"

data NixConfiguration
  = System    -- system nix config
  | User      -- user nix config
  | Nixos     -- nixos config
  | Packages  -- nix packages config
  | Home      -- optional home.nix
  deriving (Show)

doesFileExist' :: FilePath -> IO Bool
doesFileExist' path
  | "~" `isPrefixOf` path = do
    homepath <- getHomeDirectory
    doesFileExist $ homepath ++ tail path
  | otherwise = doesFileExist path

getExistingNixConfigurations :: IO [NixConfiguration]
getExistingNixConfigurations = fmap (map fst)
  $ filterM (\(t, f) -> doesFileExist' f) [ (System,   "/etc/nix/nix.conf"),
                                            (User,     "~/.config/nix/nix.conf"),
                                            (Nixos,    "/etc/nixos/configuration.nix"),
                                            (Packages, "~/.config/nixpkgs/config.nix"),
                                            (Home,     "~/.config/nixpkgs/home.nix")  ]

module OS.Common where

import           Data.Maybe                     ( isJust )
import           Data.List                      ( isPrefixOf )
import           Data.Text.IO
import           Development.Placeholders
import           Control.Monad                  ( filterM )
import           Prelude                 hiding ( die, putStrLn )
import qualified Relude.Unsafe                 as RU
import           System.Directory               ( findExecutable
                                                , doesFileExist
                                                , getHomeDirectory
                                                )
import           Turtle                         ( shell
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )

isExecutableInstalled :: String -> IO Bool
isExecutableInstalled name = isJust <$> findExecutable name

isGhcInstalled :: IO Bool
isGhcInstalled = isExecutableInstalled "ghc"

isCabalInstalled :: IO Bool
isCabalInstalled = isExecutableInstalled "cabal"

isStackInstalled :: IO Bool
isStackInstalled = isExecutableInstalled "stack"

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
    doesFileExist $ homepath ++ RU.tail path
  | otherwise = doesFileExist path

getExistingNixConfigurations :: IO [NixConfiguration]
getExistingNixConfigurations = map fst <$> filterM
  (\(_, f) -> doesFileExist' f)
  [ (System     , "/etc/nix/nix.conf")
  , (User       , "~/.config/nix/nix.conf")
  , (Nixos      , "/etc/nixos/configuration.nix")
  , (Packages   , "~/.config/nixpkgs/config.nix")
  , (HomeManager, "~/.config/nixpkgs/home.nix")
  , (Overlays   , "~/.config/nixpkgs/overlays.nix")
  ]

-- runShellCommand :: Text -> IO Text
-- runShellCommand command = sh $ do
--   out <- inshellWithErr command empty
--   lineToText $ bifold out

isAtomPackageInstalled :: Text -> Bool
isAtomPackageInstalled _name = $notImplemented
  -- run apm list
  -- project it to structured package data
  -- check if the package is in the list

  -- runAsUserPrefix "apm list --installed --bare"


  -- runShellCommand ("sudo -u $SUDO_USER apm install --color false " <> package)

  -- [neo@nixos:~]$ apm list --installed --bare
  -- atom-ide-ui@0.13.0
  -- autocomplete-haskell@1.0.1
  -- hasklig@0.4.0
  -- ide-haskell@2.4.1
  -- ide-haskell-cabal@2.5.0
  -- ide-haskell-hasktags@0.0.17
  -- ide-haskell-hie@0.12.0
  -- ide-haskell-hoogle@0.1.2
  -- ide-haskell-repl@0.9.5
  -- language-haskell@1.19.4
  -- nix@2.1.0
  -- todo-show@2.3.2

runAsUserPrefix :: Text -> Text
runAsUserPrefix cmd = "sudo -u $SUDO_USER " <> cmd

installAtomExtension :: Text -> IO ()
installAtomExtension extension = do
  putStrLn $ "Installing " <> extension <> " Atom extension"
  shell (runAsUserPrefix $ "apt install " <> extension) empty >>= \case
    ExitSuccess -> putStrLn $ extension <> " successfully installed"
    ExitFailure n ->
      die $ extension <> " installation failed with exit code: " <> repr n
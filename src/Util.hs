module Util where

import           Data.Bifoldable                ( bifold )
import           Data.Text                      ( Text )
import           Data.Text.IO                   ( putStrLn )
import           Development.Placeholders
import           Prelude                        ( (<>)
                                                , (>>=)
                                                , ($)
                                                , Bool
                                                , IO
                                                , pure
                                                )
import           Turtle                         ( inshellWithErr
                                                , sh
                                                , shell
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )
import           Turtle.Line                    ( lineToText )



-- runShellCommand :: Text -> IO Text
-- runShellCommand command = sh $ do
--   out <- inshellWithErr command empty
--   lineToText $ bifold out

isAtomPackageInstalled :: Text -> Bool
isAtomPackageInstalled name = do
  -- run apm list
  -- project it to structured package data
  -- check if the package is in the list

  -- runAsUserPrefix "apm list --installed --bare"


  $notImplemented


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
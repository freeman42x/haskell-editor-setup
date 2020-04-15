module Util where

import           Data.Text                      ( Text )
import           Data.Text.IO                   ( putStrLn )
import           Development.Placeholders
import           Prelude                        ( (<>)
                                                , (>>=)
                                                , ($)
                                                , Bool
                                                , IO
                                                )
import           Turtle                         ( shell
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )



isAtomPackageInstalled :: Text -> Bool
isAtomPackageInstalled name = $notImplemented

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

asUser :: Text -> Text
asUser cmd = "su - $SUDO_USER -c '" <> cmd <> "'"

installAtomExtension :: Text -> IO ()
installAtomExtension extension = do
  putStrLn $ "Installing " <> extension <> " Atom extension"
  shell (asUser $ "apt install " <> extension) empty >>= \case
    ExitSuccess -> putStrLn $ extension <> " successfully installed"
    ExitFailure n ->
      die $ extension <> " installation failed with exit code: " <> repr n
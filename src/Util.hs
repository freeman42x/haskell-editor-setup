module Util where

import           Prelude                        ( (<>)
                                                , (>>=)
                                                , ($)
                                                , IO
                                                )
import           Turtle                         ( shell
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )
import qualified Data.Text                     as T
import           Data.Text.IO                   ( putStrLn )


asUser :: T.Text -> T.Text
asUser cmd = "su - $SUDO_USER -c '" <> cmd <> "'"

installAtomExtension :: T.Text -> IO ()
installAtomExtension extension = do
  putStrLn $ "Installing " <> extension <> " Atom extension"
  shell (asUser $ "apt install " <> extension) empty >>= \case
    ExitSuccess -> putStrLn $ extension <> " successfully installed"
    ExitFailure n ->
      die $ extension <> " installation failed with exit code: " <> repr n
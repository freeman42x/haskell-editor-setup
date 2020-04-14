module OS.Linux.NixOS where

import           Control.Monad.IO.Class         ( liftIO )
import           Prelude                        ( IO
                                                , String
                                                , Text
                                                , return
                                                , when
                                                , ($)
                                                , (<>)
                                                , (/=)
                                                )
import           Data.Bifoldable                ( bifold )
import           Data.Text                      ( replace
                                                , isInfixOf)
import           Data.Text.IO                   ( putStrLn
                                                , readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString )
import           Turtle                         ( shell
                                                , sh
                                                , inshellWithErr
                                                , empty
                                                , die
                                                , repr
                                                , ExitCode(..)
                                                )
import           Turtle.Line                    ( lineToText )

import           Types

nixOsAtom :: Sink Action -> IO ()
nixOsAtom sink = do
  let log text = sink $ Append (text <> "\n")
      logStep text actions = do
        log $ "BEGIN : " <> text
        _ <- actions
        log $ "END   : " <> text
      runShellCommand command = sh $ do
        out <- inshellWithErr command empty
        liftIO $ log $ toMisoString $ lineToText $ bifold out
      configureAndInstall name package =
        logStep ("Configuring " <> name) $ do
          oldConfigurationNixText <- liftIO $ readFile configurationNixFile
          let newConfigurationNixText =
                if isPackagePresent
                  then oldConfigurationNixText
                  -- FIXME
                  else replace
                    environmentSystemPackages
                    (environmentSystemPackages <> "\n\
                         \    " <> package)
                    oldConfigurationNixText
                where isPackagePresent = package `isInfixOf` oldConfigurationNixText -- HACK
          liftIO $ writeFile configurationNixFile newConfigurationNixText
          when (oldConfigurationNixText /= newConfigurationNixText) -- OPTIMIZE
            (logStep "Installing Haskell GHC" (runShellCommand "nixos-rebuild switch"))

  -- TODO join
  configureAndInstall "Haskell GHC" "haskell.compiler.ghc865"
  configureAndInstall "cabal-install" "haskellPackages.cabal-install"
  configureAndInstall "Atom" "atom"
  configureAndInstall "Haskell IDE Engine"
      "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
          \ {}).selection { selector = p: { inherit (p) ghc865; }; })"

  -- liftIO $ do
  --   installAtomPackage "nix"
  --   installAtomPackage "atom-ide-ui"
  --   installAtomPackage "autocomplete-haskell"
  --   installAtomPackage "hasklig"
  --   installAtomPackage "ide-haskell-cabal"
  --   installAtomPackage "ide-haskell-hasktags"
  --   installAtomPackage "ide-haskell-hie"
  --   installAtomPackage "ide-haskell-hoogle"
  --   installAtomPackage "ide-haskell-repl"
  --   installAtomPackage "language-haskell"


putStrLnGreen :: Text -> IO ()
putStrLnGreen str = putStrLn $ "\x1b[32m" <> str <> "\x1b[0m"

configurationNixFile :: String
configurationNixFile = "/etc/nixos/configuration.nix"

environmentSystemPackages :: Text
environmentSystemPackages = "environment.systemPackages = with pkgs; ["

installAtomPackage :: Text -> IO ()
installAtomPackage package = do
  putStrLnGreen $ "Installing " <> package
  exitCode <- shell ("sudo -u $SUDO_USER apm install " <> package) empty
  case exitCode of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("apm install failed with exit code: " <> repr n)
  putStrLnGreen $ "Finished installing " <> package

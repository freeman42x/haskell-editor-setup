module OS.Linux.NixOS where

import           Control.Monad.IO.Class         ( liftIO )
import           Prelude                        ( IO
                                                , when
                                                , ($)
                                                , (<>)
                                                , (/=)
                                                )
import           Data.Bifoldable                ( bifold )
import           Data.Text                      ( replace
                                                , isInfixOf)
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString )
import           Turtle                         ( sh
                                                , inshellWithErr
                                                , empty
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
      configurationNixFile = "/etc/nixos/configuration.nix"
      environmentSystemPackages = "environment.systemPackages = with pkgs; ["
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
          -- TODO ELSE message
          when (oldConfigurationNixText /= newConfigurationNixText) -- OPTIMIZE
            (logStep "Installing Haskell GHC" (runShellCommand "nixos-rebuild switch"))

      -- TODO install or update? or message
      installAtomPackage package = do
        let installingPackage = "Installing Atom package - " <> toMisoString package
        logStep installingPackage $ runShellCommand ("sudo -u $SUDO_USER apm install " <> package)

  -- TODO join
  configureAndInstall "Haskell GHC" "haskell.compiler.ghc865"
  configureAndInstall "cabal-install" "haskellPackages.cabal-install"
  configureAndInstall "Atom" "atom"
  configureAndInstall "Haskell IDE Engine"
      "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
          \ {}).selection { selector = p: { inherit (p) ghc865; }; })"

  -- TODO join
  liftIO $ do
    installAtomPackage "nix"
    installAtomPackage "atom-ide-ui"
    installAtomPackage "autocomplete-haskell"
    installAtomPackage "hasklig"
    installAtomPackage "ide-haskell-cabal"
    installAtomPackage "ide-haskell-hasktags"
    installAtomPackage "ide-haskell-hie"
    installAtomPackage "ide-haskell-hoogle"
    installAtomPackage "ide-haskell-repl"
    installAtomPackage "language-haskell"
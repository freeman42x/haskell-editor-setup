module OS.Linux.NixOS where

import           Control.Monad.IO.Class         ( liftIO )
import           Prelude                        ( IO
                                                , ($)
                                                , (<>)
                                                , (==)
                                                , mapM_
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

          -- FIXME vvv requires Nix parsing using HNIX
          let newConfigurationNixText =
                if isPackagePresent
                  then oldConfigurationNixText

                  else replace
                    environmentSystemPackages
                    (environmentSystemPackages <> "\n\
                         \    " <> package)
                    oldConfigurationNixText
                where isPackagePresent = package `isInfixOf` oldConfigurationNixText
          -- ^^^

          liftIO $ writeFile configurationNixFile newConfigurationNixText
          if oldConfigurationNixText == newConfigurationNixText -- OPTIMIZE
            then log "Nix package already installed"
            else logStep (toMisoString package) (runShellCommand "nixos-rebuild switch")

      -- TODO install or update? extension or log message
      -- TODO ensure extensions are enabled if not enable them
      installAtomPackage package = do
        let installingPackage = "Installing Atom package - " <> toMisoString package
        logStep installingPackage $ runShellCommand ("sudo -u $SUDO_USER apm install " <> package)

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

  -- TODO join
  configureAndInstall "Haskell GHC" "haskell.compiler.ghc865"
  configureAndInstall "cabal-install" "haskellPackages.cabal-install"
  configureAndInstall "Atom" "atom"
  configureAndInstall "Haskell IDE Engine"
      "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
          \ {}).selection { selector = p: { inherit (p) ghc865; }; })"

  mapM_ installAtomPackage [ "nix"
                           , "atom-ide-ui"
                           , "autocomplete-haskell"
                           , "hasklig"
                           , "ide-haskell-cabal"
                           , "ide-haskell-hasktags"
                           , "ide-haskell-hie"
                           , "ide-haskell-hoogle"
                           , "ide-haskell-repl"
                           , "language-haskell" ]
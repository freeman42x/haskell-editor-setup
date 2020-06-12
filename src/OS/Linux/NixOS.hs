module OS.Linux.NixOS where

import           Control.Monad.IO.Class         ( liftIO )
import           Prelude                        ( IO
                                                , (<$>)
                                                , ($)
                                                , (<>)
                                                , (==)
                                                , mapM_
                                                , uncurry
                                                )
import           Data.Bifoldable                ( bifold )
import           Data.Text                      ( replace
                                                , isInfixOf
                                                , Text)
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString
                                                , MisoString)
import           Turtle                         ( sh
                                                , inshellWithErr
                                                , empty
                                                )
import           Turtle.Line                    ( lineToText )

import           Types

data ExtensionInfo = ExtensionInfo MisoString Text

nixOsAtom :: Sink Action -> IO ()
nixOsAtom sink = do
  -- TODO move to utils
  let log text = sink $ Append (text <> "\n")
      -- TODO move to utils
      logStep text actions = do
        log $ "BEGIN : " <> text
        _ <- actions
        log $ "END   : " <> text
      -- TODO move to utils
      configurationNixFile = "/etc/nixos/configuration.nix"
      -- TODO move to utils
      environmentSystemPackages = "environment.systemPackages = with pkgs; ["
      -- TODO move to utils
      runShellCommand command = sh $ do
        out <- inshellWithErr command empty
        liftIO $ log $ toMisoString $ lineToText $ bifold out
      -- TODO move to utils
      configureAndInstall (ExtensionInfo name package) =
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

      -- TODO move to utils
      -- TODO install or update? extension or log message
      -- TODO ensure extensions are enabled if not enable them
      configureAtomPackage package = do
        -- wrap in logStep
        --   check if package isAtomPackageInstalled
        --   install if not installed
        --   update if installed

        let installingPackage = "Installing Atom package - " <> toMisoString package
        logStep installingPackage $
          runShellCommand ("sudo -u $SUDO_USER apm install --color false " <> package)

  mapM_ configureAndInstall $ uncurry ExtensionInfo <$>
    [ ("Haskell GHC", "haskell.compiler.ghc865")
    , ("cabal-install", "haskellPackages.cabal-install")
    , ("Atom", "atom")
    , ("Haskell IDE Engine", "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
    \ {}).selection { selector = p: { inherit (p) ghc865; }; })") ]

  mapM_ configureAtomPackage [ "nix"
                              , "atom-ide-ui"
                              , "autocomplete-haskell"
                              , "hasklig"
                              , "ide-haskell-cabal"
                              , "ide-haskell-hasktags"
                              , "ide-haskell-hie"
                              , "ide-haskell-hoogle"
                              , "ide-haskell-repl"
                              , "language-haskell" ]

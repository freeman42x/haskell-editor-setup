module OS.Linux where

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
import           OS.Common

data ExtensionInfo = ExtensionInfo MisoString Text

nixOsAtom :: Sink Action -> IO ()
nixOsAtom sink = do
  let appendLog text = sink $ Append (text <> "\n")
      logStep text actions = do
        appendLog $ "BEGIN : " <> text
        _ <- actions
        appendLog $ "END   : " <> text
      configurationNixFile = "/etc/nixos/configuration.nix"
      environmentSystemPackages = "environment.systemPackages = with pkgs; ["
      runShellCommand command = sh $ do
        out <- inshellWithErr command empty
        liftIO $ appendLog $ toMisoString $ lineToText $ bifold out
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
            then appendLog "Nix package already installed"
            else logStep (toMisoString package) (runShellCommand "nixos-rebuild switch")

      -- TODO install or update? extension or log message
      -- TODO ensure extensions are enabled if not enable them
      configureAtomPackage package = do
        --   check if package isAtomPackageInstalled
        --   install if not installed
        --   update if installed
        let installingPackage = "Installing Atom package - " <> toMisoString package
        logStep installingPackage $
          installAtomPackage package

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
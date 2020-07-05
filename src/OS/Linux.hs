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
import           Data.Text                      ( replace
                                                , isInfixOf
                                                , Text)
import           Data.Text.IO                   ( readFile
                                                , writeFile
                                                )
import           Miso.Effect
import           Miso.String                    ( toMisoString
                                                , MisoString)

import           Types
import           OS.Common

data ExtensionInfo = ExtensionInfo MisoString Text

nixOsAtom :: Sink Action -> IO ()
nixOsAtom sink = do
  
  mapM_ (configureNixPackage sink) $ uncurry ExtensionInfo <$>
    [ ("Haskell GHC", "haskell.compiler.ghc865")
    , ("cabal-install", "haskellPackages.cabal-install")
    , ("Atom", "atom")
    , ("Haskell IDE Engine", "((import (fetchTarball \"https://github.com/infinisil/all-hies/tarball/master\")\
    \ {}).selection { selector = p: { inherit (p) ghc865; }; })") ]

  mapM_ (configureAtomPackage sink) [ "nix"
                                    , "atom-ide-ui"
                                    , "autocomplete-haskell"
                                    , "hasklig"
                                    , "ide-haskell-cabal"
                                    , "ide-haskell-hasktags"
                                    , "ide-haskell-hie"
                                    , "ide-haskell-hoogle"
                                    , "ide-haskell-repl"
                                    , "language-haskell" ]
    
appendLog :: MisoString -> Sink Action -> IO ()
appendLog text sink = sink $ Append (text <> "\n")

logStep :: MisoString -> Sink Action -> IO a -> IO ()
logStep text sink actions = do
  appendLog ("BEGIN : " <> text) sink
  _ <- actions
  appendLog ("END   : " <> text) sink
  
configureNixPackage :: Sink Action -> ExtensionInfo -> IO ()
configureNixPackage sink (ExtensionInfo name package) =
  logStep ("Configuring " <> name) sink $ do
    let configurationNixFile = "/etc/nixos/configuration.nix"
    oldConfigurationNixText <- liftIO $ readFile configurationNixFile

    -- FIXME vvv requires Nix parsing using HNIX
    let environmentSystemPackages = "environment.systemPackages = with pkgs; ["
        isPackagePresent = package `isInfixOf` oldConfigurationNixText
        newConfigurationNixText =
          if isPackagePresent
            then oldConfigurationNixText
            else replace
              environmentSystemPackages
              (environmentSystemPackages <> "\n\
                  \    " <> package)
              oldConfigurationNixText
    -- ^^^

    liftIO $ writeFile configurationNixFile newConfigurationNixText
    if oldConfigurationNixText == newConfigurationNixText -- OPTIMIZE
      then appendLog "Nix package already installed" sink
      else logStep (toMisoString package) sink (runShellCommand "nixos-rebuild switch")

-- TODO install or update? extension or log message
-- TODO ensure extensions are enabled if not enable them
configureAtomPackage :: Sink Action -> Text -> IO ()
configureAtomPackage sink package = do
  --   check if package isAtomPackageInstalled
  --   install if not installed
  --   update if installed
  let installingPackage = "Installing Atom package - " <> toMisoString package
  logStep installingPackage sink $
    installAtomPackage package
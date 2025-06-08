{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (makeLenses, view)
import Control.Monad (void)
import Control.Monad.Reader
import Data.GI.Base
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (pack, unpack)
import qualified GI.AstalRiver as River
import qualified GI.GLib as GLib
import Layout
import Options.Applicative
import State

data Env = Env
  { _envMainLoop :: GLib.MainLoop,
    _envRiver :: River.River,
    _envLayout :: River.Layout,
    _envNamespace :: String,
    _envOutputStates :: IORef (Map.Map String OutputState)
  }

makeLenses ''Env

data CLIOptions = CLIOptions
  { optNamespace :: String,
    optMainRatio :: Double,
    optMainCount :: Int,
    optViewPadding :: Int,
    optOuterPadding :: Int,
    optLayoutName :: String
  }

cliOptionsParser :: Parser CLIOptions
cliOptionsParser =
  CLIOptions
    <$> strOption
      ( long "namespace"
          <> short 'n'
          <> metavar "NAMESPACE"
          <> value "conflux"
          <> help "Namespace for the layout"
      )
    <*> option
      auto
      ( long "main-ratio"
          <> short 'r'
          <> metavar "RATIO"
          <> value 0.6
          <> help "Main area ratio"
      )
    <*> option
      auto
      ( long "main-count"
          <> short 'c'
          <> metavar "COUNT"
          <> value 1
          <> help "Number of windows in main area"
      )
    <*> option
      auto
      ( long "view-padding"
          <> short 'v'
          <> metavar "PADDING"
          <> value 2
          <> help "Padding between views"
      )
    <*> option
      auto
      ( long "outer-padding"
          <> short 'o'
          <> metavar "PADDING"
          <> value 2
          <> help "Outer padding"
      )
    <*> strOption
      ( long "layout"
          <> short 'l'
          <> metavar "LAYOUT"
          <> value "rStack"
          <> help "Initial layout"
      )

exit :: (MonadIO m) => Env -> String -> m ()
exit env msg = do
  liftIO $ putStrLn msg
  #quit $ view envMainLoop env

main :: IO ()
main = do
  opts <-
    execParser $
      info
        (cliOptionsParser <**> helper)
        ( fullDesc
            <> progDesc "River layout generator"
            <> header "conflux - a dynamic layout generator for river"
        )

  let namespace = optNamespace opts
  river <- fromJust <$> River.getDefault
  layout <- river.newLayout $ pack namespace
  mainLoop <- GLib.mainLoopNew Nothing False
  outputStates <- newIORef Map.empty

  let env =
        Env
          { _envMainLoop = mainLoop,
            _envRiver = river,
            _envLayout = layout,
            _envNamespace = namespace,
            _envOutputStates = outputStates
          }

  let defaultOutputState =
        validateState $
          OutputState
            { _osMainRatio = optMainRatio opts,
              _osMainCount = optMainCount opts,
              _osViewPadding = optViewPadding opts,
              _osOuterPadding = optOuterPadding opts,
              _osLayoutName = optLayoutName opts
            }

  let signals =
        [ (2, "Received interrupt signal, shutting down."),
          (15, "Received termination signal, shutting down.")
        ]
  mapM_
    ( \(sig, msg) ->
        GLib.unixSignalAdd GLib.PRIORITY_DEFAULT sig $
          exit env msg >> pure False
    )
    signals

  void $ on layout #namespaceInUse $ \_ -> exit env "Namespace already in use"

  void $ on layout #userCommand $ \cmd output -> do
    name <- fromMaybe "" <$> get output #name
    modifyIORef' outputStates $ \states ->
      let outputState = Map.findWithDefault defaultOutputState (unpack name) states
          newState = updateWithCommand output (unpack cmd) outputState
       in Map.insert (unpack name) newState states

  #setLayoutDemandCallback layout $ \_ output n width height -> do
    states <- readIORef outputStates
    name <- fromMaybe "" <$> get output #name
    let clientState = Map.findWithDefault defaultOutputState (unpack name) states
    runReaderT (createLayout output n width height) clientState

  #run mainLoop

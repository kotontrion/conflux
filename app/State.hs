{-# LANGUAGE TemplateHaskell #-}

module State where

import Control.Lens (makeLenses, over, set, view)
import qualified GI.AstalRiver as River

data OutputState = OutputState
  { _osMainRatio :: Double,
    _osMainCount :: Int,
    _osViewPadding :: Int,
    _osOuterPadding :: Int,
    _osLayoutName :: String
  }
  deriving (Show)

makeLenses ''OutputState

validateState :: OutputState -> OutputState
validateState state =
  OutputState
    { _osMainRatio = clamp 0.1 0.9 $ view osMainRatio state,
      _osMainCount = max 0 $ view osMainCount state,
      _osViewPadding = max 0 $ view osViewPadding state,
      _osOuterPadding = max 0 $ view osOuterPadding state,
      _osLayoutName = view osLayoutName state
    }
  where
    clamp min' max' = max min' . min max'

updateWithCommand :: River.Output -> String -> OutputState -> OutputState
updateWithCommand _ cmd state = validateState $ case words cmd of
  [prop, val] -> case prop of
    "main-ratio" -> modifyValue osMainRatio val
    "main-count" -> modifyValue osMainCount val
    "view-padding" -> modifyValue osViewPadding val
    "outer-padding" -> modifyValue osOuterPadding val
    "layout" -> set osLayoutName val state
    _ -> state
  _ -> state
  where
    modifyValue lens val = case val of
      ('+' : num) -> over lens (+ read num) state
      ('-' : num) -> over lens (\x -> x - read num) state
      num -> set lens (read num) state

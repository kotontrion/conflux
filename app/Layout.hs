{-# LANGUAGE OverloadedLabels #-}

module Layout where

import Control.Lens (view)
import Control.Monad.Reader
import Data.GI.Base
import Data.Text (Text, pack)
import Data.Word
import qualified GI.AstalRiver as River
import State

type Geometry = (Word32, Word32, Word32, Word32)

type LayoutFunction =
  River.Output ->
  Word32 ->
  Word32 ->
  Word32 ->
  Word32 ->
  Word32 ->
  ReaderT OutputState IO ([Geometry])

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

rotateGeometry :: Geometry -> Geometry
rotateGeometry (x, y, w, h) = (y, x, h, w)

mirrorGeometry :: Word32 -> Word32 -> Geometry -> Geometry
mirrorGeometry width height (x, y, w, h) =
  (width - (x + w), y, w, h)

rotate :: LayoutFunction -> LayoutFunction
rotate layout output n x y width height = do
  layout output n y x height width
    >>= pure . map rotateGeometry

mirror :: LayoutFunction -> LayoutFunction
mirror layout output n x y width height = do
  layout output n x y width height
    >>= pure . map (mirrorGeometry width height)

rows :: LayoutFunction
rows _ 0 _ _ _ _ = pure []
rows _ n x y width height = do
  let rowHeight = height `div` n
  pure
    [ (x, y + (i * rowHeight), width, rowHeight)
    | i <- [0 .. n - 1]
    ]

columns :: LayoutFunction
columns _ 0 _ _ _ _ = pure []
columns _ n x y width height = do
  let columnWidth = width `div` n
  pure
    [ (x + (i * columnWidth), y, columnWidth, height)
    | i <- [0 .. n - 1]
    ]

rStack :: LayoutFunction
rStack _ 0 _ _ _ _ = pure []
rStack o n x y width height = do
  outputState <- ask
  let mainRatio = view osMainRatio outputState
      mainCount = view osMainCount outputState
      mainViews = min n $ fromIntegral mainCount
      mainWidth
        | mainViews == n = width
        | mainViews <= 0 = 0
        | otherwise = round $ (fromIntegral width) * mainRatio
  (<>)
    <$> rows o mainViews x y mainWidth height
    <*> rows o (n - mainViews) (x + mainWidth) y (width - mainWidth) height

lStack :: LayoutFunction
lStack = mirror rStack

bStack :: LayoutFunction
bStack = rotate rStack

tStack :: LayoutFunction
tStack = rotate $ mirror rStack

getLayoutFunction :: String -> LayoutFunction
getLayoutFunction layoutName = do
  case layoutName of
    "rStack" -> rStack
    "lStack" -> lStack
    "bStack" -> bStack
    "tStack" -> tStack
    "rows" -> rows
    "columns" -> columns
    _ -> rStack

applyOuterPadding :: Word32 -> Word32 -> ReaderT OutputState IO Geometry
applyOuterPadding width height = do
  padding <- fromIntegral <$> view osOuterPadding <$> ask
  pure (padding, padding, width - 2 * padding, height - 2 * padding)

applyViewPadding :: [Geometry] -> ReaderT OutputState IO [Geometry]
applyViewPadding views = do
  padding <- fromIntegral <$> view osViewPadding <$> ask
  pure $
    map
      ( \(x, y, w, h) ->
          (x + padding, y + padding, w - 2 * padding, h - 2 * padding)
      )
      views

geometryToRiverGeometry :: (MonadIO m) => [Geometry] -> m [River.Geometry]
geometryToRiverGeometry = traverse $ \(x, y, w, h) ->
  new
    River.Geometry
    [ #x := x,
      #y := y,
      #width := w,
      #height := h
    ]

createLayout ::
  River.Output ->
  Word32 ->
  Word32 ->
  Word32 ->
  ReaderT OutputState IO (Text, [River.Geometry])
createLayout output n width height = do
  layoutName <- view osLayoutName <$> ask
  applyOuterPadding width height
    >>= uncurry4 (getLayoutFunction layoutName output n)
    >>= applyViewPadding
    >>= geometryToRiverGeometry
    >>= pure . (,) (pack layoutName)

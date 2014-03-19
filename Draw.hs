module Draw where

--import Control.Monad.State
--import Control.Monad.Reader
--import Foreign.Ptr (Ptr)
import Control.Monad
import Control.Monad.Instances
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.IORef
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G
import Graphics.Rendering.Cairo hiding (Matrix)
import qualified Graphics.Rendering.Cairo as C
import Numeric.LinearAlgebra ((<>),(><),(|>),(@>),(@@>),Vector,Matrix)
import qualified Numeric.LinearAlgebra as V

import Types
import Utils

draw :: IORef GameState -> EventM EExpose Bool
draw r = liftIO $ do
  gameState <- readIORef r
  (w',h') <- widgetGetSize $ getDrawingArea gameState
  assocs <- makeAssocs gameState
  let Just drawWindow = getDrawWindow gameState
      size = getSize gameState
      outcome = getOutcome gameState
      showAdjacents = concatMap rawAdjacents $ getShowAdjacents gameState
      adjacents = makeWeakAdjacents gameState
      [w,h] = map fromIntegral [w',h']
      ccMap = Map.fromList $ map (\i -> (i,convertCoords gameState [w,h] i)) $ range (getDimensions gameState)
  renderWithDrawable drawWindow $ do
    setSourceRGB 1 1 1
    paint
--    mapM_ (drawLink . map (snd . convertCoords gameState [w,h])) adjacents
--    mapM_ (\((_,converted),(i,cell)) -> drawCellAt (i `elem` showAdjacents) outcome cell size converted) $ sortBy (comparing (fst.fst)) $ map (\(i,cell) -> (convertCoords gameState [w,h] i,(i,cell))) assocs
    mapM_ (drawLink . map (snd . (ccMap Map.!))) adjacents
    mapM_ (\((_,converted),(i,cell)) -> drawCellAt (i `elem` showAdjacents) outcome cell size converted) $ sortBy (comparing (fst.fst)) $ map (\(i,cell) -> (ccMap Map.! i,(i,cell))) assocs
  return False

drawCellAt :: Bool -> Maybe Bool -> Int -> Double -> [Double] -> Render ()
drawCellAt a (Just True) cell
  | cell <= 0 = (if a then drawG 1 else drawG 0.8) >>.. drawNumber (-cell)
  | cell `elem` [3,5,7] = (if a then drawG 0.9 else drawG 0.5) >>.. drawFlag
drawCellAt a (Just False) cell
  | cell <= 0 = (if a then drawG 1 else drawG 0.8) >>.. drawNumber (-cell)
  | cell == 1 = (if a then drawRGB 1 0.6 0.6 else drawRGB 1 0 0) >>.. drawMine
  | cell == 2 = (if a then drawG 0.9 else drawG 0.5) >>.. drawQuestion >>.. drawWrong
  | cell == 3 = (if a then drawG 0.9 else drawG 0.5) >>.. drawMine >>.. drawQuestion
  | cell == 4 = (if a then drawG 0.9 else drawG 0.5) >>.. drawFlag >>.. drawWrong
  | cell == 5 = (if a then drawG 0.9 else drawG 0.5) >>.. drawFlag
  | cell == 6 = (if a then drawG 0.9 else drawG 0.5)
  | cell == 7 = (if a then drawG 0.9 else drawG 0.5) >>.. drawMine
drawCellAt a Nothing cell
  | cell <= 0 = (if a then drawG 1 else drawG 0.8) >>.. drawNumber (-cell)
  | cell >= 2 && cell <= 3 = (if a then drawG 0.9 else drawG 0.5) >>.. drawQuestion
  | cell >= 4 && cell <= 5 = (if a then drawG 0.9 else drawG 0.5) >>.. drawFlag
  | cell >= 6 && cell <= 7 = if a then drawG 0.9 else drawG 0.5
drawCellAt _ o c = drawRGB 0 1 1 >> (error $ "WTF?  You had outcome = "++show o++" and cell = "++show c++"!  Something broke...")

drawLink :: [[Double]] -> Render ()
drawLink [[x,y],[x',y']] = do
  setSourceRGB 0 0 0
  setLineWidth 0.5
  moveTo x y
  lineTo x' y'
  stroke

drawG :: Double -> Double -> [Double] -> Render ()
drawG x = drawRGB x x x

drawRGB :: Double -> Double -> Double -> Double -> [Double] -> Render ()
drawRGB r g b s [x,y] = do
  setSourceRGB 0 0 0
  rectangle (x-s/2) (y-s/2) s s
  strokePreserve
  setSourceRGB r g b
  fill

drawMine :: Double -> [Double] -> Render ()
drawMine s [x,y] = do
  setSourceRGB 0 0 0
  arc x y (s/3) 0 (2*pi)
  strokePreserve
  fill

drawNumber :: Int -> Double -> [Double] -> Render ()
drawNumber n s [x,y] = do
  setSourceRGB 0 0 0
  setFontSize (2*s/3)
  extents <- textExtents $ show' n
  moveTo (x - textExtentsXbearing extents - (textExtentsWidth extents)/2) (y + (textExtentsHeight extents)/2)
  showText $ show' n
  where show' 0 = ""
        show' n = show n

drawQuestion :: Double -> [Double] -> Render ()
drawQuestion s [x,y] = do
  setSourceRGB 0 0 1
  setFontSize (2*s/3)
  extents <- textExtents "?"
  moveTo (x - textExtentsXbearing extents - (textExtentsWidth extents)/2) (y + (textExtentsHeight extents)/2)
  showText "?"

drawFlag :: Double -> [Double] -> Render ()
drawFlag s [x,y] = do
  setSourceRGB 0 0 0
  moveTo (x-s/4) (y-s/2)
  lineTo (x-s/4) (y+s/2)
  stroke
  
  setSourceRGB 1 0 0
  moveTo (x-s/4) (y-s/2)
  lineTo (x+s/4) (y-s/3)
  lineTo (x-s/4) (y-s/6)
  closePath
  fill

drawWrong :: Double -> [Double] -> Render ()
drawWrong s [x,y] = do
  setSourceRGB 1 0 0
  setLineWidth 2
  moveTo (x-s/2) (y-s/2)
  lineTo (x+s/2) (y+s/2)
  stroke
  moveTo (x+s/2) (y-s/2)
  lineTo (x-s/2) (y+s/2)
  stroke

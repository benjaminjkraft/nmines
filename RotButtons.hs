

module RotButtons where

import Control.Monad
import Control.Monad.Instances
import Data.IORef
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Glade
import Numeric.LinearAlgebra ((<>),(><),(|>),(@>),(@@>),Vector,Matrix)
import qualified Numeric.LinearAlgebra as V

import Types
import Utils

refreshButtons :: IORef GameState -> IO ()
refreshButtons gameStateRef = do
  gameState <- readIORef gameStateRef
  let (up,down,left,right) = getButtonBoxes gameState
      n = getN gameState
      rotStep = getRotStep gameState
      drawingArea = getDrawingArea gameState
      xml = getXML gameState
  [upB,downB,leftB,rightB] <- mapM (mapM $ (\ k -> fmap ((,) k) $ buttonNewWithLabel $ show k)) $ replicate 4 [3..n]
  containerForeach up (containerRemove up)
  containerForeach down (containerRemove down)
  containerForeach left (containerRemove left)
  containerForeach right (containerRemove right)
  mapM_ (processButton gameStateRef up 1) upB
  mapM_ (processButton gameStateRef down 3) downB
  mapM_ (processButton gameStateRef left 2) leftB
  mapM_ (processButton gameStateRef right 0) rightB
  buttonClickAction xml "rotCW" (buttonAction gameStateRef 4 0)
  buttonClickAction xml "rotCCW" (buttonAction gameStateRef 5 0)
  return ()

processButton :: (ContainerClass b) => IORef GameState -> b -> Int -> (Int,Button) -> IO ()
processButton g box side (k,button) = do -- side: 0 = right, going CCW
  containerAdd box button
  widgetShow button
  G.on button buttonActivated $ buttonAction g side k
  return ()

buttonAction :: IORef GameState -> Int -> Int -> IO ()
buttonAction gameStateRef side k = do
  gameState <- readIORef gameStateRef
  let rotStep = getRotStep gameState
      n = getN gameState
      d = getDrawingArea gameState
      rp = case side of 
                0 -> rotPlane n 0 (k-1) . negate
                1 -> rotPlane n 1 (k-1) . negate
                2 -> rotPlane n 0 (k-1)
                3 -> rotPlane n 1 (k-1)
                4 -> rotPlane n 0 1
                5 -> rotPlane n 0 1 . negate
  modifyIORef gameStateRef (\gS -> gS { getViewPoint = rp rotStep <> getViewPoint gS })
  widgetQueueDraw d


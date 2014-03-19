

module Game where

import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.IORef
import Data.List
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G
import Numeric.LinearAlgebra ((<>),(><),(|>),(@>),(@@>),Vector,Matrix)
import qualified Numeric.LinearAlgebra as V
import System.Random

import RotButtons
import Types
import Utils

generateField :: IORef GameState -> Index -> IO (Field,Near)
generateField gameStateRef i = do
  gameState <- readIORef gameStateRef
  let is = getDimensions gameState
      g = getGen gameState
      nMines = getMinesStart gameState
      (g',g'') = split g
      cells = range is
      mineIs = take nMines $ randPermutation g' $ cells\\(rawAdjacents i)
  field <- newArray is 6
  mapM_ (flip (writeArray field) 7) mineIs
  minesNear <- fmap (array is) $ mapM (\ i -> fmap ((,) i) $ fmap sum $ mapM ((fmap (+(-6))) . readArray field) $ filter (inRange is) $ rawAdjacents i) cells
  writeIORef gameStateRef $ gameState { getGen = g'', getField = Just (field, minesNear) }
  return (field, minesNear)


randPermutation :: (RandomGen g) => g -> [a] -> [a]
randPermutation g [] = []
randPermutation g xs = z:(randPermutation g' $ ys ++ zs)
  where n = length xs
        (i,g') = randomR (0,n-1) g
        (ys, z:zs) = splitAt i xs

win :: IORef GameState -> IO ()
win r = do
  modifyIORef r (\gS -> gS { getOutcome = Just True })
  gameState <- readIORef r
  widgetQueueDraw (getDrawingArea gameState)
  imageSetIcon (getFace gameState) "face-smile-big"
  let dialog = getWinDialog gameState
  response <- dialogRun dialog
  widgetHide dialog
  case response of
       ResponseYes -> modifyIORef r newGameSame
       _ -> mainQuit


lose :: IORef GameState -> IO ()
lose r = do
  modifyIORef r (\gS -> gS { getOutcome = Just False})
  gameState <- readIORef r
  widgetQueueDraw (getDrawingArea gameState)
  imageSetIcon (getFace gameState) "face-sad"
  let dialog = getLoseDialog gameState
  response <- dialogRun dialog
  widgetHide dialog
  case response of
       ResponseYes -> modifyIORef r newGameSame
       _ -> mainQuit

newGame :: IORef GameState -> IO ()
newGame r = do
  gameState <- readIORef r
  let new = getNewGameWindow gameState
      [dS,mS,sS] = getSpins gameState
  spinButtonSetValue dS (fromIntegral $ getN gameState)
  spinButtonSetValue mS (fromIntegral $ getMinesStart gameState)
  spinButtonSetValue sS (fromIntegral $ (+1) $ head $ snd $ getDimensions gameState)
  widgetShowAll new

playNewGame :: IORef GameState -> IO ()
playNewGame r = do
  gameState <- readIORef r
  [n,m,k] <- mapM spinButtonGetValueAsInt $ getSpins gameState
  let is = (replicate n 0, replicate n (k-1))
  widgetHideAll (getNewGameWindow gameState)
  writeIORef r $ gameState { getN = n
                           , getDimensions = is
                           , getField = Nothing
                           , getMinesLeft = m
                           , getMinesStart = m
                           , getOutcome = Nothing
                           , getViewPoint = if n==(getN gameState) && is==(getDimensions gameState)
                                               then getViewPoint gameState
                                               else V.optimiseMult $ map (\i -> rotPlane n (i`mod`2) i (pi/12*(fromIntegral $ i`div`2))) [2..(n-1)]
                           }
  refreshButtons r
  widgetQueueDraw (getDrawingArea gameState)

newGameSame :: GameState -> GameState
newGameSame gameState = gameState { getField = Nothing, getMinesLeft = getMinesStart gameState, getOutcome = Nothing }

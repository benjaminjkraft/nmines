

module Click where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Array.MArray
import Data.Array.IArray
import Data.Array.IO
import Data.Array.Unboxed
import Data.IORef
import Data.List
import Data.Maybe
import Data.Ord
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G

import Game
import Types
import Utils

processClick :: IORef GameState -> EventM EButton Bool
processClick r = do
  mb <- eventButton
  c <- eventClick
  (x,y) <- eventCoordinates
  liftIO $ do
    gameState <- readIORef r
    let notOver = isNothing $ getOutcome gameState
    i <- findClick r [x,y]
    modifyIORef r (\ gS -> gS { getShowAdjacents = if c==SingleClick then i else [] })
    imageSetIcon (getFace gameState) (if c==SingleClick && mb==LeftButton && notOver then "face-surprise" else "face-plain")
    when (c/=SingleClick && notOver) $ mapM_ (openCell r mb c) i
    widgetQueueDraw (getDrawingArea gameState)
  return False

openCell :: IORef GameState -> MouseButton -> Click -> Index -> IO ()
openCell r mb c i = do
  --print $ "opening"++show i
  gameState <- readIORef r
  let field' = getField gameState
      dims = getDimensions gameState
      face = getFace gameState
  (field,minesNear) <- if isNothing field' then generateField r i else return $ fromJust field'
  cell <- readArray field i
  case c of
      ReleaseClick -> case mb of
          RightButton -> do
              let cell' = rightClickModify cell
              writeArray field i cell'
              when (cell>=4 && cell<=5) $ modifyIORef r (\gS -> gS { getMinesLeft = getMinesLeft gS + 1})
              when (cell'>=4 && cell'<=5) $ modifyIORef r (\gS -> gS { getMinesLeft = getMinesLeft gS - 1})
              minesLeftUpdate r
          LeftButton -> do
              let cell' = leftClickModify cell
              writeArray field i cell'
              when (cell'==1) $ lose r
              when (cell'==0 && minesNear!i==0) $ do
                let adj = (filter (inRange dims) (rawAdjacents i))
                filteredAdj <- filterM (fmap (/=0) . readArray field) adj
                mapM_ (openCell r mb c) filteredAdj
          _ -> return ()
      DoubleClick -> case mb of
          LeftButton -> let mN = minesNear!i in when (cell==0 && mN/=0) $ do
              let adj = filter (inRange dims) (rawAdjacents i)
              knownNear <- mapM (readArray field) adj
              let knownFull = length $ filter ((&&) <$> (>=4) <*> (<=5)) knownNear
              when (knownFull==mN) $ mapM_ (openCell r LeftButton ReleaseClick) adj
          _ -> return ()
      _ -> return ()
  e <- getElems field
  when (all (`elem`[0,3,5,7]) e) $ win r
  where rightClickModify :: Int -> Int
        rightClickModify x
          | x<=1 = x
          | x<=3 = x+4
          | otherwise = x-2
        leftClickModify :: Int -> Int
        leftClickModify x
          | x>=4 && x<=5 = x
          | otherwise = x`mod`2

findClick :: IORef GameState -> [Double] -> IO [Index]
findClick r rawCoords = do
  gameState <- readIORef r
  (w',h') <- widgetGetSize $ getDrawingArea gameState
  let dims = getDimensions gameState
      size = getSize gameState
      [w,h] = map fromIntegral [w',h']
  return $ map snd $ take 1 $ reverse $ sortBy (comparing (fst.fst)) $ filter (\((_,loc),i) -> all ((<=size/2).abs) $ zipWith (-) loc rawCoords) $ map (\i -> (convertCoords gameState [w,h] i,i)) (range dims)

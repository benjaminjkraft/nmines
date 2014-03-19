module Utils where

--import Control.Monad.State
--import Data.IORef
import Control.Applicative
import Data.Array.IArray
import Data.Array.IO
import Data.Array.MArray
import Data.Array.Unboxed
import Data.IORef
import Data.List
import Numeric.LinearAlgebra ((<>),(><),(|>),(@>),(@@>),Vector,Matrix)
import qualified Numeric.LinearAlgebra as V
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Glade

import Types

infixl 1 >>.
(>>.) :: (Monad m) => (a -> m b) -> (a -> m c) -> a -> m c
(>>.) m1 m2 arg = m1 arg >> m2 arg

infixl 1 >>..
(>>..) :: (Monad m) => (a -> b -> m c) -> (a -> b -> m d) -> a -> b -> m d
(>>..) m1 m2 arg1 arg2 = m1 arg1 arg2 >> m2 arg1 arg2

convertCoords :: GameState -> [Double] -> Index -> (Double, [Double])
convertCoords gameState dims@[w,h] i = let (ix,ix') = getDimensions gameState
                                           avgix = map ((/2).fromIntegral) $ zipWith (+) ix ix'
                                           relCoords =  map (/((sqrt $ fromIntegral $ getN gameState)*(maximum $ zipWith (-) (map fromIntegral ix') (map fromIntegral ix)))) (zipWith (-) (map fromIntegral i) avgix)
                                           relCoordsV = V.fromList relCoords
                                           asViewed = take 3 $ V.toList $ getViewPoint gameState <> relCoordsV
                                       in  (asViewed!!2, zipWith3 (\x xc w -> (minimum dims - getSize gameState)*x+xc*w) asViewed (getCenter gameState) dims)


makeAssocs :: GameState -> IO [(Index, Int)]
makeAssocs gameState = case getField gameState of
  Nothing -> return $ map (\i -> (i,6)) (range $ getDimensions gameState)
  Just (field, near) -> do
    assocs <- getAssocs field
    let mapper (i, 0) = (i,-near!i)
        mapper (i, c) = (i, c)
    return $ map mapper assocs

minesLeftUpdate :: IORef GameState -> IO ()
minesLeftUpdate r = do
  gameState <- readIORef r
  labelSetText (getMinesLeftLabel gameState) $ "Mines Left:\n"++show (getMinesLeft gameState)++"/"++show (getMinesStart gameState)

imageSetIcon :: Image -> String -> IO ()
--imageSetIcon image name = imageGetPixelSize image >>= imageSetFromIconName image name . IconSizeUser
imageSetIcon image name = imageSetFromIconName image name IconSizeLargeToolbar

makeWeakAdjacents :: GameState -> [[Index]]
makeWeakAdjacents gameState = concatMap (\i -> map (:i:[]) $ filter ((&&) <$> inRange dims <*> (<=i)) $ rawWeakAdjacents i) $ range dims
  where dims = getDimensions gameState

rawWeakAdjacents :: Index -> [Index]
rawWeakAdjacents [] = []
rawWeakAdjacents (x:xs) = (map (:xs) [x-1,x+1]) ++ (map (x:) (rawWeakAdjacents xs))

rawAdjacents :: Index -> [Index]
rawAdjacents [] = [[]]
rawAdjacents (x:xs) = (:) <$> [x-1..x+1] <*> rawAdjacents xs

buttonClickAction :: GladeXML -> String -> IO () -> IO (ConnectId Button)
buttonClickAction xml name io = do
  button <- xmlGetWidget xml castToButton name
  G.on button buttonActivated io

rotPlane :: Int -> Int -> Int -> Double -> Matrix Double
rotPlane n i i' th = V.inv p <> rotXY n th <> p
  where p = permutationMatrix perm
        perm = i:i':([0..(n-1)]\\[i,i'])

permutationMatrix :: [Int] -> Matrix Double
permutationMatrix p = V.fromLists $ map elem p
  where elem n = (replicate n 0) ++ [1] ++ (replicate (length p - n - 1) 0)

rotXY, rotXZ, rotYZ :: Int -> Double -> Matrix Double
rotXY 2 th= (2><2) [cos th, (-sin th), sin th, cos th]
rotXY n th= V.fromBlocks [[rotXY 2 th, 0], [0, V.ident (n-2)]]

rotXZ n = rotPlane n 0 2

rotYZ n = rotPlane n 2 1

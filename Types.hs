

module Types where

import Control.Applicative
import Data.Array.IO
import Data.Array.Unboxed
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Glade
import Numeric.LinearAlgebra ((<>),(><),(|>),(@>),(@@>),Vector,Matrix)
import qualified Numeric.LinearAlgebra as V
import System.Random

instance (Ix a) => Ix [a] where
  range ([],[]) = [[]]
  range (x:xs,x':xs') = (:) <$> range (x,x') <*> range (xs,xs')
  range _ = error "dim mismatch"
  index ([],[]) [] = 0
  index (x:xs,x':xs') (y:ys) = (index (x,x') y)*(rangeSize (xs,xs')) + (index (xs,xs') ys)
  index _ _ = error "dim mismatch"
  inRange ([],[]) [] = True
  inRange (x:xs, x':xs') (y:ys) = (inRange (x,x') y) && (inRange (xs,xs') ys)
  inRange _ _ = error "dim mismatch"
  rangeSize (xs, xs') = product $ zipWith (curry rangeSize) xs xs'

type Index = [Int]
type Field = IOUArray Index Int
type Near = UArray Index Int
type ButtonBoxes = (HButtonBox,HButtonBox,VButtonBox,VButtonBox)
-- type Mine = Int:
-- 0-1 opened (1 iff dead)
-- 2-3 question
-- 4-5 flag
-- 6-7 unopened

data GameState = GameState { getN :: Int
                           , getDimensions :: (Index,Index)
                           , getGen :: StdGen
                           , getViewPoint :: Matrix Double
                           , getField :: Maybe (Field, Near)
                           , getMinesStart :: Int
                           , getMinesLeft :: Int
                           , getOutcome :: Maybe Bool
                           , getShowAdjacents :: [Index]
                           , getRotStep :: Double
                           , getDrawWindow :: Maybe DrawWindow
                           , getDrawingArea :: DrawingArea
                           , getWinDialog :: Dialog
                           , getLoseDialog :: Dialog
                           , getTopWindow :: Window
                           , getNewGameWindow :: Window
                           , getSpins :: [SpinButton]
                           , getMinesLeftLabel :: Label
                           , getFace :: Image
                           , getXML :: GladeXML
                           , getCenter :: [Double]
                           , getSize :: Double
                           , getButtonBoxes :: ButtonBoxes
                           }

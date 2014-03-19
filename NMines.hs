

module Main where

--import Control.Monad.State
import Control.Monad
import Control.Monad.Instances
import Control.Monad.Trans
import Data.IORef
import Graphics.UI.Gtk hiding (on,get)
import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk.Glade
import Numeric.LinearAlgebra ((<>),(><),(|>),(@>),(@@>),Vector,Matrix)
import qualified Numeric.LinearAlgebra as V
import System.Random

import Click
import Draw
import Game
import RotButtons
import Types
import Utils
import Paths_nmines

--todo: rotButtons often reflect as well as rotate.  fix this.

main :: IO GameState
main = do
  initGUI
  gladePath <- getDataFileName "nmines.glade"
  Just xml <- xmlNew gladePath
  topWindow <- xmlGetWidget xml castToWindow "topWindow"
  newGameWindow <- xmlGetWidget xml castToWindow "newGame"
  table <- xmlGetWidget xml castToTable "table"
  drawingArea <- xmlGetWidget xml castToDrawingArea "drawingArea"
  winDialog <- xmlGetWidget xml castToDialog "winDialog"
  loseDialog <- xmlGetWidget xml castToDialog "loseDialog"
  minesLeftLabel <- xmlGetWidget xml castToLabel "minesLeftLabel"
  dimsSpin <- xmlGetWidget xml castToSpinButton "dimsSpin"
  minesSpin <- xmlGetWidget xml castToSpinButton "minesSpin"
  sizeSpin <- xmlGetWidget xml castToSpinButton "sizeSpin"
  face <- xmlGetWidget xml castToImage "face"
  hBBUp <- hButtonBoxNew
  hBBDown <- hButtonBoxNew
  vBBLeft <- vButtonBoxNew
  vBBRight <- vButtonBoxNew
  mapM_ (\(c,(x,y)) -> tableAttach table c x (x+1) y (y+1) [Shrink] [Shrink] 0 0) [(hBBUp,(1,2)),(hBBDown,(1,4))]
  mapM_ (\(c,(x,y)) -> tableAttach table c x (x+1) y (y+1) [Shrink] [Shrink] 0 0) [(vBBLeft,(0,3)),(vBBRight,(2,3))]

  gameState <- initialState (hBBUp,hBBDown,vBBLeft,vBBRight) drawingArea minesLeftLabel face winDialog loseDialog topWindow newGameWindow dimsSpin minesSpin sizeSpin xml
  gameStateRef <- newIORef gameState
  minesLeftUpdate gameStateRef

  G.on topWindow objectDestroy $ mainQuit
  G.on drawingArea realize $ do
    d <- widgetGetDrawWindow drawingArea
    modifyIORef gameStateRef (\gS -> gS { getDrawWindow = Just d })
  G.on drawingArea exposeEvent (draw gameStateRef)
  G.on drawingArea buttonPressEvent (processClick gameStateRef)
  G.on drawingArea buttonReleaseEvent (processClick gameStateRef)
  refreshButtons gameStateRef
  buttonClickAction xml "reset" $ newGame gameStateRef --modifyIORef gameStateRef newGameSame >> widgetQueueDraw drawingArea
  buttonClickAction xml "quit" $ mainQuit
  buttonClickAction xml "newGameButton" $ playNewGame gameStateRef
  widgetShowAll topWindow
  mainGUI
  readIORef gameStateRef

initialState b d l f winD loseD t new dS mS sS x = do
  g <- newStdGen
  return $ GameState { getN = 5
                     , getDimensions = ([0,0,0,0,0],[2,2,2,2,2])
                     , getGen = g
                     , getViewPoint = rotPlane 5 0 2 (pi/12) <> rotPlane 5 1 3 (pi/12)
                     , getField = Nothing
                     , getMinesLeft = 20
                     , getMinesStart = 20
                     , getOutcome = Nothing
                     , getShowAdjacents = []
                     , getRotStep = pi/12
                     , getDrawWindow = Nothing
                     , getDrawingArea = d
                     , getWinDialog = winD
                     , getLoseDialog = loseD
                     , getTopWindow = t
                     , getNewGameWindow = new
                     , getSpins = [dS, mS, sS]
                     , getMinesLeftLabel = l
                     , getFace = f
                     , getXML = x
                     , getCenter = [0.5,0.5]
                     , getSize = 20
                     , getButtonBoxes = b
                     }

-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  GPL3
--
-- Maintainer  :  Roger Turnau
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------


module Main where

import Display
import Game
import Bindings
import Graphics.UI.GLUT as GLUT
import Data.IORef
import Control.Monad

myInit :: IO ()
myInit = undefined

main = do
    (progName, args) <- getArgsAndInitialize
    initialDisplayMode $= [GLUT.DoubleBuffered]
    createWindow progName
    game <- newIORef initGame
    -- windowSize $= Size _INITIAL_WIDTH _INITIAL_HEIGHT
    displayCallback $= display game
    if length args == 0
      then fullScreen
      else do
        print $ args !! 0
        when ((args !! 0) == "-w") $ do
          windowSize $= Size (read $ args !! 1) (read $ args !! 2)

    keyboardMouseCallback $= Just (keyboard game)
    reshapeCallback $= Just (reshape game)
    addTimerCallback frameRate $ timer game
    mainLoop

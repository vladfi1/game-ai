
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ThreesGui where

import Control.Monad
import Control.Monad.Loops

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Control.Concurrent
import Data.Maybe

import qualified Data.Map as Map

import NewGame
import Threes
import System.IO.Unsafe


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
--main :: IO ()
--main = startGUI defaultConfig setup

guiInited :: MVar Bool
guiInited =  unsafePerformIO $ newMVar False 

userInput :: MVar Int
userInput =  unsafePerformIO newEmptyMVar

getUserKeyEvent :: IO (String)
getUserKeyEvent = do
  key <- iterateWhile (\key -> not ( (key >= 37) && (key <=40) )) $ (takeMVar userInput )
  
  return  (case key of 
    37 -> "Left"
    38 -> "Up"
    39 -> "Right"
    40 -> "Down")




guiHumanPlayer :: GameState ThreesState -> IO (GameState ThreesState)
guiHumanPlayer game @ Player {actions}  = do
  inited <- tryTakeMVar guiInited
  if isJust inited then do
    putStrLn "Initing"
    putStrLn $ show inited
    forkIO initGUI
    return ()
  else do
    return () 

  print game
  line <- getUserKeyEvent
  putStrLn $ "Got Line " ++ line
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

onkeydown :: Handler Int
onkeydown = \key -> do
      putStrLn "GOT KEY DOWN"
      putStrLn $ show key
      tryPutMVar userInput key
      return ()

initGUI = startGUI defaultConfig setup
setup :: Window -> UI ()
setup window = do
    liftIO $ putStrLn "SETUP WINDOW"
    threesDir <- loadDirectory "2048-HTML"
    customJSuri <- loadFile "text/javascript" "2048-HTML/custom.js"
    
    body <- getBody window
    liftIO $ do -- UI()
      putStrLn "registering"
      registerAction <- register (UI.keydown body) onkeydown 
      registerAction

    runFunction $ ffi trampolineCode customJSuri threesDir

trampolineCode = unlines ["var xhr= new XMLHttpRequest()",
  "xhr.open('GET', %1, true)",
  "console.log(%1)",
  "xhr.onreadystatechange= function() {",
  "if (this.readyState!=4) {",
  "console.log('readyState = ' + this.readyState)",
  "return",
  "}",
  "if (this.status!=200) {",
  "return",
  "}",
  "var head= document.getElementsByTagName('head')[0];",
  "var script= document.createElement('script');",
  "script.type= 'text/javascript';",
  "script.innerHTML = this.responseText;",
  "head.appendChild(script);",
  "}",
  "window.onLoad = function() {loadThrees(%2);};",
  "xhr.send()"]

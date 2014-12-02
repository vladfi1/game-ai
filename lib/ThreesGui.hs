
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module ThreesGui where

import Control.Monad

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

--userInput :: IO (MVar Int)
--userInput =  newEmptyMVar

initGUI = startGUI defaultConfig setup


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
  --putStrLn "GUI INITIED"
  --userInputMVar <- userInput
  --
  --key <- takeMVar userInputMVar 
  --putStrLn $ show key
  --putStrLn "GOT USER INPUT"
  line <- getLine
  let tile = read line :: Direction
  return $ (Map.fromList actions) Map.! tile

  --return game

--onkeydown :: Handler Int
--onkeydown = \key -> do
--      putStrLn "GOT KEY DOWN"
--      putStrLn $ show key
--      userInputMVar <- userInput
--      tryPutMVar userInputMVar key
--      return ()

setup :: Window -> UI ()
setup window = do
    liftIO $ putStrLn "SETUP WINDOW"
    
    threesDir <- loadDirectory "Threes-HTML"
    customJSuri <- loadFile "text/javascript" "Threes-HTML/custom.js"
    
    body <- getBody window
    --liftIO $ do -- UI()
    --  --register :: IO (IO())
    --  registerAction <- register (UI.keydown body) onkeydown 
    --  registerAction
    
    --liftIO $ putStrLn "REGISTERED"
    callFunction $ ffi trampolineCode customJSuri threesDir

--guiGetAction = do 
--  action <- callFunction $ ffi "waitOnUserAction()"
--  return $ turnToDirection action

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

trampoline :: String -> String -> JSFunction String --customJSUrl -> threesDir
trampoline = ffi $ trampolineCode

buildGrid :: Double -> Double -> Double -> [(Double, Double, Double, Double, String, String)]
buildGrid nrows ncols size= [ (0, 0, size, size, "red", "1"),
  (size, 0, size, size, "blue", "2"),
  (0, size, size, size, "blue", ""),
  (size, size, size, size, "red", "3")]


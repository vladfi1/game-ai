
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
import qualified Data.Matrix as Matrix 

import NewGame
import Threes
import qualified Threes
import System.IO.Unsafe


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
--main :: IO ()
--main = startGUI defaultConfig setup

keyToDirection :: Int -> String
keyToDirection key =  (case key of 
      37 -> "Left"
      38 -> "Up"
      39 -> "Right"
      40 -> "Down"
      otherwise -> "POOP")

makeGuiHumanPlayer :: IO (MVar Int) -> IO (MVar (GameState ThreesState)) -> IO (GameState ThreesState -> IO (GameState ThreesState))
makeGuiHumanPlayer threepennyToMainIO mainToThreePennyIO = do

  putStrLn "Initing"
  userInput <- threepennyToMainIO
  gamestateChannel <- mainToThreePennyIO 

  forkIO $ initGUI userInput gamestateChannel
  
  let between a b c = a <= c && c <= b
  let print gamestate = putMVar gamestateChannel gamestate
  let getUserInput =  fmap keyToDirection $ iterateUntil (between 37 40) (takeMVar userInput)
  
  --guiHumanPlayer :: GameState ThreesState ->
  return $ \gamestate -> do
    print gamestate
    line <- getUserInput
    putStrLn $ "Got Line " ++ show line
    let tile = read line :: Direction
    return $ (Map.fromList (actions gamestate)) Map.! tile


onkeydown :: MVar Int -> Handler Int
onkeydown userInput key = do
      putStrLn "GOT KEY DOWN"  
      putStrLn $ show key 
      tryPutMVar userInput key
      return ()


gamestateToStrArr :: GameState ThreesState ->  String
gamestateToStrArr = Matrix.prettyMatrix . Threes.grid . state  

initGUI :: MVar Int -> MVar (GameState ThreesState) -> IO ()
initGUI userInput newStateChan = startGUI defaultConfig $ setup userInput newStateChan

setup :: MVar Int -> MVar (GameState ThreesState) -> Window ->  UI ()
setup userInput newStateChan window = do
    liftIO $ putStrLn "SETUP WINDOW"
    threesDir <- loadDirectory "2048-HTML"
    customJSuri <- loadFile "text/javascript" "2048-HTML/custom.js"
    
    body <- getBody window
    runFunction $ ffi trampolineCode customJSuri threesDir

    liftIO $ do  --listen for key events
      putStrLn "registering"
      registerAction <- register (UI.keydown body) $ onkeydown userInput
      registerAction
    
    eventTup <- liftIO newEvent 
    let (writeBoardEvent, triggerWriteBoardEvent) = eventTup

    onEvent writeBoardEvent $ \gamestate -> do
      liftIO $ putStrLn $ "GOT WRITEBOARD EVENT\n" ++ (gamestateToStrArr gamestate)
      runFunction $ ffi "writeGameState(%1);" (gamestateToStrArr gamestate)

    liftIO $ forkIO $ do 
      threadDelay 2000000 --a hack, a new gamestate is triggered before threes-html loads. Wait 2 seconds before listening for events
      forever $ do --seperate thread for writing the gamestate
        putStrLn "waiting on new state" 
        gamestate <- takeMVar newStateChan
        putStrLn "got new state" 
        triggerWriteBoardEvent gamestate

    return ()

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

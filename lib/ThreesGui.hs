
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
import Prelude  hiding (Left, Right)


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
--main :: IO ()
--main = startGUI defaultConfig setup

keyToDirection :: Int -> Direction 
keyToDirection key =  (case key of 
      37 -> Left
      38 -> Up
      39 -> Right
      40 -> Down)

makeGuiHumanPlayer :: IO (GameState ThreesState -> IO (Direction))
makeGuiHumanPlayer  = do

  putStrLn "Initing"
  userInput <- newEmptyMVar
  gamestateChannel <- newEmptyMVar 

  forkIO $ initGUI userInput gamestateChannel
  
  let between a b c = (a <= c) && (c <= b)
  let toThreePenny gamestate = putMVar gamestateChannel gamestate
  let validDirections gamestate = map fst (actions gamestate)

  return $ \gamestate -> do
    
    let validDirections = map fst (actions gamestate)
    let isValidInput key = (between 37 40 key) && ((keyToDirection key) `elem` validDirections)
    let getUserInput =  fmap keyToDirection $ (iterateUntil (isValidInput) (takeMVar userInput)) 

    toThreePenny gamestate
    direction <- getUserInput
    putStrLn $ "Got Direction " ++ show direction
    return direction


onkeydown :: MVar Int -> Handler Int
onkeydown userInput key = do
      tryPutMVar userInput key
      return ()


gamestateToStrArr :: GameState ThreesState ->  String
gamestateToStrArr = Matrix.prettyMatrix . Threes.grid . state  

initGUI :: MVar Int -> MVar (GameState ThreesState) -> IO ()
initGUI userInput newStateChan = startGUI defaultConfig $ setup userInput newStateChan

gamestateToBrowser :: String -> Int -> JSFunction () 
gamestateToBrowser =  ffi "writeGameState(%1, %2);" --write gamestate to 

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
      runFunction $ gamestateToBrowser (gamestateToStrArr gamestate) ((nextTile . state) gamestate)
      liftIO $ putStrLn $ "EMITTED WRITEBOARD EVENT\n"

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

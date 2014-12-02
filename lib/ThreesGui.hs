module ThreesGui where

import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
--main :: IO ()
--main = startGUI defaultConfig setup
initGUI = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    
    threesDir <- loadDirectory "Threes-HTML"
    customJSuri <- loadFile "text/javascript" "Threes-HTML/custom.js"
    
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



import Control.Monad

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Threes js"

    canvas <- UI.canvas
        # set UI.height 1000
        # set UI.width  1000

    getBody window #+ [element canvas]

    let squareSize = 100
    let rects = buildGrid 1 2 squareSize

    let drawRect (x,y,w,h,color,text) = do
          element canvas # set UI.fillStyle (UI.htmlColor color)
          UI.fillRect (x,y) w h canvas
          element canvas # set UI.fillStyle (UI.htmlColor "black")
          element canvas # set UI.textFont  "50px Arial"
          UI.fillText text (x + squareSize/3, y + squareSize/2) canvas

    forM_ rects drawRect



buildGrid :: Double -> Double -> Double -> [(Double, Double, Double, Double, String, String)]
buildGrid nrows ncols size= [ (0, 0, size, size, "red", "1"),
  (size, 0, size, size, "blue", "2"),
  (0, size, size, size, "blue", ""),
  (size, size, size, size, "red", "3")]


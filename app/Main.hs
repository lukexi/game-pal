module Main where

import Game.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL

main :: IO ()
main = do
  (window, events, maybeHMD, maybeRenderHMD, maybeSixenseBase) <- initWindow "GamePal" True False
  whileWindow window $ do
    processEvents events $ \_ -> return ()


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal hiding (getNow)
import Data.Time


main :: IO ()
main = do

  vrPal@VRPal{..} <- initVRPal "VR Pal" [UseOpenVR]
  
  whileVR vrPal $ \headM44 hands events -> do

    let pulse = do
          now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getNow vrPal
          glClearColor 0.2 0.1 (now * 0.3) 1

    renderWith vrPal newPose headM44  
      (pulse >> glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (\projMat viewM44 -> return ())

    processEvents gpEvents $ \e -> closeOnEscape gpWindow e

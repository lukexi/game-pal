{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Data.Time

main :: IO ()
main = do

  vrPal@VRPal{..} <- initVRPal "VR Pal" [UseOpenVR]


  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  whileWindow gpWindow $ do

    hands <- getHands vrPal

    let viewMat = viewMatrixFromPose newPose
        pulse = do
          now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getCurrentTime
          glClearColor 0.2 0.1 (now * 0.3) 1
    renderWith vrPal viewMat 
      (pulse >> glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (\projMat viewMat -> return ())

    processEvents gpEvents $ \e -> closeOnEscape gpWindow e

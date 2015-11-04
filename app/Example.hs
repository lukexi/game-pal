{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear.Extra
import Data.Maybe
import Data.Time

import Halive.Utils

main :: IO ()
main = do

  gamePal@VRPal{..} <- initVRPal "GamePal" GCPerFrame [UseOpenVR]


  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  whileWindow gpWindow $ do


    hands <- getHands gamePal

    let viewMat = viewMatrixFromPose newPose
        pulse = do
          now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getCurrentTime
          glClearColor 0.2 0.1 (now * 0.3) 1
    renderWith gamePal viewMat 
      (pulse >> glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (\projMat viewMat -> return ())

    processEvents gpEvents $ \e -> closeOnEscape gpWindow e

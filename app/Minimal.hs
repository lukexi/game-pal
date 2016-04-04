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
    
    whileWindow gpWindow $ do
        (headM44, _vrEvents) <- tickVR vrPal identity

        let pulseColor = do
                now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getNow vrPal
                glClearColor 0.2 0.1 (now * 0.3) 1
    
        renderWith vrPal headM44 $ \projMat viewM44 -> do 
            pulseColor
            glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    
        processEvents gpEvents $ \e -> closeOnEscape gpWindow e

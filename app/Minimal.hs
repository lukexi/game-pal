{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal hiding (getNow)
import Data.Time
import Control.Monad.Trans
import Control.Monad

main :: IO ()
main = do

    vrPal@VRPal{..} <- initVRPal "VR Pal" [UseOpenVR]
    
    whileWindow gpWindow $ do
        (headM44, events) <- tickVR vrPal identity

        let setPulseColor = do
                now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getNow vrPal
                glClearColor 0.2 0.1 (now * 0.3) 1
    
        renderWith vrPal headM44 $ \projMat viewM44 -> do 
            --liftIO $ print =<< getNow vrPal
            setPulseColor
            glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    
        forM_ events $ \case
            GLFWEvent e -> closeOnEscape gpWindow e
            _ -> return ()

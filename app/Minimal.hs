{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.VR.Pal
import Graphics.GL.Pal hiding (getNow)
import Data.Time

main :: IO ()
main = do

    vrPal@VRPal{..} <- initVRPal "VR Pal"

    whileWindow vrpWindow $ \windowEvents -> do
        (headM44, events) <- tickVR vrPal identity windowEvents

        let setPulseColor = do
                now <- (/ 2) . (+ 1) . sin . realToFrac . utctDayTime <$> getNow vrPal
                glClearColor 0.2 0.1 (now * 0.3) 1

        renderWith vrPal headM44 $ \_projMat _viewM44 _projRaw _viewport -> do
            --liftIO $ print =<< getNow vrPal
            setPulseColor
            glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

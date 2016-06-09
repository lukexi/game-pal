{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.VR.Pal.Emulation where
import Graphics.UI.GLFW.Pal
import Control.Monad

import Control.Monad.Trans
import Linear.Extra
import Control.Lens.Extra
import Graphics.VR.Pal.Types
import Graphics.VR.Pal.Hands
import Graphics.GL.Pal
import Data.IORef

emulateRightHand :: (MonadIO m) => VRPal -> M44 GLfloat -> [Event] -> m [VRPalEvent]
emulateRightHand VRPal{..} playerM44 events = do

    projM44     <- getWindowProjection gpWindow 45 0.1 1000
    mouseRay    <- cursorPosToWorldRay gpWindow projM44 (poseFromMatrix playerM44)
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \e ->
        onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandDepthRef (+ (y*0.1))
    handZ <- liftIO (readIORef gpEmulatedHandDepthRef)
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
    let handPosition = projectRay mouseRay handZ
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handMatrix   = transformationFromPose $ newPose
                                              & posPosition .~ handPosition
                                              & posOrientation .~ axisAngle (V3 0 1 0) a
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip

    let handStateEvent = HandEvent RightHand (HandStateEvent rightHand)
    return (map VREvent [handStateEvent])

emulateRightHandVR :: (MonadIO m) => VRPal -> Pose Float -> [VRPalEvent] -> m [Hand]
emulateRightHandVR VRPal{..} _player events = do
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \case
        GLFWEvent e -> onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandDepthRef (+ (y*0.1))
        _ -> return ()
    z <- liftIO (readIORef gpEmulatedHandDepthRef)

    (fromIntegral -> w, fromIntegral -> h) <- getWindowSize gpWindow
    (x, y) <- getCursorPos gpWindow
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handPosition = V3 (-x/w * 0.1) z (-y/h * 0.1)
        handMatrix   = transformationFromPose $ newPose
                                              & posPosition .~ handPosition
                                              & posOrientation .~ axisAngle (V3 0 1 0) a
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]

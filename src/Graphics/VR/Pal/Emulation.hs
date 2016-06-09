{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.VR.Pal.Emulation where
import Graphics.UI.GLFW.Pal
import Control.Monad
import Control.Arrow

import Control.Monad.Trans
import Linear.Extra
import Control.Lens.Extra
import Graphics.VR.Pal.Types
import Graphics.VR.Pal.Hands
import Graphics.GL.Pal
import Data.IORef

-- | Projects from the mouse cursor position in the window to a fixed depth controlled by scroll.
-- Allows controlling on a regular 2D screen.
emulateRightHandScreen :: (MonadIO m) => VRPal -> M44 GLfloat -> [Event] -> m [VRPalEvent]
emulateRightHandScreen VRPal{..} playerM44 events = do

    projM44     <- getWindowProjection gpWindow 45 0.1 1000
    mouseRay    <- cursorPosToWorldRay gpWindow projM44 (poseFromMatrix playerM44)
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    let modifyHand = liftIO . modifyIORef' gpEmulatedHandRef
        readHand   = liftIO (readIORef gpEmulatedHandRef)


    whenKeyPressed gpWindow Key'Z $ modifyHand (second (_x +~ 0.005))
    whenKeyPressed gpWindow Key'X $ modifyHand (second (_x -~ 0.005))

    handEvents <- fmap concat . forM events $ \e -> do
        onScroll e $ \_x y ->
            modifyHand (first (+ (y*0.1)))


        return $ case e of
            (MouseButton MouseButton'1 MouseButtonState'Pressed  _) -> [HandEvent RightHand (HandButtonEvent HandButtonTrigger ButtonDown) ]
            (MouseButton MouseButton'1 MouseButtonState'Released _) -> [HandEvent RightHand (HandButtonEvent HandButtonTrigger ButtonUp)   ]
            _ -> []


    (handZ, handEuler) <- readHand


    let handPosition = projectRay mouseRay handZ
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handMatrix   = mkTransformation (eulerToQuat handEuler) handPosition
        rightHand    = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip

    let handStateEvent = HandEvent RightHand (HandStateEvent rightHand)
    return (map VREvent (handStateEvent:handEvents))

-- | Mouse cursor moves in XZ (no window-to-world mapping), scrolling controls Y
-- Allows controlling via mouse on a VR setup with no hand controllers.
emulateRightHandVR :: (MonadIO m) => VRPal -> M44 GLfloat -> [Event] -> m [Hand]
emulateRightHandVR VRPal{..} playerM44 events = do
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \e ->
        onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandRef (first (+ (y*0.1)))

    z <- fst <$> liftIO (readIORef gpEmulatedHandRef)

    (fromIntegral -> w, fromIntegral -> h) <- getWindowSize gpWindow
    (x, y) <- getCursorPos gpWindow
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handPosition = V3 (-x/w * 0.1) z (-y/h * 0.1)
        handMatrix   = playerM44 !*! mkTransformation (axisAngle (V3 0 1 0) a) handPosition
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]

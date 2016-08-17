{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Graphics.VR.Pal.Emulation where
import Control.Monad
import Control.Arrow
import Graphics.VR.Pal.SDLUtils
import SDL

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

    projM44     <- getWindowProjection vrpWindow 45 0.1 1000
    mouseRay    <- cursorPosToWorldRay vrpWindow projM44 (poseFromMatrix playerM44)

    mouseButtonStates <- getMouseButtons
    let mouse1Down = mouseButtonStates ButtonLeft
        mouse2Down = mouseButtonStates ButtonRight

    let modifyHand = liftIO . modifyIORef' vrpEmulatedHandRef
        readHand   = liftIO (readIORef vrpEmulatedHandRef)


    whenKeyPressed ScancodeZ $ modifyHand (second (_x +~ 0.005))
    whenKeyPressed ScancodeX $ modifyHand (second (_x -~ 0.005))
    whenKeyPressed ScancodeC $ modifyHand (second (_y +~ 0.005))
    whenKeyPressed ScancodeV $ modifyHand (second (_y -~ 0.005))

    handEvents <- fmap concat . forM events $ \e -> do
        onScroll e $ \(V2 _x y) ->
            modifyHand (first (+ (y*0.1)))


        return $ case eventPayload e of
            (MouseButtonEvent (MouseButtonEventData {
                mouseButtonEventMotion = Pressed, mouseButtonEventButton = ButtonLeft})) ->
                    [HandEvent RightHand (HandButtonEvent HandButtonTrigger ButtonDown) ]
            (MouseButtonEvent (MouseButtonEventData {
                mouseButtonEventMotion = Released, mouseButtonEventButton = ButtonLeft})) ->
                    [HandEvent RightHand (HandButtonEvent HandButtonTrigger ButtonUp) ]
            _ -> []


    (handZ, handEuler) <- readHand


    let handPosition = projectRay mouseRay handZ
        trigger      = if mouse1Down then 1 else 0
        grip         = mouse2Down
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

    mouseButtonStates <- getMouseButtons
    let mouse1Down = mouseButtonStates ButtonLeft
        mouse2Down = mouseButtonStates ButtonRight


    forM_ events $ \e ->
        onScroll e $ \(V2 _x y) ->
            liftIO $ modifyIORef' vrpEmulatedHandRef (first (+ (y*0.1)))

    z <- fst <$> liftIO (readIORef vrpEmulatedHandRef)

    V2 w h <- fmap fromIntegral <$> get (windowSize vrpWindow)
    (_, P (fmap fromIntegral -> V2 x y)) <- getModalMouseLocation
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
        trigger      = if mouse1Down then 1 else 0
        grip         = mouse2Down
        handPosition = V3 (-x/w * 0.1) z (-y/h * 0.1)
        handMatrix   = playerM44 !*! mkTransformation (axisAngle (V3 0 1 0) a) handPosition
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]

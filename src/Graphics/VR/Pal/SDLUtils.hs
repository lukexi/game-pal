{-# LANGUAGE ViewPatterns #-}
module Graphics.VR.Pal.SDLUtils where
import SDL
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Linear.Extra
import Control.Lens.Extra
createGLWindow :: MonadIO m => Text -> m Window
createGLWindow windowName = do
    initialize
        [ InitVideo
        , InitEvents
        -- , InitJoystick , InitGameController
        ]
    window <- createWindow windowName defaultWindow
        { windowOpenGL = Just $ defaultOpenGL
            { glProfile = Core Normal 4 4
            }
        }
    glContext <- glCreateContext window
    glMakeCurrent window glContext
    swapInterval $= ImmediateUpdates
    return window


whileWindow :: MonadIO m => Window -> ([Event] -> m a) -> m ()
whileWindow window action = do
    let loop = do
            events <- pollEvents
            _ <- action events
            let shouldQuit = (QuitEvent `elem`) $ map eventPayload events
            unless shouldQuit loop
    loop
    destroyWindow window

whenKeyPressed :: MonadIO m => Scancode -> m () -> m ()
whenKeyPressed scanCode action = do
    scanCodeState <- getKeyboardState
    when (scanCodeState scanCode) action

isShiftDown :: MonadIO m => m Bool
isShiftDown = do
    modState <- getModState
    return (keyModifierLeftShift modState || keyModifierRightShift modState)


onScroll :: (Monad m) => Event -> (V2 Float -> m ()) -> m ()
onScroll (Event {eventPayload =
    (MouseWheelEvent (MouseWheelEventData
        { mouseWheelEventPos = amount } )) }) f
    = f (realToFrac <$> amount)
onScroll _                 _ = return ()


-- | Use the aspect ratio from the window to get a proper projection
getWindowProjection :: (Floating a, MonadIO m) => Window -> a -> a -> a -> m (M44 a)
getWindowProjection win fov near far = do
    V2 w h <- get (windowSize win)
    return $ perspective fov (fromIntegral w / fromIntegral h) near far

windowPosToWorldRay :: (MonadIO m)
                    => Window
                    -> M44 Float
                    -> Pose Float
                    -> V2 Int
                    -> m (Ray Float)
windowPosToWorldRay win proj pose coord = do
    winSize <- get (windowSize win)
    let V2 xNDC yNDC = win2Ndc coord winSize
        start        = ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
        end          = ndc2Wld (V4 xNDC yNDC 0.0    1.0)
        dir          = normalize (end ^-^ start)
    return (Ray start dir)

    where -- Converts from window coordinates (origin top-left) to normalized device coordinates
      win2Ndc (fmap fromIntegral -> V2 x y) (fmap fromIntegral -> V2 w h) =
        V2
            ((x / w        - 0.5) * 2.0)
            (((h - y) / h - 0.5) * 2.0)
      -- Converts from normalized device coordinates to world coordinates
      ndc2Wld i = hom2Euc (invViewProj !* i)
      -- Converts from homogeneous coordinates to Euclidean coordinates
      hom2Euc v = (v ^/ (v ^. _w)) ^. _xyz
      invViewProj = inv44 (proj !*! viewMatrixFromPose pose)

cursorPosToWorldRay :: (MonadIO m)
                    => Window
                    -> M44 Float
                    -> Pose Float
                    -> m (Ray Float)
cursorPosToWorldRay win proj pose = do
    (_, P cursorPos) <- getModalMouseLocation
    windowPosToWorldRay win proj pose (fromIntegral <$> cursorPos)


-- | Pass this to glViewport to get the correct size on Retina Macs and normal Windows
-- FIXME: check if we use HighDPI and double the value here?
getWindowViewport :: (Num a, MonadIO m) => Window -> m (a, a, a, a)
getWindowViewport win = do
    V2 w h <- get (windowSize win)
    return (0, 0, fromIntegral w, fromIntegral h)

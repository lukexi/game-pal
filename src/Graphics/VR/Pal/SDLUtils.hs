{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Graphics.VR.Pal.SDLUtils where
import SDL
import SDL.Internal.Types
import qualified SDL.Raw.Video as Raw
import Foreign.C
import Foreign.Ptr
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as Text
import Linear.Extra
import Linear.Affine
import Control.Lens.Extra
import Data.List (sort)
import Foreign

#if defined(mingw32_HOST_OS)
foreign import ccall "win32_SetProcessDpiAware" win32_SetProcessDpiAware :: IO Bool
#endif

glProfileVersion :: Profile
#if defined(darwin_HOST_OS)
glProfileVersion = Core Normal 4 1
#else
glProfileVersion = Core Normal 4 4
#endif

createGLWindow :: MonadIO m => Text -> m Window
createGLWindow windowName = do
    initialize
        [ InitVideo
        , InitEvents
        -- , InitJoystick , InitGameController
        ]

    -- Make Windows treat us as HighDPI aware
#if defined(mingw32_HOST_OS)
    _ <- liftIO win32_SetProcessDpiAware
#endif

    window <- createWindow windowName defaultWindow
        { windowOpenGL = Just $ defaultOpenGL
            { glProfile = glProfileVersion
            }
        , windowHighDPI = True
        , windowInitialSize = V2 1600 1200
        --, windowPosition = Centered
        , windowPosition = Absolute (P (V2 100 100))
        , windowResizable = True
        }
    glContext <- glCreateContext window
    glMakeCurrent window glContext
    swapInterval $= ImmediateUpdates
    return window

getDrawableSize :: (Num a, MonadIO m) => Window -> m (V2 a)
getDrawableSize (Window window) =
    liftIO $
    alloca $ \wptr ->
    alloca $ \hptr -> do
      Raw.glGetDrawableSize window wptr hptr
      fmap fromIntegral <$> (V2 <$> peek wptr <*> peek hptr)

getMouseLocationV2 :: (Num a, MonadIO m) => m (V2 a)
getMouseLocationV2 = do
    (_, P loc) <- getModalMouseLocation
    return (fromIntegral <$> loc)

getWindowSizeV2 :: (Num a, MonadIO m) => Window -> m (V2 a)
getWindowSizeV2 window = fmap fromIntegral <$> get (windowSize window)

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
onScroll (Event {eventPayload = MouseWheelEvent
    (MouseWheelEventData
        { mouseWheelEventPos = amount } )
    }) f
    = f (realToFrac <$> amount)
onScroll _                 _ = return ()


onMouseMove :: (Monad m) => Event -> (V2 Float -> m ()) -> m ()
onMouseMove (Event {eventPayload = MouseMotionEvent
    (MouseMotionEventData
        { mouseMotionEventPos = P position } )
    }) f
    = f (realToFrac <$> position)
onMouseMove _                 _ = return ()


onMouse1Down :: (Monad m) => Event -> (m ()) -> m ()
onMouse1Down (Event {eventPayload = MouseButtonEvent
    (MouseButtonEventData {
        mouseButtonEventMotion = Pressed,
        mouseButtonEventButton = ButtonLeft } )
    }) f
    = f
onMouse1Down _                 _ = return ()

onMouse1Up :: (Monad m) => Event -> (m ()) -> m ()
onMouse1Up (Event {eventPayload = MouseButtonEvent
    (MouseButtonEventData {
        mouseButtonEventMotion = Released,
        mouseButtonEventButton = ButtonLeft } )
    }) f
    = f
onMouse1Up _                 _ = return ()

-- | Use the aspect ratio from the window to get a proper projection
getWindowProjection :: (Floating a, MonadIO m) => Window -> a -> a -> a -> m (M44 a)
getWindowProjection win fov near far = do
    V2 w h <- getWindowSizeV2 win
    return $ perspective fov (w / h) near far

windowPosToWorldRay :: (MonadIO m)
                    => Window
                    -> M44 Float
                    -> Pose Float
                    -> V2 Float
                    -> m (Ray Float)
windowPosToWorldRay win proj pose coord = do
    winSize <- getWindowSizeV2 win
    let V2 xNDC yNDC = win2Ndc coord winSize
        start        = ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
        end          = ndc2Wld (V4 xNDC yNDC 0.0    1.0)
        dir          = normalize (end ^-^ start)
    return (Ray start dir)

    where -- Converts from window coordinates (origin top-left) to normalized device coordinates
      win2Ndc (V2 x y) (V2 w h) =
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
    cursorPos <- getMouseLocationV2
    windowPosToWorldRay win proj pose cursorPos


-- | Pass this to glViewport to get the correct size on Retina Macs and normal Windows
getWindowViewport :: (Num a, MonadIO m) => Window -> m (a, a, a, a)
getWindowViewport win = do
    V2 w h <- getDrawableSize win
    return (0, 0, w, h)


data ModKey = ModKeyShift
            | ModKeyControl
            | ModKeyAlt
            | ModKeySuper
            deriving (Eq, Show, Ord)






modKeysFromBools :: KeyModifier -> [ModKey]
modKeysFromBools KeyModifier{..} = sort $
    concat [ [ModKeyShift   | keyModifierLeftShift || keyModifierRightShift]
           , [ModKeyControl | keyModifierLeftCtrl  || keyModifierRightCtrl]
           , [ModKeyAlt     | keyModifierLeftAlt   || keyModifierRightAlt]
           , [ModKeySuper   | keyModifierLeftGUI   || keyModifierRightGUI]
           ]

matchModKeys :: KeyModifier -> [ModKey] -> Bool
matchModKeys modKeyBools modKeys = sort (modKeysFromBools modKeyBools) == sort modKeys

ifKeyWithMods :: Monad m => a -> Event -> [ModKey] -> Keycode ->  m a -> m a
ifKeyWithMods _ (Event {eventPayload = KeyboardEvent
    (KeyboardEventData {
        keyboardEventKeyMotion = Pressed,
        keyboardEventKeysym = Keysym
            { keysymModifier = modifierKeyBools
            , keysymKeycode = keycode
            }
    })
    }) modKeys key action
    |     keycode == key
       && matchModKeys modifierKeyBools modKeys
    = action
ifKeyWithMods a _ _ _ _ = return a

ifKeyUp :: Monad m => a -> Event -> Keycode -> m a -> m a
ifKeyUp _ (Event {eventPayload = KeyboardEvent
    (KeyboardEventData {
            keyboardEventKeyMotion = Released,
            keyboardEventKeysym = Keysym
                { keysymKeycode = keycode
                }
    })
    }) key action
    | keycode == key = action
ifKeyUp a _ _ _ = return a

-- | If the event matches the key, run the action.
ifKeyDown :: Monad m => a -> Event -> Keycode -> m a -> m a
ifKeyDown _ (Event {eventPayload = KeyboardEvent
    (KeyboardEventData {
            keyboardEventKeyMotion = Pressed,
            keyboardEventKeysym = Keysym
                { keysymKeycode = keycode
                }
    })
    }) key action
    | keycode == key = action
ifKeyDown a _ _ _ = return a

ifKey :: Monad m => a -> Event -> Keycode -> m a -> m a
ifKey a key = ifKeyWithMods a key []


ifText :: Monad m => a -> Event -> (Text -> m a) -> m a
ifText _ (Event {eventPayload = TextInputEvent
    (TextInputEventData { textInputEventText = text })
    }) f = f text
ifText a _             _ = return a


onKeyWithMods :: Monad m => Event -> [ModKey] -> Keycode -> m () -> m ()
onKeyWithMods = ifKeyWithMods ()

onKeyUp :: Monad m => Event -> Keycode -> m () -> m ()
onKeyUp = ifKeyUp ()

onText :: Monad m => Event -> (Text -> m ()) -> m ()
onText = ifText ()

-- | If the event matches the key, run the action.
onKeyDown :: Monad m => Event -> Keycode -> m () -> m ()
onKeyDown = ifKeyDown ()

onKey :: Monad m => Event -> Keycode -> m () -> m ()
onKey = ifKey ()

-- | Warning: does not include a valid Scancode, Window, or Timestamp
fakeKeycodePressedEvent keycode mods = fakeEvent . KeyboardEvent $
    KeyboardEventData
        { keyboardEventWindow = Window nullPtr
        , keyboardEventKeyMotion = Pressed
        , keyboardEventKeysym = Keysym
            { keysymKeycode = keycode
            , keysymScancode = Scancode 0
            , keysymModifier = keyModifierFromModkeys mods
            }
        }

fakeTextInputEventFromChar char = fakeEvent . TextInputEvent $
    TextInputEventData
        { textInputEventWindow = Window nullPtr
        , textInputEventText = Text.pack [char]
        }


fakeEvent payload = (Event {eventTimestamp = 0, eventPayload = payload })

keyModifierFromModkeys modkeys = KeyModifier
    { keyModifierLeftShift  = ModKeyShift `elem` modkeys
    , keyModifierRightShift = False
    , keyModifierLeftCtrl   = ModKeyControl `elem` modkeys
    , keyModifierRightCtrl  = False
    , keyModifierLeftAlt    = ModKeyAlt `elem` modkeys
    , keyModifierRightAlt   = False
    , keyModifierLeftGUI    = ModKeySuper `elem` modkeys
    , keyModifierRightGUI   = False
    , keyModifierNumLock    = False
    , keyModifierCapsLock   = False
    , keyModifierAltGr      = False
    }

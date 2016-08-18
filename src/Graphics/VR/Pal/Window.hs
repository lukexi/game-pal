{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Graphics.VR.Pal.Window where
import Graphics.VR.Pal.SDLUtils
import Graphics.VR.OpenVR
import Control.Monad

import Control.Monad.Trans
import Linear.Extra
import Linear.Affine
import Graphics.VR.Pal.Types
import Graphics.VR.Pal.Hands
import Graphics.VR.Pal.Emulation
import Graphics.GL.Pal
import Data.Maybe
import Data.Time
import Data.IORef
import SDL
import Data.Text (Text)

initVRPal :: Text -> IO VRPal
initVRPal windowName = initVRPalWithConfig windowName defaultVRPalConfig

initVRPalWithConfig :: Text -> VRPalConfig -> IO VRPal
initVRPalWithConfig windowName VRPalConfig{..} = do

    window <- createGLWindow windowName
    --swapInterval $= SwapImmediate
    glClear GL_COLOR_BUFFER_BIT
    glSwapWindow window


    (hmdType, isRoomScale) <- if
        | UseOpenVR `elem` vpcDevices -> do
            hmdPresent <- isHMDPresent
            let openVRConfig = defaultOpenVRConfig { ovcMSAASamples = vpcMSAASamples }
            mOpenVR <- if hmdPresent then createOpenVRWithConfig openVRConfig else return Nothing
            case mOpenVR of
                Just openVR -> do
                    -- Disable the cursor
                    --setCursorInputMode window CursorInputMode'Disabled

                    forM_ (ovrEyes openVR) $ \eye -> case eiEye eye of
                        LeftEye -> do
                            let (_, _, w, h) = eiViewport eye
                            windowSize window $= V2 (fromIntegral w) (fromIntegral h)
                            setWindowPosition window (Absolute (P (V2 0 0)))
                            return ()
                        _ -> return ()

                    -- Check if we're in the Vive or an Oculus by looking for lighthouses
                    roomScale <- isUsingLighthouse (ovrSystem openVR)

                    return (OpenVRHMD openVR, if roomScale then RoomScale else NotRoomScale)
                Nothing -> return (NoHMD, NotRoomScale)
        | otherwise -> return (NoHMD, NotRoomScale)

    glClear GL_COLOR_BUFFER_BIT
    glSwapWindow window

    start    <- getCurrentTime
    timeRef  <- newIORef start
    deltaRef <- newIORef 0

    emulatedHandDepthRef <- newIORef (1, V3 0 0 0)

    return VRPal
        { vrpWindow          = window
        --, vrpThreadWindow    = threadWin
        , vrpHMD             = hmdType
        , vrpTimeRef         = timeRef
        , vrpDeltaRef        = deltaRef
        , vrpRoomScale       = isRoomScale
        , vrpEmulatedHandRef = emulatedHandDepthRef
        }

getDeltaTime :: MonadIO m => VRPal -> m NominalDiffTime
getDeltaTime VRPal{..} = liftIO (readIORef vrpDeltaRef)

getNow :: MonadIO m => VRPal -> m UTCTime
getNow VRPal{..} = liftIO (readIORef vrpTimeRef)

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

tickVR :: MonadIO m => VRPal -> M44 GLfloat -> [Event] -> m (M44 GLfloat, [VRPalEvent])
tickVR vrPal@VRPal{..} playerM44 windowEvents = do
    let winEvents = map WindowEvent windowEvents

    tickDelta vrPal

    case vrpHMD of
        OpenVRHMD openVR@OpenVR{..} -> do
            events                       <- pollNextEvent ovrSystem
            (headM44Raw, handM44sByRole) <- waitGetPoses openVR
            let headM44 = playerM44 !*! headM44Raw

            hands <- catMaybes <$> forM handM44sByRole (\(controllerRole, handM44Raw) -> do
                forM (trackedControllerRoleToWhichHand controllerRole) $ \whichHand -> do
                    buttonStates <- getControllerState ovrSystem controllerRole
                    let handM44 = playerM44 !*! handM44Raw
                    return $ HandEvent whichHand (HandStateEvent (handFromOpenVRController handM44 buttonStates))
                )

            let vrEvents = HeadEvent headM44 : hands ++ (catMaybes $ map vrEventFromOpenVREvent events)

            return (headM44, winEvents ++ map VREvent vrEvents)
        _ -> do
            emulatedVREvents <- emulateRightHandScreen vrPal playerM44 windowEvents
            return (playerM44, winEvents ++ emulatedVREvents)

renderWith :: MonadIO m
           => VRPal
           -> M44 GLfloat
           -> (M44 GLfloat -> M44 GLfloat -> V4 GLfloat -> V4 GLfloat -> m b)
           -> m ()
renderWith VRPal{..} headM44 eyeRenderFunc = do
    let viewM44 = inv44 headM44
    case vrpHMD of
        NoHMD  -> do
            (x,y,w,h) <- getWindowViewport vrpWindow
            glViewport x y w h
            renderFlat vrpWindow viewM44 (\p fv -> eyeRenderFunc p fv 0 0)
            glSwapWindow vrpWindow
        OpenVRHMD openVR -> do
            renderOpenVR openVR viewM44 eyeRenderFunc

            -- Mirror one eye to the window
            V2 w h <- get (windowSize vrpWindow)
            let oneEye = listToMaybe (ovrEyes openVR)
            forM_ oneEye (\eye -> mirrorOpenVREyeToWindow eye (fromIntegral w) (fromIntegral h))

            glSwapWindow vrpWindow

renderOpenVR :: (MonadIO m)
             => OpenVR
             -> M44 GLfloat
             -> (M44 GLfloat -> M44 GLfloat -> V4 GLfloat -> V4 GLfloat -> m a1)
             -> m ()
renderOpenVR OpenVR{..} viewM44 eyeRenderFunc = do

    -- Render each eye, with multisampling
    forM_ ovrEyes $ \EyeInfo{..} -> do

        -- Will render into mfbResolveTextureID
        withMultisamplingFramebuffer eiMultisampleFramebuffer $ do
            let (x, y, w, h) = eiViewport
                viewport     = realToFrac <$> V4 x y w h
                finalView    = eiEyeHeadTrans !*! viewM44
            glViewport x y w h

            _ <- eyeRenderFunc eiProjection finalView eiProjectionRaw viewport
            return ()

    -- Submit frames after rendering both
    forM_ ovrEyes $ \EyeInfo{..} -> do
        let MultisampleFramebuffer{..} = eiMultisampleFramebuffer
        submitFrameForEye ovrCompositor eiEye (unTextureID mfbResolveTextureID)



renderFlat :: MonadIO m
           => Window -> M44 GLfloat -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderFlat win viewM44 renderFunc = do

    projection  <- getWindowProjection win 45 0.1 1000

    _           <- renderFunc projection viewM44

    return ()

recenterSeatedPose :: MonadIO m => VRPal -> m ()
recenterSeatedPose vrPal = case vrpHMD vrPal of
    OpenVRHMD openVR -> resetSeatedZeroPose (ovrSystem openVR)
    _ -> return ()


fadeVRToColor :: MonadIO m => VRPal -> V4 GLfloat -> GLfloat -> m ()
fadeVRToColor vrPal color fadeTime = case vrpHMD vrPal of
    OpenVRHMD openVR -> fadeCompositorToColor (ovrCompositor openVR) color fadeTime
    _ -> return ()

tickDelta :: MonadIO m => VRPal -> m ()
tickDelta VRPal{..} = liftIO $ do
    lastTime <- readIORef vrpTimeRef
    currTime <- getCurrentTime

    let diffTime = diffUTCTime currTime lastTime

    writeIORef vrpTimeRef currTime
    writeIORef vrpDeltaRef diffTime

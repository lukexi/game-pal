{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Graphics.VR.Pal.Window where
import Graphics.UI.GLFW.Pal
import Graphics.VR.OpenVR
import Control.Monad

import Control.Monad.Trans
import Linear.Extra
import Graphics.VR.Pal.Types
import Graphics.VR.Pal.Hands
import Graphics.GL.Pal
import Data.Maybe
-- import System.Mem
import Data.Time
import Data.IORef
import Control.Concurrent
--import Halive.Utils


initVRPal :: String -> [VRPalDevices] -> IO VRPal
initVRPal windowName devices = do

    -- Calling swapBuffers triggers a ton of dropped frames. Valve's
    -- mirroring doesn't seem to trigger this problem.
    let useSDKMirror = False

    -- Turn off garbage collection per frame when Halive is active,
    -- as it grinds things to a halt (I don't know why)
    --doGCPerFrame <- not <$> isHaliveActive
    let doGCPerFrame = False

    let (resX, resY) = (500, 400)

    (window, threadWin, events) <- createWindow windowName resX resY


    swapInterval 0

    (hmdType, isRoomScale) <- if
        | UseOpenVR `elem` devices -> do
            hmdPresent <- isHMDPresent
            mOpenVR <- if hmdPresent then createOpenVR else return Nothing
            case mOpenVR of
                Just openVR -> do
                    -- Disable the cursor
                    --setCursorInputMode window CursorInputMode'Disabled

                    if useSDKMirror
                        then do
                            showMirrorWindow (ovrCompositor openVR)
                            -- Clear and hide the application window, as we don't display to it
                            glClear GL_COLOR_BUFFER_BIT
                            -- iconifyWindow window
                            -- Need focus to receive keyboard input, so focusing it here
                            restoreWindow window
                            showWindow window
                        else forM_ (ovrEyes openVR) $ \eye -> case eiEye eye of
                            LeftEye -> do
                                let (_, _, w, h) = eiViewport eye
                                setWindowSize window (fromIntegral w) (fromIntegral h)
                                setWindowPosition window 0 0
                                return ()
                            _ -> return ()

                    -- Check if we're in the Vive or an Oculus by looking for lighthouses
                    roomScale <- isUsingLighthouse (ovrSystem openVR)

                    return (OpenVRHMD openVR, if roomScale then RoomScale else NotRoomScale)
                Nothing -> return (NoHMD, NotRoomScale)
        | otherwise -> return (NoHMD, NotRoomScale)

    start    <- getCurrentTime
    timeRef  <- newIORef start
    deltaRef <- newIORef 0

    emulatedHandDepthRef <- newIORef 1

    -- See note in renderWith
    backgroundSwap <- newEmptyMVar
    _ <- forkIO . forever $ do
        action <- takeMVar backgroundSwap
        action

    return VRPal
        { gpWindow               = window
        , gpThreadWindow         = threadWin
        , gpEvents               = events
        , gpHMD                  = hmdType
        , gpTimeRef              = timeRef
        , gpDeltaRef             = deltaRef
        , gpGCPerFrame           = doGCPerFrame
        , gpRoomScale            = isRoomScale
        , gpUseSDKMirror         = case hmdType of { NoHMD -> False; _ -> useSDKMirror }
        , gpEmulatedHandDepthRef = emulatedHandDepthRef
        , gpBackgroundSwap       = backgroundSwap
        }

getDeltaTime :: MonadIO m => VRPal -> m NominalDiffTime
getDeltaTime VRPal{..} = liftIO (readIORef gpDeltaRef)

getNow :: MonadIO m => VRPal -> m UTCTime
getNow VRPal{..} = liftIO (readIORef gpTimeRef)

logIO :: MonadIO m => String -> m ()
logIO = liftIO . putStrLn

tickVR :: MonadIO m => VRPal -> M44 GLfloat -> m (M44 GLfloat, [VRPalEvent])
tickVR vrPal@VRPal{..} playerM44 = do
    winEvents <- map GLFWEvent <$> gatherEvents gpEvents

    tickDelta vrPal

    case gpHMD of
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
        _ ->
            return (playerM44, winEvents)

renderWith :: MonadIO m
           => VRPal
           -> M44 GLfloat
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith VRPal{..} headM44 eyeRenderFunc = do
    let viewM44 = inv44 headM44
    case gpHMD of
        NoHMD  -> do
            (x,y,w,h) <- getWindowViewport gpWindow
            glViewport x y w h
            renderFlat gpWindow viewM44 eyeRenderFunc
            swapBuffers gpWindow
        OpenVRHMD openVR -> do
            renderOpenVR openVR viewM44 eyeRenderFunc
            when (not gpUseSDKMirror) $ do
                (w,h) <- getWindowSize gpWindow
                -- Mirror one eye to the window
                let oneEye = listToMaybe (ovrEyes openVR)
                forM_ oneEye (\eye -> mirrorOpenVREyeToWindow eye (fromIntegral w) (fromIntegral h))

                -- This is a workaround to horrible regular stalls when calling swapBuffers on the main thread.
                -- We use a one-slot MVar rather than a channel to avoid any memory leaks if the background
                -- thread can't keep up - it's not important to update the mirror window on any particular
                -- schedule as long as it happens semi-regularly.
                void . liftIO $ tryPutMVar gpBackgroundSwap (swapBuffers gpWindow)




    -- when gpGCPerFrame $
    --   profile "GC" 0 $ liftIO performMinorGC

renderOpenVR :: (MonadIO m)
             => OpenVR
             -> M44 GLfloat
             -> (M44 GLfloat -> M44 GLfloat -> m a1)
             -> m ()
renderOpenVR OpenVR{..} viewM44 eyeRenderFunc = do

    -- Render each eye, with multisampling
    forM_ ovrEyes $ \EyeInfo{..} -> do

        -- Will render into mfbResolveTextureID
        withMultisamplingFramebuffer eiMultisampleFramebuffer $ do
            let (x, y, w, h) = eiViewport
                finalView    = eiEyeHeadTrans !*! viewM44
            glViewport x y w h

            _ <- eyeRenderFunc eiProjection finalView
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
recenterSeatedPose vrPal = case gpHMD vrPal of
    OpenVRHMD openVR -> resetSeatedZeroPose (ovrSystem openVR)
    _ -> return ()


fadeVRToColor :: MonadIO m => VRPal -> V4 GLfloat -> GLfloat -> m ()
fadeVRToColor vrPal color time = case gpHMD vrPal of
    OpenVRHMD openVR -> fadeCompositorToColor (ovrCompositor openVR) color time
    _ -> return ()

tickDelta :: MonadIO m => VRPal -> m ()
tickDelta VRPal{..} = liftIO $ do
    lastTime <- readIORef gpTimeRef
    currTime <- getCurrentTime

    let diffTime = diffUTCTime currTime lastTime

    writeIORef gpTimeRef currTime
    writeIORef gpDeltaRef diffTime

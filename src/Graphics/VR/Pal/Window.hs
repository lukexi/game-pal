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

#ifdef USE_OCULUS_SDK
import Graphics.Oculus
#endif


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
    
    (window, events) <- createWindow windowName resX resY
  
  
    -- swapInterval 0
  
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
                                setWindowSize window (fromIntegral w `div` 2) (fromIntegral h `div` 2)
                                return ()
                            _ -> return ()
      
                    roomScale <- isUsingLighthouse (ovrSystem openVR)
                    return (OpenVRHMD openVR, if roomScale then RoomScale else NotRoomScale)
                Nothing -> return (NoHMD, NotRoomScale)
#ifdef USE_OCULUS_SDK
        | UseOculus `elem` devices && oculusSupported -> do
            hmd <- createHMD
            setWindowSize window 
                (fromIntegral . fst . hmdBufferSize $ hmd) 
                (fromIntegral . snd . hmdBufferSize $ hmd)
            return (OculusHMD hmd, NotRoomScale)
#endif
        | otherwise -> return (NoHMD, NotRoomScale)
    
    start    <- getCurrentTime
    timeRef  <- newIORef start
    deltaRef <- newIORef 0
  
    emulatedHandDepthRef <- newIORef 1
    
    backgroundSwap <- newEmptyMVar
    forkIO . forever $ do
        action <- takeMVar backgroundSwap
        action  

    return VRPal
        { gpWindow               = window
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

tickVR :: MonadIO m => VRPal -> M44 GLfloat -> m (M44 GLfloat, [VREvent])
tickVR vrPal@VRPal{..} playerM44 = do
    tickDelta vrPal

    case gpHMD of
        OpenVRHMD OpenVR{..} -> do
            events                    <- pollNextEvent ovrSystem
            (headM44Raw, handM44sByRole) <- waitGetPoses ovrCompositor ovrSystem
            let headM44 = playerM44 !*! headM44Raw 
      
            hands <- catMaybes <$> forM handM44sByRole (\(controllerRole, handM44Raw) -> do
                forM (trackedControllerRoleToWhichHand controllerRole) $ \whichHand -> do
                    buttonStates <- getControllerState ovrSystem controllerRole
                    let handM44 = playerM44 !*! handM44Raw 
                    return $ HandEvent whichHand (HandStateEvent (handFromOpenVRController handM44 buttonStates))
                )

            let vrEvents = HeadEvent headM44 : hands ++ (catMaybes $ map vrEventFromOpenVREvent events) 

            return (headM44, vrEvents)
#ifdef USE_OCULUS_SDK
        OculusHMD hmd -> 
            headM44 <- liftIO (getHMDPose (hmdInfo hmd))
            return (headM44, [])
#endif
        _ -> return (playerM44, [])

renderWith :: MonadIO m
           => VRPal
           -> M44 GLfloat
           -> m ()
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith VRPal{..} headM44 frameRenderFunc eyeRenderFunc = do
    let viewM44 = inv44 headM44
    case gpHMD of
        NoHMD  -> do
            (x,y,w,h) <- getWindowViewport gpWindow
            glViewport x y w h
            frameRenderFunc
            renderFlat gpWindow viewM44 eyeRenderFunc
            swapBuffers gpWindow
        OpenVRHMD openVR -> do
            renderOpenVR openVR viewM44 frameRenderFunc eyeRenderFunc gpUseSDKMirror
            when (not gpUseSDKMirror) $ do
                -- This is a workaround to horrible regular stalls when calling swapBuffers on the main thread.
                -- We use a one-slot MVar rather than a channel to avoid any memory leaks if the background
                -- thread can't keep up - it's not important to update the mirror window on any particular
                -- schedule as long as it happens semi-regularly. 
                void . liftIO $ tryPutMVar gpBackgroundSwap (swapBuffers gpWindow)
#ifdef USE_OCULUS_SDK
        OculusHMD hmd -> 
            renderOculus hmd playerPose frameRenderFunc eyeRenderFunc
#endif
    
    
    -- when gpGCPerFrame $ 
    --   profile "GC" 0 $ liftIO performGC

renderOpenVR :: (MonadIO m) 
             => OpenVR
             -> M44 GLfloat
             -> m a
             -> (M44 GLfloat -> M44 GLfloat -> m a1)
             -> Bool
             -> m ()
renderOpenVR OpenVR{..} viewM44 frameRenderFunc eyeRenderFunc useSDKMirror = do

    -- Render each eye, with multisampling
    forM_ ovrEyes $ \EyeInfo{..} -> do

        -- Will render into mfbResolveTextureID
        withMultisamplingFramebuffer eiMultisampleFramebuffer $ do
            -- Must call this within loop since we have 2 different framebuffers
            -- (so, get rid of it as a concept??)
            _ <- frameRenderFunc
            let (x, y, w, h) = eiViewport
                finalView    = eiEyeHeadTrans !*! viewM44
            glViewport x y w h
          
            _ <- eyeRenderFunc eiProjection finalView
            return ()

    -- Submit frames after rendering both
    forM_ ovrEyes $ \EyeInfo{..} -> do
        let MultisampleFramebuffer{..} = eiMultisampleFramebuffer
        submitFrameForEye ovrCompositor eiEye (unTextureID mfbResolveTextureID)

    -- Finally, mirror.
    when (not useSDKMirror) $ forM_ (listToMaybe ovrEyes) $ \eye -> do
        -- Duplicating Valve hacks from hellovr_opengl sample in openvr repo
        mirrorOpenVREyeToWindow eye
        



#ifdef USE_OCULUS_SDK
renderOculus :: MonadIO m 
             => HMD
             -> Pose GLfloat 
             -> m ()
             -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderOculus hmd playerPose frameRenderFunc eyeRenderFunc = do
    renderHMDFrame hmd $ \eyeViews -> do
        frameRenderFunc
        
        renderHMDEyes eyeViews $ \projection eyeView -> do
      
            let finalView = eyeView !*! viewMatrixFromPose playerPose
      
            eyeRenderFunc projection finalView 
    renderHMDMirror hmd
#endif

renderFlat :: MonadIO m 
           => Window -> M44 GLfloat -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderFlat win viewM44 renderFunc = do
    
    projection  <- getWindowProjection win 45 0.1 1000
    
    _           <- renderFunc projection viewM44
  
    return ()

recenterSeatedPose :: MonadIO m => VRPal -> m ()
recenterSeatedPose gamePal = case gpHMD gamePal of
#ifdef USE_OCULUS_SDK
    OculusHMD hmd -> liftIO (recenterPose hmd)
#endif
    OpenVRHMD openVR -> resetSeatedZeroPose (ovrSystem openVR)
    _ -> return ()


tickDelta :: MonadIO m => VRPal -> m ()
tickDelta VRPal{..} = liftIO $ do
    lastTime <- readIORef gpTimeRef
    currTime <- getCurrentTime
  
    let diffTime = diffUTCTime currTime lastTime
  
    writeIORef gpTimeRef currTime
    writeIORef gpDeltaRef diffTime
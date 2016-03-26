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
-- import System.Mem
import Data.Time
import Data.IORef
--import Halive.Utils

#ifdef USE_OCULUS_SDK
import Graphics.Oculus
#endif

#ifdef USE_HYDRA_SDK
import qualified System.Hardware.Hydra as Hydra
#endif



initVRPal :: String -> [VRPalDevices] -> IO VRPal
initVRPal windowName devices = do

  -- Calling swapBuffers triggers a ton of dropped frames. Valve's
  -- mirroring doesn't seem to trigger this problem.
  let useSDKMirror = True

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
            hideKeyboard
            -- 
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
    }

getDeltaTime :: MonadIO m => VRPal -> m NominalDiffTime
getDeltaTime VRPal{..} = liftIO (readIORef gpDeltaRef)

getNow :: MonadIO m => VRPal -> m UTCTime
getNow VRPal{..} = liftIO (readIORef gpTimeRef)

whileVR :: MonadIO m => VRPal -> (M44 GLfloat -> [Hand] -> [VREvent] -> m a) -> m ()
whileVR vrPal@VRPal{..} action = whileWindow gpWindow $ do
  tickDelta vrPal
  case gpHMD of
    OpenVRHMD OpenVR{..} -> do
      events <- pollNextEvent ovrSystem
      (headM44, handM44sByRole) <- waitGetPoses ovrCompositor ovrSystem

      hands <- forM handM44sByRole $ \(controllerRole, handM44) -> do
        buttonStates <- getControllerState ovrSystem controllerRole
        return (handFromOpenVRController controllerRole handM44 buttonStates)
      
      action headM44 hands (map vrEventFromOpenVREvent events)
#ifdef USE_OCULUS_SDK
    OculusHMD hmd -> 
      headM44 <- liftIO (getHMDPose (hmdInfo hmd))
      action headM44 [] []
#endif
    _ -> action identity [] []

renderWith :: MonadIO m
           => VRPal
           -> Pose GLfloat
           -> M44 GLfloat
           -> m ()
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith VRPal{..} playerPose headM44 frameRenderFunc eyeRenderFunc = do
  case gpHMD of
    NoHMD  -> do
      (x,y,w,h) <- getWindowViewport gpWindow
      glViewport x y w h
      frameRenderFunc
      renderFlat gpWindow playerPose eyeRenderFunc
    OpenVRHMD openVR -> 
      renderOpenVR openVR playerPose headM44 frameRenderFunc eyeRenderFunc gpUseSDKMirror
#ifdef USE_OCULUS_SDK
    OculusHMD hmd -> 
      renderOculus hmd playerPose frameRenderFunc eyeRenderFunc
#endif
  -- We always call swapBuffers since mirroring is handled manually in 0.6+ and OpenVR
  when (not gpUseSDKMirror) $ 
    swapBuffers gpWindow
  
  -- when gpGCPerFrame $ 
  --   profile "GC" 0 $ liftIO performGC

renderOpenVR :: (MonadIO m) 
             => OpenVR
             -> Pose GLfloat
             -> M44 GLfloat
             -> m a
             -> (M44 GLfloat -> M44 GLfloat -> m a1)
             -> Bool
             -> m ()
renderOpenVR OpenVR{..} playerPose headM44 frameRenderFunc eyeRenderFunc useSDKMirror = do
  let viewM44 = inv44 headM44 !*! transformationFromPose playerPose
  
  forM_ ovrEyes $ \eye@EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do

      _ <- frameRenderFunc
      
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! viewM44
      glViewport x y w h

      _ <- eyeRenderFunc eiProjection finalView

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

      when (not useSDKMirror) $ 
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
           => Window -> Pose GLfloat -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderFlat win playerPose renderFunc = do
  
  projection  <- getWindowProjection win 45 0.1 1000
  
  _           <- renderFunc projection (viewMatrixFromPose playerPose)

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
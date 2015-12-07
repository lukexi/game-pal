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
import Graphics.GL.Pal
-- import System.Mem
import Data.Time
import Data.IORef
import Halive.Utils

#ifdef USE_OCULUS_SDK
import Graphics.Oculus
#endif

#ifdef USE_HYDRA_SDK
import qualified System.Hardware.Hydra as Hydra
#endif



initVRPal :: String -> [VRPalDevices] -> IO VRPal
initVRPal windowName devices = do

  -- This helped in OpenVR SDK 0.9.10, where it seemed like calling swapBuffers
  -- during mirroring was still causing a VSync even with swapInterval set to 0,
  -- causing a ton of dropped frames
  let useSDKMirror = False

  -- Turn off garbage collection per frame when Halive is active, 
  -- as it grinds things to a halt (I don't know why)
  doGCPerFrame <- not <$> isHaliveActive

#ifdef USE_HYDRA_SDK
  maybeSixenseBase <- if UseHydra `elem` devices 
    then Just <$> Hydra.initSixense 
    else return Nothing
#else
  let maybeSixenseBase = Nothing
#endif

  let (resX, resY) = (500, 400)
  
  (window, events) <- createWindow windowName resX resY

  swapInterval 0

  (hmdType, isRoomScale) <- if 
    | UseOpenVR `elem` devices -> do
        hmdPresent <- isHMDPresent
        mOpenVR <- if hmdPresent then createOpenVR else return Nothing
        case mOpenVR of
          Just openVR -> do
            -- 
            if useSDKMirror
              then do
                showMirrorWindow (ovrCompositor openVR)
                -- Clear and hide the application window, as we don't display to it
                glClear GL_COLOR_BUFFER_BIT
                iconifyWindow window
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

  getDelta <- makeGetDelta

  return VRPal
    { gpWindow       = window
    , gpEvents       = events
    , gpHMD          = hmdType
    , gpSixenseBase  = maybeSixenseBase
    , gpGetDelta     = getDelta
    , gpGCPerFrame   = doGCPerFrame
    , gpRoomScale    = isRoomScale
    , gpUseSDKMirror = case hmdType of { NoHMD -> False; _ -> useSDKMirror }
    }

renderWith :: MonadIO m
           => VRPal
           -> M44 GLfloat
           -> m ()
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith VRPal{..} viewMat frameRenderFunc eyeRenderFunc = do
  case gpHMD of
    NoHMD  -> do
      (x,y,w,h) <- getWindowViewport gpWindow
      glViewport x y w h
      frameRenderFunc
      renderFlat gpWindow viewMat eyeRenderFunc
    OpenVRHMD openVR -> do
      renderOpenVR openVR viewMat frameRenderFunc eyeRenderFunc
#ifdef USE_OCULUS_SDK
    OculusHMD hmd -> do
      renderOculus hmd viewMat frameRenderFunc eyeRenderFunc
      renderHMDMirror hmd
#endif
  -- We always call swapBuffers since mirroring is handled manually in 0.6+ and OpenVR
  when (not gpUseSDKMirror) $
    swapBuffers gpWindow
  
  -- when gpGCPerFrame $ 
  --   profile "GC" 0 $ liftIO performGC

renderOpenVR :: (MonadIO m) 
             => OpenVR
             -> M44 GLfloat
             -> m a
             -> (M44 GLfloat -> M44 GLfloat -> m a1)
             -> m ()
renderOpenVR OpenVR{..} viewMat frameRenderFunc eyeRenderFunc = do

  headPose <- inv44 <$> waitGetPoses ovrCompositor
  let headView = headPose !*! viewMat
  
  forM_ ovrEyes $ \eye@EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do

      _ <- frameRenderFunc
      
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! headView
      glViewport x y w h

      _ <- eyeRenderFunc eiProjection finalView

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

      mirrorOpenVREyeToWindow eye

renderFlat :: MonadIO m 
           => Window -> M44 GLfloat -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderFlat win viewMat renderFunc = do
  
  projection  <- getWindowProjection win 45 0.1 1000
  
  _ <- renderFunc projection viewMat

  return ()

makeGetDelta :: IO (IO NominalDiffTime)
makeGetDelta  = do 

  start <- getCurrentTime
  timeRef <- newIORef start

  let getDelta = do

        lastTime <- readIORef timeRef
        currTime <- getCurrentTime

        let diffTime = diffUTCTime currTime lastTime

        writeIORef timeRef currTime

        return diffTime 

  return getDelta

getPoseForHMDType :: (Fractional a, MonadIO m) => HMDType -> m (M44 a)
getPoseForHMDType hmdType = case hmdType of
  OpenVRHMD openVR -> do
    poses <- getDevicePosesOfClass (ovrSystem openVR) TrackedDeviceClassHMD
    return $ if not (null poses) then head poses else identity
  NoHMD -> return identity
#ifdef USE_OCULUS_SDK
  OculusHMD hmd -> liftIO . getHMDPose . hmdInfo $ hmd
#endif

recenterWhenOculus :: Monad m => VRPal -> m ()
recenterWhenOculus gamePal = case gpHMD gamePal of
#ifdef USE_OCULUS_SDK
  OculusHMD hmd -> liftIO $ recenterPose hmd
#endif
  _ -> return ()

#ifdef USE_OCULUS_SDK
renderOculus :: MonadIO m 
             => HMD
             -> M44 GLfloat 
             -> m ()
             -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderOculus hmd viewMat frameRenderFunc eyeRenderFunc = renderHMDFrame hmd $ \eyeViews -> do
  
  frameRenderFunc
  
  renderHMDEyes eyeViews $ \projection eyeView -> do

    let finalView = eyeView !*! viewMat

    eyeRenderFunc projection finalView 
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
module Game.Pal.Window where
import Graphics.UI.GLFW.Pal
import Graphics.VR.OpenVR
import Control.Monad
import System.Hardware.Hydra
import Control.Monad.Trans
import Linear.Extra
import Game.Pal.View
import Game.Pal.Types
import Graphics.GL.Pal
import System.Mem
import Data.Time
import Data.IORef

#ifdef USE_OCULUS_SDK
import Graphics.Oculus
#endif

initGamePal :: String -> GCPerFrame -> [GamePalDevices] -> IO GamePal
initGamePal windowName gcPerFrame devices = do
  maybeSixenseBase <- if UseHydra `elem` devices then Just <$> initSixense else return Nothing
  
  let (resX, resY) = (2000, 1000)
  
  (window, events) <- createWindow windowName resX resY

  swapInterval 0

  hmdType <- if 
    | UseOpenVR `elem` devices -> do
        mOpenVR <- createOpenVR
        case mOpenVR of
          Just openVR -> do
            forM_ (ovrEyes openVR) $ \eye -> case eiEye eye of
              LeftEye -> do
                let (_, _, w, h) = eiViewport eye
                setWindowSize window (fromIntegral w) (fromIntegral h)
              _ -> return ()
            return (OpenVRHMD openVR)
#ifdef USE_OCULUS_SDK
    | UseOculus `elem` devices && oculusSupported -> do
        hmd <- createHMD
        setWindowSize window 
          (fromIntegral . fst . hmdBufferSize $ hmd) 
          (fromIntegral . snd . hmdBufferSize $ hmd)
        return (OculusHMD hmd)
#endif
    | otherwise -> return NoHMD

  getDelta <- makeGetDelta

  return $ GamePal
    { gpWindow      = window
    , gpEvents      = events
    , gpHMD         = hmdType
    , gpSixenseBase = maybeSixenseBase
    , gpGetDelta    = getDelta
    , gpGCPerFrame  = gcPerFrame
    }

renderWith :: MonadIO m
           => GamePal
           -> M44 GLfloat
           -> m ()
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith GamePal{..} viewMat frameRenderFunc eyeRenderFunc = do
  case gpHMD of
    NoHMD  -> do
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
  swapBuffers gpWindow
  
  when (gpGCPerFrame == GCPerFrame) $ 
    liftIO performGC


renderOpenVR OpenVR{..} viewMat frameRenderFunc eyeRenderFunc = do

  headPose <- safeInv44 <$> waitGetPoses ovrCompositor
  
  forM_ ovrEyes $ \eye@EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do

      frameRenderFunc
      
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! headPose !*! viewMat
      glViewport x y w h

      eyeRenderFunc eiProjection finalView

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

      mirrorOpenVREyeToWindow eye

renderFlat :: MonadIO m 
           => Window -> M44 GLfloat -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderFlat win viewMat renderFunc = do
  
  projection  <- makeProjection win
  
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

getPoseForHMDType hmdType = case hmdType of
  OpenVRHMD openVR -> do
    poses <- getDevicePosesOfClass (ovrSystem openVR) TrackedDeviceClassHMD
    return $ if not (null poses) then head poses else identity
  NoHMD -> return identity
#ifdef USE_OCULUS_SDK
  OculusHMD hmd -> liftIO . getHMDPose . hmdInfo $ hmd
#endif

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

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Pal.Window where
import Graphics.UI.GLFW.Pal
import Graphics.Oculus
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


oculusSupported :: Bool
#if defined(mingw32_HOST_OS)
oculusSupported = True
#else
oculusSupported = False
#endif

initGamePal :: String -> GCPerFrame -> [GamePalDevices] -> IO GamePal
initGamePal windowName gcPerFrame devices = do
  maybeSixenseBase <- if UseHydra `elem` devices then Just <$> initSixense else return Nothing

  
  let (resX, resY) = (2000, 1000)
  
  (window, events) <- createWindow windowName resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
    setWindowSize window (resX `div` 2) (resY `div` 2)

  swapInterval 0

  hmdType <- if 
    | UseOpenVR `elem` devices -> do
        mOpenVR <- createOpenVR
        case mOpenVR of
          Just openVR -> return (OpenVRHMD openVR)
    | UseOculus `elem` devices && oculusSupported -> do
        hmd <- createHMD
        setWindowSize window 
          (fromIntegral . fst . hmdBufferSize $ hmd) 
          (fromIntegral . snd . hmdBufferSize $ hmd)
        return (OculusHMD hmd)
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
      OculusHMD hmd -> do
        renderOculus hmd viewMat frameRenderFunc eyeRenderFunc
        renderHMDMirror hmd
      OpenVRHMD openVR -> do
        renderOpenVR openVR viewMat frameRenderFunc eyeRenderFunc

  -- We always call swapBuffers since mirroring is handled independently in 0.6+
  swapBuffers gpWindow
  -- Commenting out temporarily because it breaks Halive:
  when (gpGCPerFrame == GCPerFrame) $ 
    liftIO performGC

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

renderOpenVR OpenVR{..} viewMat frameRenderFunc eyeRenderFunc = do
  poses <- waitGetPoses ovrCompositor
  
  let headPose = case poses of
        (headPoseWorld:_) -> safeInv44 headPoseWorld
        _        -> identity

  forM_ ovrEyes $ \EyeInfo{..} -> do

    withFramebuffer eiFramebuffer $ do

      frameRenderFunc
      
      let (x, y, w, h) = eiViewport
          finalView    = eiEyeHeadTrans !*! headPose !*! viewMat
      glViewport x y w h

      eyeRenderFunc eiProjection finalView

      submitFrameForEye ovrCompositor eiEye eiFramebufferTexture

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
  OculusHMD hmd -> getMaybeHMDPose (Just hmd)
  OpenVRHMD openVR -> do
    poses <- waitGetPoses (ovrCompositor openVR)
    return $ if not (null poses) then head poses else identity
  NoHMD -> return identity


getMaybeHMDPose :: MonadIO m => Maybe HMD -> m (M44 GLfloat)
getMaybeHMDPose maybeHMD = do
  (headOrient, headPosit) <- maybe 
    (return (axisAngle (V3 0 1 0) 0, V3 0 0 0)) 
    (liftIO . getHMDPose . hmdInfo) 
    maybeHMD
  return (transformationFromPose (Pose headPosit headOrient))

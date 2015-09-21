{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Pal.Window where
import Graphics.UI.GLFW.Pal
import Graphics.Oculus
import Control.Monad
import System.Hardware.Hydra
import Control.Monad.Trans
import Linear
import Game.Pal.View
import Game.Pal.Types
import Graphics.GL
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

  
  let (resX, resY) = (1024, 768)
  
  (window, events) <- createWindow windowName resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
    setWindowSize window (resX `div` 2) (resY `div` 2)

  swapInterval 0

  maybeHMD <- if UseOculus `elem` devices && oculusSupported
    then do
      hmd <- createHMD
      setWindowSize window 
        (fromIntegral . fst . hmdBufferSize $ hmd) 
        (fromIntegral . snd . hmdBufferSize $ hmd)
      return (Just hmd)
    else return Nothing

  getDelta <- makeGetDelta

  return $ GamePal
    { gpWindow      = window
    , gpEvents      = events
    , gpHMD         = maybeHMD
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
      Nothing  -> do
        frameRenderFunc
        renderFlat gpWindow viewMat eyeRenderFunc
      Just hmd -> do
        renderVR hmd viewMat frameRenderFunc eyeRenderFunc
        renderHMDMirror hmd
  -- We always call swapBuffers since mirroring is handled independently in 0.6+
  swapBuffers gpWindow
  -- Commenting out temporarily because it breaks Halive:
  when (gpGCPerFrame == GCPerFrame) $ 
    liftIO performGC

renderVR :: MonadIO m 
         => HMD
         -> M44 GLfloat 
         -> m ()
         -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderVR hmd viewMat frameRenderFunc renderFunc = renderHMDFrame hmd $ \eyeViews -> do
  
  frameRenderFunc
  
  renderHMDEyes eyeViews $ \projection eyeView -> do

    let finalView = eyeView !*! viewMat

    renderFunc projection finalView 

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


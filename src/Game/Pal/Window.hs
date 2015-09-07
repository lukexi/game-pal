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

initGamePal :: String -> [GamePalDevices] -> IO GamePal
initGamePal windowName devices = do
  maybeSixenseBase <- if UseHydra `elem` devices then Just <$> initSixense else return Nothing

  (resX, resY, maybeHMD) <- if UseOculus `elem` devices
    then do
      hmd          <- createHMD
      (resX, resY) <- getHMDResolution hmd
      return (resX, resY, Just hmd)
    else return (1024, 768, Nothing)
  
  (window, events) <- createWindow windowName resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
    setWindowSize window (resX `div` 2) (resY `div` 2)

  -- Set up Oculus
  maybeRenderHMD <- forM maybeHMD $ \hmd -> do
    renderHMD <- configureHMDRendering hmd windowName
    dismissHSWDisplay hmd
    recenterPose hmd
    return renderHMD
  return $ GamePal
    { gpWindow      = window
    , gpEvents      = events
    , gpHMD         = maybeHMD
    , gpRenderHMD   = maybeRenderHMD
    , gpSixenseBase = maybeSixenseBase
    }

renderWith :: MonadIO m
           => Window
           -> Maybe RenderHMD
           -> M44 GLfloat
           -> m ()
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith window maybeRenderHMD viewMat frameRenderFunc eyeRenderFunc = do
  case maybeRenderHMD of
      Nothing        -> frameRenderFunc >> renderFlat window viewMat eyeRenderFunc
      Just renderHMD -> renderVR   renderHMD viewMat frameRenderFunc eyeRenderFunc

renderVR :: MonadIO m 
         => RenderHMD
         -> M44 GLfloat 
         -> m ()
         -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderVR renderHMD viewMat frameRenderFunc renderFunc = renderHMDFrame renderHMD $ \eyeViews -> do
  
  frameRenderFunc
  
  renderHMDEyes eyeViews $ \projection eyeView -> do

    let finalView = eyeView !*! viewMat

    renderFunc projection finalView 

renderFlat :: MonadIO m 
           => Window -> M44 GLfloat -> (M44 GLfloat -> M44 GLfloat -> m b) -> m ()
renderFlat win viewMat renderFunc = do
  
  projection  <- makeProjection win
  
  _ <- renderFunc projection viewMat

  swapBuffers win

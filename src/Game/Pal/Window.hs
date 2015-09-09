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

  
  let (resX, resY) = (1024, 768)
  
  (window, events) <- createWindow windowName resX resY
  -- Compensate for retina framebuffers on Mac
  (frameW, frameH) <- getFramebufferSize window
  when (frameW > resX && frameH > resY) $
    setWindowSize window (resX `div` 2) (resY `div` 2)

  maybeHMD <- if UseOculus `elem` devices
    then Just <$> createHMD
    else return Nothing

  return $ GamePal
    { gpWindow      = window
    , gpEvents      = events
    , gpHMD         = maybeHMD
    , gpSixenseBase = maybeSixenseBase
    }

renderWith :: MonadIO m
           => Window
           -> Maybe HMD
           -> M44 GLfloat
           -> m ()
           -> (M44 GLfloat -> M44 GLfloat -> m b)
           -> m ()
renderWith window maybeRenderHMD viewMat frameRenderFunc eyeRenderFunc = do
  case maybeRenderHMD of
      Nothing  -> do
        frameRenderFunc
        renderFlat window viewMat eyeRenderFunc
      Just hmd -> 
        renderVR hmd viewMat frameRenderFunc eyeRenderFunc
  -- We always call swapBuffers since mirroring is handled independently in 0.6+
  swapBuffers window

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
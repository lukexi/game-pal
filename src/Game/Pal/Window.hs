module Game.Pal.Window where
import Graphics.UI.GLFW.Pal
import Graphics.Oculus
import Control.Monad
import System.Hardware.Hydra

initWindow :: String -> Bool -> Bool -> IO (Window, Events, Maybe HMD, Maybe RenderHMD, Maybe SixenseBase)
initWindow windowName enableVR enableHydra = do
  maybeSixenseBase <- if enableHydra then Just <$> initSixense else return Nothing

  (resX, resY, maybeHMD) <- if enableVR 
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
  return (window, events, maybeHMD, maybeRenderHMD, maybeSixenseBase)

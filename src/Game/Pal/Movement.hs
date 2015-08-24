{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Game.Pal.Movement where
import Linear
import Graphics.GL
import Graphics.Oculus
import Graphics.UI.GLFW.Pal
import Control.Monad.State
import Control.Lens
import Game.Pal.Types
import System.Hardware.Hydra

-- | These functions are all meant to be used with Control.Lens's 'zoom'
applyMouseLook :: (MonadIO m, MonadState s m) => Window -> Lens' s Pose -> m ()
applyMouseLook win poseLens = do
  (x,y) <- getCursorPos win
  (w,h) <- getWindowSize win
  let (x', y') = (x - (fromIntegral w / 2), y - (fromIntegral h / 2))
  poseLens . posOrientation .= axisAngle (V3 0 1 0) (-x'/500)
                             * axisAngle (V3 1 0 0) (-y'/500)

-- | Move player by the given vector, 
-- rotated to be relative to their current orientation
movePose :: MonadState s m => Lens' s Pose -> V3 GLfloat -> m ()
movePose poseLens vec = do
  orient <- use $ poseLens . posOrientation
  poseLens . posPosition += rotate orient vec

applyWASD :: (MonadIO m, MonadState s m) => Window -> Lens' s Pose -> m ()
applyWASD win poseLens = do
  let pos = 0.1
      neg = -pos
  whenKeyPressed win Key'W           $ movePose poseLens (V3 0   0   neg)
  whenKeyPressed win Key'S           $ movePose poseLens (V3 0   0   pos)
  whenKeyPressed win Key'A           $ movePose poseLens (V3 neg 0   0  )
  whenKeyPressed win Key'D           $ movePose poseLens (V3 pos 0   0  )
  whenKeyPressed win Key'Space       $ movePose poseLens (V3 0   pos 0  )
  whenKeyPressed win Key'LeftControl $ movePose poseLens (V3 0   neg 0  )

deadzoneOf zone value = if abs value > zone then value else 0

applyHydraJoystickMovement :: MonadState s m => [ControllerData] -> Lens' s Pose -> m ()
applyHydraJoystickMovement [left, right] poseLens = do
  
  -- Move player forward/back/left/right with left joystick
  movePose poseLens $ V3 
    (joystickX left / 10) 
    0 
    (deadzoneOf 0.01 (-joystickY left / 10))

  -- Turn player left/right with right joystick
  -- (quat rotation must be rotation * original)
  poseLens . posOrientation %= \old -> (axisAngle ( V3 0 1 0 ) (-joystickX right * 0.1)) * old
  
  -- Move player down and up with left and right joystick clicks
  when (ButtonJoystick `elem` handButtons left)  $ movePose poseLens ( V3 0 (-0.1) 0  )
  when (ButtonJoystick `elem` handButtons right) $ movePose poseLens ( V3 0   0.1  0  )

applyHydraJoystickMovement _ _ = return ()

applyGamepadJoystickMovement :: MonadState s m => Event -> Lens' s Pose -> m ()
applyGamepadJoystickMovement e poseLens = onGamepadAxes e $ \GamepadAllAxes{..} -> do
  movePose poseLens (V3 (realToFrac gaxLeftStickX / 10) 0 (realToFrac gaxLeftStickY / 10))
  -- Quat rotation must be rotation * original rather than vice versa
  poseLens . posOrientation %= \old -> axisAngle ( V3 0 1 0 ) (-(realToFrac gaxRightStickY) * 0.1) * old

handsToWorldPoses :: [ControllerData] -> Pose -> [Pose]
handsToWorldPoses hands (Pose playerPos playerRot) = map handWorldPose hands
  where handWorldPose handData = Pose positWorld orientWorld
          where
            handPosit   = fmap (realToFrac . (/500)) (pos handData) + V3 0 (-1) (-1)
            handOrient  = rotQuat handData
            positWorld  = rotate playerRot handPosit + playerPos
            orientWorld = playerRot * handOrient

getMaybeHMDPose :: MonadIO m => Maybe HMD -> m Pose
getMaybeHMDPose maybeHMD = do
  (headOrient, headPosit) <- maybe (return (axisAngle (V3 0 1 0) 0, V3 0 0 0)) (liftIO . getHMDPose) maybeHMD
  return (Pose headPosit headOrient)

onGamepadAxes :: Monad m => Event -> (GamepadAllAxes -> m ()) -> m ()
onGamepadAxes (GamepadAxes axes@(GamepadAllAxes{})) a = a axes
onGamepadAxes _                                     _ = return ()

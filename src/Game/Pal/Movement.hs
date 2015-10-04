{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Game.Pal.Movement where
import Linear.Extra
import Graphics.GL
import Graphics.Oculus
import Graphics.UI.GLFW.Pal
import Control.Monad.State
import Control.Lens.Extra
import System.Hardware.Hydra
import Game.Pal.Types
import Game.Pal.Hand

moveSpeed :: GLfloat
moveSpeed = 0.01

applyMouseLook :: (MonadIO m, MonadState s m) => Window -> Lens' s (Pose GLfloat) -> m ()
applyMouseLook win poseLens = do
  (x,y) <- getCursorPos win
  (w,h) <- getWindowSize win
  let (x', y') = (x - (fromIntegral w / 2), y - (fromIntegral h / 2))
  poseLens . posOrientation .= axisAngle (V3 0 1 0) (-x'/500)
                             * axisAngle (V3 1 0 0) (-y'/500)

-- | Move player by the given vector, 
-- rotated to be relative to their current orientation
movePose :: MonadState s m => Lens' s (Pose GLfloat) -> V3 GLfloat -> m ()
movePose poseLens vec = do
  orient <- use $ poseLens . posOrientation
  poseLens . posPosition += rotate orient vec

applyWASD :: (MonadIO m, MonadState s m) => Window -> Lens' s (Pose GLfloat) -> m ()
applyWASD win poseLens = do
  shiftDown <- (== KeyState'Pressed) <$> getKey win Key'LeftShift
  let pos = moveSpeed    * if shiftDown then 10 else 1
      neg = (-moveSpeed) * if shiftDown then 10 else 1
  whenKeyPressed win Key'W           $ movePose poseLens (V3 0   0   neg)
  whenKeyPressed win Key'S           $ movePose poseLens (V3 0   0   pos)
  whenKeyPressed win Key'A           $ movePose poseLens (V3 neg 0   0  )
  whenKeyPressed win Key'D           $ movePose poseLens (V3 pos 0   0  )
  whenKeyPressed win Key'Q           $ movePose poseLens (V3 0   pos 0  )
  whenKeyPressed win Key'E           $ movePose poseLens (V3 0   neg 0  )


applyHandJoystickMovement :: MonadState s m => [Hand] -> Lens' s (Pose GLfloat) -> m ()
applyHandJoystickMovement [left, right] poseLens = do
  
  -- Move player forward/back/left/right with left joystick
  movePose poseLens $ V3 
    ((left ^. hndXY . _x) / 10) 
    0 
    (-(left ^. hndXY . _y) / 10)

  -- Turn player left/right with right joystick
  -- (quat rotation must be rotation * original)
  poseLens . posOrientation %= \old -> 
    (axisAngle (V3 0 1 0) 
               (-(right ^. hndXY . _x) * moveSpeed)) 
    * old
  
  -- Move player down and up with left and right joystick clicks
  when (left  ^. hndButtonJ) $ movePose poseLens ( V3 0 (-moveSpeed) 0  )
  when (right ^. hndButtonJ) $ movePose poseLens ( V3 0   moveSpeed  0  )

applyHydraJoystickMovement _ _ = return ()

applyGamepadJoystickMovement :: MonadState s m => Event -> Lens' s (Pose GLfloat) -> m ()
applyGamepadJoystickMovement e poseLens = onGamepadAxes e $ \GamepadAllAxes{..} -> do
  movePose poseLens (V3 (realToFrac gaxLeftStickX / 10) 0 (realToFrac gaxLeftStickY / 10))
  -- Quat rotation must be rotation * original rather than vice versa
  poseLens . posOrientation %= \old -> 
    axisAngle ( V3 0 1 0 ) (-(realToFrac gaxRightStickY) * moveSpeed) * old



onGamepadAxes :: Monad m => Event -> (GamepadAllAxes -> m ()) -> m ()
onGamepadAxes (GamepadAxes axes@(GamepadAllAxes{})) a = a axes
onGamepadAxes _                                     _ = return ()




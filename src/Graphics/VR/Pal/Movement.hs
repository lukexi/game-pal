{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Graphics.VR.Pal.Movement where
import Linear.Extra
import Graphics.GL
import Control.Monad.State hiding (get)
import Control.Lens.Extra
import SDL
import Graphics.VR.Pal.SDLUtils

moveSpeed :: GLfloat
moveSpeed = 0.01

applyMouseLook :: (MonadIO m, MonadState s m) => Window -> Lens' s (Pose GLfloat) -> m ()
applyMouseLook win poseLens = do
    V2 x y <- getMouseLocationV2
    V2 w h <- get (windowSize win)
    let (x', y') = (x - (fromIntegral w / 2), y - (fromIntegral h / 2))
    poseLens . posOrientation .= axisAngle (V3 0 1 0) (-x'/500)
                               * axisAngle (V3 1 0 0) (-y'/500)

-- | Move player by the given vector,
-- rotated to be relative to their current orientation
movePose :: MonadState s m => Lens' s (Pose GLfloat) -> V3 GLfloat -> m ()
movePose poseLens vec = do
    orient <- use $ poseLens . posOrientation
    poseLens . posPosition += rotate orient vec

turnPose :: MonadState s m => Lens' s (Pose GLfloat) -> Quaternion GLfloat -> m ()
turnPose poseLens turn =
    -- Quat rotation must be rotation * original rather than vice versa
    poseLens . posOrientation %= (turn *)

applyWASD :: (MonadIO m, MonadState s m) => Lens' s (Pose GLfloat) -> m ()
applyWASD poseLens = do
    shiftDown <- isShiftDown
    let pos = moveSpeed    * if shiftDown then 10 else 1
        neg = (-moveSpeed) * if shiftDown then 10 else 1
    whenKeyPressed ScancodeW $ movePose poseLens (V3 0   0   neg)
    whenKeyPressed ScancodeS $ movePose poseLens (V3 0   0   pos)
    whenKeyPressed ScancodeA $ movePose poseLens (V3 neg 0   0  )
    whenKeyPressed ScancodeD $ movePose poseLens (V3 pos 0   0  )
    whenKeyPressed ScancodeQ $ movePose poseLens (V3 0   neg 0  )
    whenKeyPressed ScancodeE $ movePose poseLens (V3 0   pos 0  )


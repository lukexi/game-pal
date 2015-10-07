module Game.Pal.View where
import Linear.Extra
import Graphics.UI.GLFW.Pal
import Control.Monad.State
import Control.Lens.Extra
import Data.Maybe (fromMaybe)

-- | Get a view matrix for a camera at a given position and orientation
viewMatrix :: (RealFloat a, Conjugate a) => V3 a -> Quaternion a -> M44 a
viewMatrix position orientation = mkTransformation q (rotate q . negate $ position)
    where q = conjugate orientation

-- | Use the aspect ratio from the window to get a proper projection
makeProjection :: (Floating a, MonadIO m) => Window -> m (M44 a)
makeProjection win = do
    (w,h) <- getWindowSize win
    return $ perspective 45 (fromIntegral w / fromIntegral h) 0.01 100

viewMatrixFromPose :: (RealFloat a, Conjugate a) => Pose a -> M44 a
viewMatrixFromPose (Pose posit orient) = viewMatrix posit orient

windowPosToWorldRay :: (RealFloat a, Conjugate a, Epsilon a, MonadIO m) => Window -> Pose a -> (a, a) -> m (V3 a, V3 a)
windowPosToWorldRay win pose coord = do
  (xNDC, yNDC) <- win2Ndc coord
  start <- ndc2Wld (V4 xNDC yNDC (-1.0) 1.0)
  end <- ndc2Wld (V4 xNDC yNDC 0.0 1.0)
  let dir = normalize (end ^-^ start)
  return (start, dir ^* 1000.0)

  where -- Converts from window coordinates (origin top-left) to normalized device coordinates
        win2Ndc (x, y) = do
          (w, h) <- getWindowSize win
          let h' = fromIntegral h
          return ((((x / fromIntegral w) - 0.5) * 2.0), ((((h' - y) / h') - 0.5) * 2.0))
        -- Converts from normalized device coordinates to world coordinates
        ndc2Wld i = do 
          projM <- makeProjection win
          let m = unsafeInv (projM !*! (viewMatrixFromPose pose))
          return $ hom2Euc (m !* i)
        -- Converts from homogeneous coordinates to Euclidean coordinates
        hom2Euc v = (v ^/ (v ^. _w)) ^. _xyz
        unsafeInv m = fromMaybe (error "windowPosToWorldRay: could not compute inverse") (inv44 m)
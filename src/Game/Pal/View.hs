module Game.Pal.View where
import Linear.Extra
import Graphics.UI.GLFW.Pal
import Control.Monad.State

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

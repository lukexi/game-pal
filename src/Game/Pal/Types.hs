{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Game.Pal.Types where
import Control.Lens
import Linear
import Graphics.GL
import GHC.Generics
import Data.Binary

data Pose = Pose
  { _posPosition    :: V3 GLfloat
  , _posOrientation :: Quaternion GLfloat 
  } deriving (Generic, Binary, Show)
makeLenses ''Pose

newPose :: Pose
newPose = Pose 0 (axisAngle (V3 0 1 0) 0)

poseToMatrix :: Pose -> M44 GLfloat
poseToMatrix (Pose posit orient) = mkTransformation orient posit

{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}

module Game.Pal.Pose where

import Linear
import Data.Binary
import GHC.Generics
import Graphics.GL
import Control.Lens

data Pose = Pose
  { _posPosition    :: !(V3 GLfloat)
  , _posOrientation :: !(Quaternion GLfloat )
  } deriving (Generic, Binary, Show)
makeLenses ''Pose

newPose :: Pose
newPose = Pose 0 (axisAngle (V3 0 1 0) 0)

poseToMatrix :: Pose -> M44 GLfloat
poseToMatrix (Pose posit orient) = mkTransformation orient posit

transformationFromPose :: Pose -> M44 GLfloat
transformationFromPose = poseToMatrix

shiftBy :: V3 GLfloat -> Pose -> Pose
shiftBy vec pose = pose & posPosition +~ rotate (pose ^. posOrientation) vec

addPoses :: Pose -> Pose -> Pose
addPoses basePose addedPose = 
  let Pose basePosition baseOrientation = basePose
      Pose addPosition  addOrientation  = addedPose
  in  Pose 
    (addPosition    + basePosition) 
    (addOrientation * baseOrientation) 
    -- quat rotation order must be rotation*original

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Pal.Hands where
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal

import qualified System.Hardware.Hydra as Hydra
import Graphics.VR.OpenVR
import Game.Pal.Types

data Hand = Hand
  { _hndMatrix   :: M44 GLfloat
  , _hndXY       :: V2 GLfloat  -- Touchpad on Vive
  , _hndTrigger  :: GLfloat
  , _hndGrip     :: GLfloat -- Grip on Vive, Grip on Oculus, Shoulder on Hydra
  , _hndButtonS  :: Bool -- Start button on Hydra
  , _hndButtonJ  :: Bool -- Joystick button on Hydra
  , _hndButtonA  :: Bool
  , _hndButtonB  :: Bool
  , _hndButtonC  :: Bool
  , _hndButtonD  :: Bool
  } deriving (Show)
makeLenses ''Hand

emptyHand :: Hand
emptyHand = Hand
  { _hndMatrix   = identity
  , _hndXY       = 0
  , _hndTrigger  = 0
  , _hndGrip     = 0
  , _hndButtonS  = False
  , _hndButtonA  = False
  , _hndButtonB  = False
  , _hndButtonC  = False
  , _hndButtonD  = False
  }

getHands GamePal{..} = case gpHMD of
    OpenVRHMD openVR -> do
      poses <- waitGetPoses (ovrCompositor openVR)
      
      let (leftHandPose, rightHandPose) = case poses of
            (_head:controller1:controller2:_xs) -> (controller1, controller2)
            _                                   -> (identity, identity)

      return $ map handFromOpenVRController [leftHandPose, rightHandPose]


    _ -> map handFromHydra <$> maybe (return []) 
               (Hydra.getHands) 
               gpSixenseBase


handFromOpenVRController matrix = emptyHand { _hndMatrix = matrix }

handFromHydra :: Hydra.ControllerData -> Hand
handFromHydra handData = emptyHand
  { _hndMatrix = mkTransformation
      (Hydra.rotQuat handData)
      ((position * hydraScale) + hydraOffset)
  , _hndXY      = V2 (deadzoneOf 0.05 $ Hydra.joystickX handData)
                     (Hydra.joystickY handData)
  , _hndTrigger = Hydra.trigger handData
  , _hndGrip    = if Hydra.ButtonBumper `elem` buttons then 1 else 0
  , _hndButtonS = Hydra.ButtonStart    `elem` buttons
  , _hndButtonJ = Hydra.ButtonJoystick `elem` buttons
  , _hndButtonA = Hydra.Button1        `elem` buttons
  , _hndButtonB = Hydra.Button2        `elem` buttons
  , _hndButtonC = Hydra.Button3        `elem` buttons
  , _hndButtonD = Hydra.Button4        `elem` buttons
  }
  where
    position = realToFrac <$> Hydra.pos handData
    buttons = Hydra.handButtons handData
    hydraScale = 1/500
    hydraOffset = V3 0 (-1) (-1)

handsWorldPoses :: M44 GLfloat -> [Hand] -> [M44 GLfloat]
handsWorldPoses player hands  = map ((!*! player) . (view hndMatrix)) hands

deadzoneOf :: (Num a, Ord a) => a -> a -> a
deadzoneOf zone value = if abs value > zone then value else 0
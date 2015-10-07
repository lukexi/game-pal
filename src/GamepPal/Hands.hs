{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Game.Pal.Hands where
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal

import qualified System.Hardware.Hydra as Hydra
import Graphics.VR.OpenVR
import Game.Pal.Types
import Control.Monad

type HandID = Int
data Hand = Hand
  { _hndID       :: HandID
  , _hndMatrix   :: M44 GLfloat
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
  { _hndID       = 0
  , _hndMatrix   = identity
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
    OpenVRHMD OpenVR{..} -> do
      poses <- getDevicePosesOfClass ovrSystem TrackedDeviceClassController

      hands <- forM (zip [0..] poses) $ \(i, pose) -> do
        buttonStates <- getControllerState ovrSystem i
        return (handFromOpenVRController i pose buttonStates)

      return hands

    _ -> map handFromHydra <$> maybe (return []) 
               (Hydra.getHands) 
               gpSixenseBase


handFromOpenVRController i matrix (triggerState, gripState, startState) = emptyHand 
  { _hndID      = fromIntegral i
  , _hndMatrix  = matrix 
  , _hndTrigger = if triggerState then 1 else 0
  , _hndGrip    = if gripState then 1 else 0
  , _hndButtonS = startState
  }

handFromHydra :: Hydra.ControllerData -> Hand
handFromHydra handData = emptyHand
  { _hndID = if Hydra.whichHand handData == Hydra.LeftHand then 0 else 1
  , _hndMatrix = mkTransformation
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

handsToWorldPoses :: M44 GLfloat -> [Hand] -> [M44 GLfloat]
handsToWorldPoses player hands  = map ((player !*!) . (view hndMatrix)) hands

deadzoneOf :: (Num a, Ord a) => a -> a -> a
deadzoneOf zone value = if abs value > zone then value else 0
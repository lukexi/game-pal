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
import Control.Monad.Trans

type HandID = Int
data Hand = Hand
  { _hndID       :: HandID
  , _hndMatrix   :: M44 GLfloat
  , _hndXY       :: V2 GLfloat  -- Touchpad on Vive
  , _hndTrigger  :: GLfloat
  , _hndGrip     :: Bool -- Grip on Vive, Grip on Oculus, Shoulder on Hydra
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
  , _hndGrip     = False
  , _hndButtonS  = False
  , _hndButtonA  = False
  , _hndButtonB  = False
  , _hndButtonC  = False
  , _hndButtonD  = False
  }

-- | Get the hands from OpenVR or Sixense
getHands GamePal{..} = case gpHMD of
  OpenVRHMD OpenVR{..} -> do
    poses <- getDevicePosesOfClass ovrSystem TrackedDeviceClassController

    hands <- forM (zip [0..] poses) $ \(i, pose) -> do
      buttonStates <- getControllerState ovrSystem i
      return (handFromOpenVRController i pose buttonStates)

    -- Check for Hydras in case we're using SteamVR + Oculus
    if null hands
      then handsFromHydra gpSixenseBase
      else return hands

  _ -> handsFromHydra gpSixenseBase


handFromOpenVRController i matrix (x, y, trigger, grip, start) = emptyHand 
  { _hndID      = fromIntegral i
  , _hndMatrix  = matrix 
  , _hndXY      = realToFrac <$> V2 x y
  , _hndTrigger = realToFrac trigger 
  , _hndGrip    = grip
  , _hndButtonS = start
  }

handsFromHydra mSixenseBase = 
  map handFromHydra <$> 
    maybe (return []) 
          Hydra.getHands
          mSixenseBase

handFromHydra :: Hydra.ControllerData -> Hand
handFromHydra handData = emptyHand
  { _hndID = if Hydra.whichHand handData == Hydra.LeftHand then 0 else 1
  , _hndMatrix = mkTransformation
      (Hydra.rotQuat handData)
      ((position * hydraScale) + hydraOffset)
  , _hndXY      = V2 (deadzoneOf 0.05 $ Hydra.joystickX handData)
                     (Hydra.joystickY handData)
  , _hndTrigger = Hydra.trigger handData
  , _hndGrip    = Hydra.ButtonBumper   `elem` buttons
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
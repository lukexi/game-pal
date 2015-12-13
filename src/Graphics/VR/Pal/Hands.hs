{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Graphics.VR.Pal.Hands where
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL.Pal

import Graphics.VR.OpenVR
import Graphics.VR.Pal.Types
import Control.Monad.Trans
import Foreign.C

#ifdef USE_HYDRA_SDK
import qualified System.Hardware.Hydra as Hydra
#endif

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
  , _hndButtonJ  = False
  , _hndButtonA  = False
  , _hndButtonB  = False
  , _hndButtonC  = False
  , _hndButtonD  = False
  }

triggerHandHapticPulse :: MonadIO m => VRPal -> CInt -> CInt -> CUShort -> m ()
triggerHandHapticPulse VRPal{..} controllerNumber axis duration = case gpHMD of
  OpenVRHMD openVR -> 
    triggerHapticPulse (ovrSystem openVR) controllerNumber axis duration
  _ -> return ()

handFromOpenVRController :: (Integral a, Real b) 
                         => a -> M44 GLfloat -> (b, b, b, Bool, Bool) -> Hand
handFromOpenVRController i matrix (x, y, trigger, grip, start) = emptyHand 
  { _hndID      = fromIntegral i
  , _hndMatrix  = matrix 
  , _hndXY      = realToFrac <$> V2 x y
  , _hndTrigger = realToFrac trigger 
  , _hndGrip    = grip
  , _hndButtonS = start
  }

handsToWorldPoses :: M44 GLfloat -> [Hand] -> [M44 GLfloat]
handsToWorldPoses player hands  = map ((player !*!) . (view hndMatrix)) hands

deadzoneOf :: (Num a, Ord a) => a -> a -> a
deadzoneOf zone value = if abs value > zone then value else 0

showHandKeyboard :: MonadIO m => VRPal -> m ()
showHandKeyboard VRPal{..} = case gpHMD of
  OpenVRHMD _ -> showKeyboard
  _ -> return ()

hideHandKeyboard :: MonadIO m => VRPal -> m ()
hideHandKeyboard VRPal{..} = case gpHMD of
  OpenVRHMD _ -> hideKeyboard
  _ -> return ()

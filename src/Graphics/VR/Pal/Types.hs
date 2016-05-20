{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Graphics.VR.Pal.Types where

import Graphics.UI.GLFW.Pal

import Data.Time
import Data.IORef
import Data.Hashable
import GHC.Generics
import Graphics.VR.OpenVR
import Control.Concurrent
import Control.Lens.Extra
import Linear.Extra
import Graphics.GL

-- | Passed to init to determine which devices to initialize
data VRPalDevices = UseOpenVR | UseOculus deriving (Eq, Show, Ord)

-- | Instructs vr-pal to run a garbage collection after each frame render
data GCPerFrame = GCPerFrame | NoGCPerFrame deriving (Eq, Show, Ord)

-- | Indicates when we're using the Vive
data RoomScale = RoomScale | NotRoomScale deriving (Eq, Show, Ord)

data HMDType = NoHMD
             | OpenVRHMD OpenVR

data VRPal = VRPal
    { gpWindow               :: !Window
    , gpThreadWindow         :: !Window
    , gpEvents               :: !Events
    , gpHMD                  :: !HMDType
    , gpTimeRef              :: !(IORef UTCTime)
    , gpDeltaRef             :: !(IORef NominalDiffTime)
    , gpGCPerFrame           :: !Bool
    , gpUseSDKMirror         :: !Bool
    , gpRoomScale            :: !RoomScale
    , gpEmulatedHandDepthRef :: !(IORef Float)
    , gpBackgroundSwap       :: !(MVar (IO ()))
    }


data VRPalEvent = GLFWEvent Event
                | VREvent VREvent
                deriving Show

data ButtonState = ButtonDown | ButtonUp | ButtonTouch | ButtonUntouch deriving Show

data HandButton = HandButtonA
                | HandButtonB
                | HandButtonPad
                | HandButtonStart
                | HandButtonGrip
                | HandButtonTrigger
                deriving Show

data WhichHand = LeftHand | RightHand deriving (Show, Eq, Generic)
instance Hashable WhichHand

data HandEvent = HandStateEvent  Hand
               | HandButtonEvent HandButton ButtonState
               deriving Show

data VREvent = HeadEvent (M44 GLfloat)
             | HandEvent WhichHand HandEvent
             | HandKeyboardInputEvent String
             deriving Show

type HandID = Int
data Hand = Hand
    { _hndID       :: HandID
    , _hndMatrix   :: M44 GLfloat
    , _hndXY       :: V2 GLfloat  -- Touchpad on Vive
    , _hndTrigger  :: GLfloat
    , _hndGrip     :: Bool -- Grip on Vive
    , _hndButtonS  :: Bool -- Top button on Vive
    , _hndButtonJ  :: Bool -- Pad button on Vive
    , _hndButtonA  :: Bool
    , _hndButtonB  :: Bool
    } deriving (Show)
makeLenses ''Hand

{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
module Graphics.VR.Pal.Types where

import Graphics.UI.GLFW.Pal
import System.Hardware.Hydra
import Data.Time
import Graphics.VR.OpenVR
#ifdef USE_OCULUS_SDK
import Graphics.Oculus
#endif

-- | Passed to init to determine which devices to initialize
data VRPalDevices = UseOpenVR | UseOculus | UseHydra deriving (Eq, Show, Ord)

-- | Instructs vr-pal to run a garbage collection after each frame render
data GCPerFrame = GCPerFrame | NoGCPerFrame deriving (Eq, Show, Ord)

-- | Indicates whether we're using the Vive or the Hydras
data RoomScale = RoomScale | NotRoomScale deriving (Eq, Show, Ord)

data HMDType = NoHMD 
             | OpenVRHMD OpenVR
#ifdef USE_OCULUS_SDK
             | OculusHMD HMD
#endif

data VRPal = VRPal
  { gpWindow      :: !Window
  , gpEvents      :: !Events
  , gpHMD         :: !HMDType
  , gpSixenseBase :: !(Maybe SixenseBase)
  , gpGetDelta    :: !(IO NominalDiffTime)
  , gpGCPerFrame  :: !Bool
  , gpRoomScale   :: !RoomScale
  }


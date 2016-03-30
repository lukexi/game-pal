{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
module Graphics.VR.Pal.Types where

import Graphics.UI.GLFW.Pal

import Data.Time
import Data.IORef
import Graphics.VR.OpenVR
import Control.Concurrent
#ifdef USE_OCULUS_SDK
import Graphics.Oculus
#endif

-- | Passed to init to determine which devices to initialize
data VRPalDevices = UseOpenVR | UseOculus deriving (Eq, Show, Ord)

-- | Instructs vr-pal to run a garbage collection after each frame render
data GCPerFrame = GCPerFrame | NoGCPerFrame deriving (Eq, Show, Ord)

-- | Indicates when we're using the Vive
data RoomScale = RoomScale | NotRoomScale deriving (Eq, Show, Ord)

data HMDType = NoHMD 
             | OpenVRHMD OpenVR
#ifdef USE_OCULUS_SDK
             | OculusHMD HMD
#endif

data VRPal = VRPal
    { gpWindow               :: !Window
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




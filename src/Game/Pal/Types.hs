{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE CPP #-}
module Game.Pal.Types where

import Graphics.UI.GLFW.Pal
import System.Hardware.Hydra
-- import Graphics.Oculus
import Data.Time
import Graphics.VR.OpenVR

data GamePalDevices = UseOpenVR | UseOculus | UseHydra deriving (Eq, Show, Ord)

data GCPerFrame = GCPerFrame | NoGCPerFrame deriving (Eq, Show, Ord)

data HMDType = NoHMD 
             | OpenVRHMD OpenVR
#ifdef USE_OCULUS_SDK
             | OculusHMD HMD
#endif

data GamePal = GamePal
  { gpWindow      :: !Window
  , gpEvents      :: !Events
  , gpHMD         :: !HMDType
  , gpSixenseBase :: !(Maybe SixenseBase)
  , gpGetDelta    :: !(IO NominalDiffTime)
  , gpGCPerFrame  :: !GCPerFrame
  }


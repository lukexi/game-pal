{-# LANGUAGE TemplateHaskell, DeriveGeneric, DeriveAnyClass #-}
module Game.Pal.Types where

import Graphics.UI.GLFW.Pal
import System.Hardware.Hydra
import Graphics.Oculus
import Data.Time

data GamePalDevices = UseOculus | UseHydra deriving (Eq, Show, Ord)

data GCPerFrame = GCPerFrame | NoGCPerFrame deriving (Eq, Show, Ord)

data GamePal = GamePal
  { gpWindow      :: !Window
  , gpEvents      :: !Events
  , gpHMD         :: !(Maybe HMD)
  , gpSixenseBase :: !(Maybe SixenseBase)
  , gpGetDelta    :: !(IO NominalDiffTime)
  , gpGCPerFrame  :: !GCPerFrame
  }


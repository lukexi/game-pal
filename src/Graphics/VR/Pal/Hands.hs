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
    }

whichHandToTrackedControllerRole :: WhichHand -> TrackedControllerRole
whichHandToTrackedControllerRole LeftHand = TrackedControllerRoleLeftHand
whichHandToTrackedControllerRole RightHand = TrackedControllerRoleRightHand

trackedControllerRoleToWhichHand :: TrackedControllerRole -> Maybe WhichHand 
trackedControllerRoleToWhichHand TrackedControllerRoleLeftHand  = Just LeftHand
trackedControllerRoleToWhichHand TrackedControllerRoleRightHand = Just RightHand
trackedControllerRoleToWhichHand _                              = Nothing

eButtonToHandButton :: EButton -> Maybe HandButton
eButtonToHandButton EButtonApplicationMenu = Just HandButtonStart
eButtonToHandButton EButtonGrip            = Just HandButtonGrip
eButtonToHandButton EButtonAxis0           = Just HandButtonPad
eButtonToHandButton EButtonAxis1           = Just HandButtonTrigger
eButtonToHandButton _                      = Nothing

triggerHandHapticPulse :: MonadIO m => VRPal -> WhichHand -> CInt -> CUShort -> m ()
triggerHandHapticPulse VRPal{..} whichHand axis duration = case gpHMD of
    OpenVRHMD openVR -> 
        triggerHapticPulse (ovrSystem openVR) (whichHandToTrackedControllerRole whichHand) axis duration
    _ -> return ()

handFromOpenVRController :: (Real a) => M44 GLfloat -> (a, a, a, Bool, Bool) -> Hand
handFromOpenVRController matrix (x, y, trigger, grip, start) = emptyHand 
    { _hndMatrix  = matrix 
    , _hndXY      = realToFrac <$> V2 x y
    , _hndTrigger = realToFrac trigger 
    , _hndGrip    = grip
    , _hndButtonS = start
    }

vrEventFromOpenVREvent :: OpenVREvent -> Maybe VREvent
vrEventFromOpenVREvent (VREventKeyboardCharInput string)            = Just (HandKeyboardInputEvent string)
vrEventFromOpenVREvent (VREventButtonPress   controllerRole button) = handButtonEventFromOpenVREvent controllerRole button ButtonDown
vrEventFromOpenVREvent (VREventButtonUnpress controllerRole button) = handButtonEventFromOpenVREvent controllerRole button ButtonUp
vrEventFromOpenVREvent (VREventButtonTouch   controllerRole button) = handButtonEventFromOpenVREvent controllerRole button ButtonTouch
vrEventFromOpenVREvent (VREventButtonUntouch controllerRole button) = handButtonEventFromOpenVREvent controllerRole button ButtonUntouch

handButtonEventFromOpenVREvent :: TrackedControllerRole -> EButton -> ButtonState -> Maybe VREvent
handButtonEventFromOpenVREvent controllerRole button buttonState = do
    whichHand <- trackedControllerRoleToWhichHand controllerRole
    handButton  <- eButtonToHandButton button
    return $ HandEvent whichHand (HandButtonEvent handButton buttonState)

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



onLeftHandEvent :: Monad m => VRPalEvent -> (HandEvent -> m ()) -> m ()
onLeftHandEvent (VREvent (HandEvent LeftHand handEvent)) f = f handEvent
onLeftHandEvent _ _ = return ()

onRightHandEvent :: Monad m => VRPalEvent -> (HandEvent -> m ()) -> m ()
onRightHandEvent (VREvent (HandEvent RightHand handEvent)) f = f handEvent
onRightHandEvent _ _ = return ()

onHandEvent :: Monad m => WhichHand -> VRPalEvent -> (HandEvent -> m ()) -> m ()
onHandEvent desiredHand (VREvent (HandEvent eventHand handEvent)) f 
    | desiredHand == eventHand = f handEvent
onHandEvent _ _ _ = return ()


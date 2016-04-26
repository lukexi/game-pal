
emulateRightHand :: (MonadIO m) => VRPal -> Pose Float -> [VRPalEvent] -> m [Hand]
emulateRightHand VRPal{..} player events = do

    projM44     <- getWindowProjection gpWindow 45 0.1 1000
    mouseRay    <- cursorPosToWorldRay gpWindow projM44 player
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \case
        GLFWEvent e -> onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandDepthRef (+ (y*0.1))
        _ -> return ()
    handZ <- liftIO (readIORef gpEmulatedHandDepthRef)
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
    let handPosition = projectRay mouseRay handZ
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handMatrix   = transformationFromPose $ newPose
                                              & posPosition .~ handPosition
                                              & posOrientation .~ axisAngle (V3 0 1 0) a
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]

emulateRightHandVR :: (MonadIO m) => VRPal -> Pose Float -> [VRPalEvent] -> m [Hand]
emulateRightHandVR VRPal{..} _player events = do
    mouseState1 <- getMouseButton gpWindow MouseButton'1
    mouseState2 <- getMouseButton gpWindow MouseButton'2

    forM_ events $ \case
        GLFWEvent e -> onScroll e $ \_x y ->
            liftIO $ modifyIORef' gpEmulatedHandDepthRef (+ (y*0.1))
        _ -> return ()
    z <- liftIO (readIORef gpEmulatedHandDepthRef)

    (fromIntegral -> w, fromIntegral -> h) <- getWindowSize gpWindow
    (x, y) <- getCursorPos gpWindow
    -- a <- getNow -- swap with line below to rotate hand for testing
    let a = 0
        trigger      = if mouseState1 == MouseButtonState'Pressed then 1 else 0
        grip         = mouseState2 == MouseButtonState'Pressed
        handPosition = V3 (-x/w * 0.1) z (-y/h * 0.1)
        handMatrix   = transformationFromPose $ newPose
                                              & posPosition .~ handPosition
                                              & posOrientation .~ axisAngle (V3 0 1 0) a
        rightHand = emptyHand
                & hndMatrix  .~ handMatrix
                & hndTrigger .~ trigger
                & hndGrip    .~ grip
    return [emptyHand, rightHand]
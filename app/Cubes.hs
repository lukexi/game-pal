{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.VR.Pal
import Graphics.GL.Pal
import Data.Time
import Control.Lens.Extra
import Control.Monad
import Control.Monad.State

data Uniforms = Uniforms
    { uModelViewProjection :: UniformLocation (M44 GLfloat)
    , uModel               :: UniformLocation (M44 GLfloat)
    , uCamera              :: UniformLocation (V3  GLfloat)
    , uDiffuse             :: UniformLocation (V4  GLfloat)
    , uTime                :: UniformLocation GLfloat
    } deriving (Data)

data Cube = Cube
    { _cubMatrix :: M44 GLfloat
    , _cubColor  :: V4 GLfloat
    }
makeLenses ''Cube

getNow :: IO Double
getNow = realToFrac . utctDayTime <$> getCurrentTime

worldCubes :: [Cube]
worldCubes = [cubeAt x y z | x <- [-5..5], y <- [-2..2], z <- [-5..5] ]
  where
    cubeAt x y z = Cube
        { _cubMatrix = transformationFromPose $ newPose { _posPosition = V3 x y z }
        , _cubColor = color
        }
      where color = V4 ((y + 2) / 4) 0.4 ((x+2)/4) 1 -- increase redness as y goes up, blueness as x goes up


main :: IO ()
main = do

    vrPal@VRPal{..} <- initVRPal "VR Pal"

    cubeProg   <- createShaderProgram "app/cube.vert" "app/cube.frag"
    cubeGeo    <- cubeGeometry (0.1 :: V3 GLfloat) (V3 1 1 1)
    cubeShape  <- makeShape cubeGeo cubeProg

    glClearColor 0.1 0.2 0.3 1
    glEnable GL_DEPTH_TEST
    useProgram (sProgram cubeShape)

    void . flip runStateT identity . whileWindow vrpWindow $ \winEvents -> do
        playerM44 <- get
        (headM44, events) <- tickVR vrPal playerM44 winEvents
        applyWASD (iso poseFromMatrix transformationFromPose)

        let handStates = flip concatMap events $ \case
                VREvent (HandEvent _ (HandStateEvent handState)) -> [handState]
                _                                                -> []
            handCubes = map (\hand -> Cube { _cubColor = 1, _cubMatrix = hand ^. hndMatrix }) handStates
        renderWith vrPal headM44
            (render cubeShape (handCubes ++ worldCubes))

render :: (MonadIO m)
       => Shape Uniforms
       -> [Cube]
       -> M44 GLfloat
       -> M44 GLfloat
       -> V4 GLfloat
       -> V4 GLfloat
       -> m ()
render cubeShape cubes projM44 viewM44 _projRaw _viewport = do
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

    let Uniforms{..} = sUniforms cubeShape
        projView = projM44 !*! viewM44

    -- We extract eyePos from the view matrix to get eye-to-head offsets baked in
    uniformV3 uCamera (inv44 viewM44 ^. translation)

    withVAO (sVAO cubeShape) $ forM_ cubes $ \cube -> do
        uniformV4 uDiffuse (cube ^. cubColor)

        draw (cube ^. cubMatrix) projView cubeShape

draw :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
draw model projView shape = do
    let Uniforms{..} = sUniforms shape

    uniformM44 uModelViewProjection (projView !*! model)
    uniformM44 uModel               model

    let vc = geoIndexCount (sGeometry shape)
    glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr



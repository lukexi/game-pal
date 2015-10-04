{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Game.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Lens
import Data.Data
import Data.Maybe

import Halive.Utils

data Cube = Cube
  { _cubPose  :: !(Pose GLfloat)
  , _cubColor :: !(V4 GLfloat)
  }
makeLenses ''Cube

data Shapes u1 u2 = Shapes
  { _shpCube   :: Shape u1
  , _shpMarker :: Shape u2 
  }
makeLenses ''Shapes


data World = World
  { _wldCubes  :: ![Cube]
  , _wldPlayer :: !(Pose GLfloat)
  , _wldTime   :: !Float
  }
makeLenses ''World

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uViewProjection      :: UniformLocation (M44 GLfloat)
  , uNormalMatrix        :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uRepelPosition       :: UniformLocation (V3  GLfloat)
  , uRepelStrength       :: UniformLocation GLfloat
  , uDiffuse             :: UniformLocation (V4  GLfloat)
  , uTime                :: UniformLocation GLfloat
  } deriving (Data)

main :: IO ()
main = do
  
  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "GamePal" NoGCPerFrame [UseOpenVR]

  -- Set up our cube resources
  cubeProg   <- createShaderProgram "app/jello.vert" "app/jello.frag"
  cubeGeo    <- icosahedronGeometry 1 5  --cubeGeometry (2 :: V3 GLfloat) (V3 30 30 30)
  cubeShape  <- makeShape cubeGeo cubeProg

  -- Set up our marker resources
  markerProg   <- createShaderProgram "app/marker.vert" "app/marker.frag"
  markerGeo    <- icosahedronGeometry 0.1 2
  markerShape  <- makeShape markerGeo markerProg--markerGeo markerProg

  let shapes = Shapes { _shpCube   = cubeShape
                      , _shpMarker = markerShape
                      }

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  let world = World 
        { _wldCubes = flip map [-5..5] $ 
            \x -> Cube 
              { _cubPose  = Pose { _posPosition    = V3 (x * 3) 0 0
                                 , _posOrientation = axisAngle  ( normalize ( V3 (sin x)  1 (cos ( x * 4.3 )) )  ) 0
                                 }

              , _cubColor = hslColor (x/10) 1 0.5 1
              }
        , _wldPlayer = newPose { _posPosition = V3 0 0 5 }
        , _wldTime = 0
        }
  void . flip runStateT world . whileWindow gpWindow $ do
    --liftIO . print =<< liftIO gpGetDelta

    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer
    
    viewMat <- viewMatrixFromPose <$> use wldPlayer
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shapes)

render :: (MonadIO m, MonadState World m) 
       => Shapes Uniforms Uniforms 
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shapes projection viewMat = do

  time <- use wldTime
  let cubeShape = shapes ^. shpCube
  let markerShape = shapes ^. shpMarker

  useProgram (sProgram cubeShape)
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      markerPos = V3  (sin( time * 0.1 )* 10 )  (sin( time * 1 )* 2 ) (sin( time * 1.3 )* 2 )

  uniformV3 uCamera eyePos
  uniformF  uTime time
  uniformV3 uRepelPosition markerPos
  uniformF  uRepelStrength 1

  withVAO (sVAO cubeShape) $ do

    cubes <- use wldCubes
    forM_ cubes $ \obj -> do
      uniformV4 uDiffuse (obj ^. cubColor)

      let model = mkTransformation (obj ^. cubPose . posOrientation) (obj ^. cubPose . posPosition)

      drawShape model projection viewMat cubeShape


  useProgram (sProgram markerShape)
  
 -- let Uniforms{..} = sUniforms markerShape
      --projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      --eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      --markerPos = V3 (sin( time * 0.5 )* 10) (sin( time * 1 )* 2) (sin( time * 2 )* 2) 

  withVAO (sVAO markerShape) $ do
    let model = mkTransformation ( Quaternion 0 (V3 0 1 0) ) markerPos

    drawShape model projection viewMat markerShape

drawShape :: MonadIO m  => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawShape model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr

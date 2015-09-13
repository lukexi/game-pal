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
-- import Halive.Utils

data Cube = Cube
  { _cubPose :: Pose
  , _cubColor :: V4 GLfloat
  }
makeLenses ''Cube

data World = World
  { _wldCubes  :: [Cube]
  , _wldPlayer :: Pose
  }
makeLenses ''World

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uDiffuse             :: UniformLocation (V4  GLfloat)
  } deriving (Data)

main :: IO ()
main = do

  GamePal{..} <- initGamePal "GamePal" [UseOculus]

  -- Set up our cube resources
  cubeProg   <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo    <- cubeGeometry (1 :: V3 GLfloat) (V3 1 1 1)
  cubeShape  <- makeShape cubeGeo cubeProg

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1

  useProgram (sProgram cubeShape)

  let world = World 
                (map (\x -> 
                  Cube (newPose & posPosition . _x .~ x) (hslColor (x/10) 1 0.5 1))
                  [-5..5])
                (newPose {_posPosition = V3 0 0 5})
  void . flip runStateT world . whileWindow gpWindow $ do
    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer
    
    viewMat <- viewMatrixFromPose <$> use wldPlayer
    renderWith gpWindow gpHMD viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render cubeShape)

render :: (MonadIO m, MonadState World m) 
       => Shape Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render cubeShape projection viewMat = do
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation

  uniformV3 uCamera eyePos

  withVAO (sVAO cubeShape) $ do

    cubes <- use wldCubes
    forM_ cubes $ \obj -> do
      uniformV4 uDiffuse (obj ^. cubColor)

      let model = mkTransformation (obj ^. cubPose . posOrientation) (obj ^. cubPose . posPosition)

      drawShape model projectionView cubeShape

drawShape :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Shape Uniforms -> m ()
drawShape model projectionView shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr

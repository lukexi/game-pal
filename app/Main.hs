{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Game.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal2
import Graphics.Oculus
import Graphics.GL
import Linear
import Control.Monad.State
import Control.Lens
import Data.Data
import Data.Maybe

data World = World
  { _wldCubes  :: [Pose]
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
  (window, events, maybeHMD, maybeRenderHMD, maybeSixenseBase) <- initWindow "GamePal" False False

  -- Set up our cube resources
  cubeProg   <- createShaderProgram "app/cube.vert" "app/cube.frag"
  cubeGeo    <- cubeGeometry (0.5 :: V3 GLfloat) (V3 1 1 1)
  cube       <- entity cubeGeo  cubeProg 

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE
  glCullFace GL_BACK
  useProgram (program cube)

  let world = World [newPose] (newPose {_posPosition = V3 0 0 5})
  void . flip runStateT world . whileWindow window $ do
    zoom wldPlayer $ do
      applyMouseLook window
      applyWASD window
      processEvents events $ \e -> do
        closeOnEscape window e
        applyGamepadJoystickMovement e

    case maybeRenderHMD of
      Nothing        -> renderFlat window    cube
      Just renderHMD -> renderVR   renderHMD cube

renderVR :: (MonadIO m, MonadState World m) 
         => RenderHMD -> Entity Uniforms -> m ()
renderVR renderHMD cube = renderHMDFrame renderHMD $ \eyeViews -> do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  view <- viewMatrixFromPose <$> use wldPlayer

  renderHMDEyes eyeViews $ \projection eyeView -> do

    let finalView = eyeView !*! view 

    render cube projection finalView 

renderFlat :: (MonadIO m, MonadState World m) 
           => Window -> Entity Uniforms -> m ()
renderFlat win cube = do

  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  projection  <- makeProjection win
  view <- viewMatrixFromPose <$> use wldPlayer

  render cube projection view

  swapBuffers win

render :: (MonadIO m, MonadState World m) 
       => Entity Uniforms
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render cube projection view = do
  let Uniforms{..} = uniforms cube
      projectionView = projection !*! view
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe view (inv44 view) ^. translation

  uniformV3 uCamera eyePos

  withVAO (vAO cube) $ do

    cubes <- use wldCubes
    forM_ cubes $ \obj -> do
      uniformV4 uDiffuse (V4 0 1 0 1)

      let model = mkTransformation (obj ^. posOrientation) (obj ^. posPosition)

      drawEntity model projectionView cube

drawEntity :: MonadIO m => M44 GLfloat -> M44 GLfloat -> Entity Uniforms -> m ()
drawEntity model projectionView anEntity = do 

  let Uniforms{..} = uniforms anEntity

  uniformM44 uModelViewProjection (projectionView !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model

  let vc = vertCount (geometry anEntity)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Graphics.VR.Pal
import Graphics.UI.GLFW.Pal
import Graphics.GL.Pal
import Graphics.GL
import Linear.Extra
import Control.Monad.State
import Control.Lens.Extra
import Data.Data
import Data.Maybe

import Halive.Utils

data RenderObject = RenderObject
  { _renPose  :: !(Pose GLfloat)
  , _renColor :: !(V4 GLfloat)
  }
makeLenses ''RenderObject

data Shapes u1 u2 = Shapes
  { _shpCube   :: Shape u1
  , _shpJello  :: Shape u2
  , _shpMarker :: Shape u2 
  }
makeLenses ''Shapes


data World = World
  { _wldCubes  :: ![RenderObject]
  , _wldJello  :: ![RenderObject]
  , _wldPlayer :: !(Pose GLfloat)
  , _wldTime   :: !Float
  , _wldHands  :: ![Hand]
  }
makeLenses ''World

data Uniforms = Uniforms
  { uModelViewProjection :: UniformLocation (M44 GLfloat)
  , uViewProjection      :: UniformLocation (M44 GLfloat)
  , uNormalMatrix        :: UniformLocation (M44 GLfloat)
  , uInverseModel        :: UniformLocation (M44 GLfloat)
  , uModel               :: UniformLocation (M44 GLfloat)
  , uCamera              :: UniformLocation (V3  GLfloat)
  , uRepelPosition1      :: UniformLocation (V3  GLfloat)
  , uRepelPosition2      :: UniformLocation (V3  GLfloat)
  , uRepelStrength       :: UniformLocation GLfloat
  , uDiffuse             :: UniformLocation (V4  GLfloat)
  , uTime                :: UniformLocation GLfloat
  } deriving (Data)

main :: IO ()
main = do

  gamePal@VRPal{..} <- reacquire 0 $ initVRPal "VRPal" [UseOpenVR]

  -- Set up our cube resources
  cubeProg     <- createShaderProgram "app/cube.vert" "app/logo.frag"
  cubeGeo      <- cubeGeometry (0.5 :: V3 GLfloat) (V3 50 50 50 )
  cubeShape    <- makeShape cubeGeo cubeProg


  jelloProg    <- createShaderProgram "app/jello.vert" "app/jello.frag"
  jelloGeo     <- icosahedronGeometry 0.3 4  
  jelloShape   <- makeShape jelloGeo jelloProg


  -- Set up our marker resources
  markerProg   <- createShaderProgram "app/marker.vert" "app/marker.frag"
  markerGeo    <- icosahedronGeometry 0.01 2
  markerShape  <- makeShape markerGeo markerProg--markerGeo markerProg

  let shapes = Shapes { _shpCube   = cubeShape
                      , _shpJello  = jelloShape
                      , _shpMarker = markerShape
                      }

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  let world = World 
        { _wldCubes = flip map [-1..1] $ 
            \x -> RenderObject 
              { _renPose  = Pose { _posPosition    = V3 (x) 1 (-0.5)
                                 , _posOrientation = axisAngle  ( normalize ( V3 (sin x)  1 (cos ( x * 4.3 )) )  ) 0
                                 }

              , _renColor = hslColor (x/10) 1 0.5 1
              }
        , _wldJello = flip map [-1..1] $ 
            \x -> RenderObject 
              { _renPose  = Pose { _posPosition    = V3 (x) 1 (0.5)
                                 , _posOrientation = axisAngle  ( normalize ( V3 (sin x)  1 (cos ( x * 4.3 )) )  ) 0
                                 }

              , _renColor = hslColor (x/10) 1 0.5 1
              }
        , _wldPlayer = newPose { _posPosition = V3 0 0 0 }
        , _wldTime = 0
        , _wldHands = [ emptyHand , emptyHand ]
        }
  void . flip runStateT world . whileWindow gpWindow $ do

    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    applyMouseLook gpWindow wldPlayer
    applyWASD      gpWindow wldPlayer

    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer

    -- Get latest Hydra data
    player <- use wldPlayer
    wldHands <~ fst <$> getHands gamePal
    --wldHands .= [emptyHand, emptyHand]

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
  let jelloShape = shapes ^. shpJello
  let markerShape = shapes ^. shpMarker



  {-

    Render Cubes
    
  -}

  useProgram (sProgram cubeShape)
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = safeInv44 viewMat ^. translation

  player <- use wldPlayer
  handsWorld <- handsToWorldPoses (transformationFromPose player) <$> use wldHands
  let markerPositions = map (^. translation) handsWorld

  uniformV3 uCamera eyePos
  uniformF  uTime time
  forM_ (zip [uRepelPosition1, uRepelPosition2] markerPositions) $ \(uRepelPosition, markerPosition) -> 
    uniformV3 uRepelPosition markerPosition
  uniformF  uRepelStrength 0.5

  withVAO (sVAO cubeShape) $ do

    cubes <- use wldCubes
    forM_ cubes $ \obj -> do
      uniformV4 uDiffuse (obj ^. renColor)

      let model = mkTransformation (obj ^. renPose . posOrientation) (obj ^. renPose . posPosition)

      drawShape' model projection viewMat cubeShape




  {-

    Render Jello
    
  -}
  useProgram (sProgram jelloShape)

  let Uniforms{..} = sUniforms jelloShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = safeInv44 viewMat ^. translation

  player <- use wldPlayer
  handsWorld <- handsToWorldPoses (transformationFromPose player) <$> use wldHands
  let markerPositions = map (^. translation) handsWorld

  uniformV3 uCamera eyePos
  uniformF  uTime time
  forM_ (zip [uRepelPosition1, uRepelPosition2] markerPositions) $ \(uRepelPosition, markerPosition) -> 
    uniformV3 uRepelPosition markerPosition
  uniformF  uRepelStrength 0.5

  withVAO (sVAO jelloShape) $ do

    jello <- use wldJello
    forM_ jello $ \obj -> do
      uniformV4 uDiffuse (obj ^. renColor)

      let model = mkTransformation (obj ^. renPose . posOrientation) (obj ^. renPose . posPosition)

      drawShape' model projection viewMat jelloShape




  {-

    Render Hands 
    
  -}

  useProgram (sProgram markerShape)
  
 -- let Uniforms{..} = sUniforms markerShape
      --projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      --eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      --markerPos = V3 (sin( time * 0.5 )* 10) (sin( time * 1 )* 2) (sin( time * 2 )* 2) 

  withVAO (sVAO markerShape) $ do

    forM_ markerPositions $ \position -> do
      let model = mkTransformation ( Quaternion 0 (V3 0 1 0) ) position

      drawShape' model projection viewMat markerShape

drawShape' :: MonadIO m  => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawShape' model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (safeInv44 model)
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = geoVertCount (sGeometry shape)
  glDrawElements GL_TRIANGLES vc GL_UNSIGNED_INT nullPtr




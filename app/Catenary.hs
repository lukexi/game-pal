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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word
import Debug.Trace
import System.Random
import Control.Monad.Random

import Halive.Utils


type ObjectID = Word32 

data Cube = Cube
  { _cubPose :: !(Pose GLfloat)
  , _cubColor :: !(V4 GLfloat)
  }
makeLenses ''Cube


data Line = Line
  { _linPose        :: !(Pose GLfloat)
  , _linStartPoint  :: !(V3 GLfloat)
  , _linEndPoint    :: !(V3 GLfloat)
  , _linColor       :: !(V4 GLfloat)
  , _linActive      :: !Int
  }
makeLenses ''Line

data Shapes u1 u2 = Shapes
  { _shpCube   :: Shape u1
  , _shpLine   :: Shape u1
  , _shpMarker :: Shape u2 
  }
makeLenses ''Shapes


data World = World
  { _wldCubes  :: ![Cube]
  , _wldLines  :: !(Map ObjectID Line)
  , _wldPlayer :: !(Pose GLfloat)
  , _wldTime   :: !Float
  , _wldClosest :: !Cube
  , _wldMarker  :: !(Pose GLfloat)
  , _wldTouching :: !Int
  , _wldActiveLineID :: !ObjectID
  , _wldActiveLine :: !Int
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
  , uStartPoint          :: UniformLocation (V3  GLfloat)
  , uEndPoint            :: UniformLocation (V3  GLfloat)
  , uRepelStrength       :: UniformLocation GLfloat
  , uDiffuse             :: UniformLocation (V4  GLfloat)
  , uTime                :: UniformLocation GLfloat
  } deriving (Data)

main :: IO ()
main = do

  gamePal@GamePal{..} <- reacquire 0 $ initGamePal "Catenary"  GCPerFrame [UseOpenVR]

  -- Set up our cube resources
  cubeProg   <- createShaderProgram "app/jello.vert" "app/jello.frag"
  cubeGeo    <- icosahedronGeometry 0.3 5  --cubeGeometry (2 :: V3 GLfloat) (V3 30 30 30)
  cubeShape  <- makeShape cubeGeo cubeProg

  -- Set up our marker resources
  markerProg   <- createShaderProgram "app/marker.vert" "app/marker.frag"
  markerGeo    <- icosahedronGeometry 0.02 2
  markerShape  <- makeShape markerGeo markerProg--markerGeo markerProg


    -- Set up our marker resources
  lineProg   <- createShaderProgram "app/line.vert" "app/line.frag"
  lineGeo    <- lineGeometry 40
  lineShape  <- makeShape lineGeo lineProg--markerGeo markerProg


  let shapes = Shapes{ _shpCube   = cubeShape
                     , _shpMarker = markerShape
                     , _shpLine   = lineShape
                     }

  glEnable GL_DEPTH_TEST
  glClearColor 0 0 0.1 1
  glEnable GL_CULL_FACE

  

  startRand <- getRandom

  let world = World 
        { _wldCubes = flip map [-5..5] $ 

            \x -> Cube 
              { _cubPose  = Pose { _posPosition    = getBasePoint x
                                 , _posOrientation = axisAngle  ( normalize ( V3 (sin x)  1 (cos ( x * 4.3 )) )  ) 0
                                 }

              , _cubColor = hslColor (x/10) 1 0.5 1
              }
        , _wldLines = mempty
        , _wldPlayer = newPose {_posPosition = V3 0 0 0}

        , _wldMarker = newPose {_posPosition = V3 3 2 3}
        , _wldTime = 0
        , _wldTouching = 0
        , _wldClosest = Cube 
              { _cubPose  = Pose { _posPosition    = V3 10000 0 0
                                 , _posOrientation = axisAngle  ( V3 0 1 0 ) 0
                                 }

              , _cubColor = hslColor 1 1 0.5 1
              }
        , _wldActiveLineID = startRand
        , _wldActiveLine = 0
        , _wldHands = [ emptyHand , emptyHand ]
        }

  void . flip runStateT world . whileWindow gpWindow $ do
    --liftIO . print =<< liftIO gpGetDelta

    delta <- realToFrac <$> liftIO gpGetDelta
    wldTime += delta

    time <- use wldTime


    -- Get latest Hydra data
    player <- use wldPlayer
    wldHands <~ fst <$> getHands gamePal

    
    handsWorld <- handsToWorldPoses (transformationFromPose player) <$> use wldHands
    let markerPositions = map (^. translation) handsWorld

    let x = sin( time * 5 ) * 10
        y = cos( time * 3 ) * 4
        z = cos( time * 4 ) * 5

    wldMarker .= newPose {_posPosition = markerPositions !! 0 }


    applyMouseLook gpWindow wldPlayer
    applyWASD gpWindow wldPlayer
    processEvents gpEvents $ \e -> do
      closeOnEscape gpWindow e
      applyGamepadJoystickMovement e wldPlayer


    shiftDown <- (== KeyState'Pressed) <$> getKey gpWindow Key'LeftShift
    whenKeyPressed gpWindow Key'Z           $ liftIO $ putStrLn $ "oh" ++ show 5 ++ " yeah"

    -- Getting the new closest cube!!!!

    cubes <- use wldCubes
    markerPose <- use wldMarker


    let markerP = markerPose ^. posPosition

    forM_ cubes $ \obj -> do

      closestCube <- use wldClosest

      let closestD = getLength $ (closestCube ^. cubPose . posPosition) - markerP

          pos = (obj ^. cubPose . posPosition)
          dif = pos - markerP
          len = getLength dif

      --liftIO $ putStrLn $ show len

      if len < closestD
        then do
          wldClosest .= obj
        else
          return ()






    closest <- use wldClosest
    touch   <- use wldTouching
    lines   <- use wldLines



    -- trying to work in logic of if I've touched cube
    let cubePos = (closest ^. cubPose . posPosition) 
    let l = getLength $ cubePos - markerP

    -- If we are within a touching radius 
    -- and are not touching yet, than touch
    if l < 0.3 && touch == 0 
      
      then do
        
        -- set touching, so we don't touch infinitely
        wldTouching .= 1

        activeLine <- use wldActiveLine

        -- if there is no active line
        -- then create a new line and set it to active
        if activeLine == 0
          
          then makeNewLine cubePos
            

          
          -- If there is an active, set the end point to our new touched object
          -- and 
          else if activeLine == 1 

            then do 

              --liftIO $ putStrLn $ show  "wha" 
              lines <- use wldLines
              activeLineID <- use wldActiveLineID

              let activeLine = wldLines . at activeLineID . traverse
              
              activeLine . linActive   .= 0
              activeLine . linEndPoint .= cubePos
              
{-}
              case Map.lookup activeLineID lines of
                Nothing -> do
                  liftIO $ putStrLn $ show "this is weird" 
                  return ()
                Just l -> do

                  --runStateT l
                  --linActive .= 0

                  wldLines . at activeLineID . traverse

                  l . linActive .= 0
                  ( l ^. linEndPoint) .= cubePos-}

                  --liftIO $ putStrLn $ show ( l ^. linActive )
              --let l = Map.lookup lines activeLineID

{-
              forM_ lines $ \( lineID, lineName ) -> do

                if lineID == activeLineID
                  then do return ()

                  else do return ()
             -- llll <- lines . at lineID


              --line <- lookup lines lineID
-}
              return ()

 --             line . linEndPoint .= cubePos

              

            else return ()

              


    else return ()



    -- If we are within a touching radius 
    -- and are not touching yet, than touch
    if l >= 4 && touch == 1
      then do
        -- set touching, so we don't touch infinitely
        wldTouching .= 0

    else return ()




    
    viewMat <- viewMatrixFromPose <$> use wldPlayer
    renderWith gamePal viewMat 
      (glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT))
      (render shapes )

render :: (MonadIO m, MonadState World m) 
       => Shapes Uniforms Uniforms 
       -> M44 GLfloat
       -> M44 GLfloat
       -> m ()
render shapes projection viewMat = do

  time <- use wldTime
  let cubeShape = shapes ^. shpCube
  let markerShape = shapes ^. shpMarker
  let lineShape = shapes ^. shpLine

  markerPose <- use wldMarker
  --let markerPos = V3  (sin( time * 0.1 )* 10 )  (sin( time * 1 )* 2 ) (sin( time * 1.3 )* 2 )
  let markerP = markerPose ^. posPosition

  useProgram (sProgram cubeShape)
  let Uniforms{..} = sUniforms cubeShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      markerPos = markerP

  player <- use wldPlayer
  handsWorld <- handsToWorldPoses (transformationFromPose player) <$> use wldHands
  let markerPositions = map (^. translation) handsWorld

  uniformV3 uCamera eyePos
  uniformF  uTime time
  forM_ (zip [uRepelPosition1, uRepelPosition2] markerPositions) $ \(uRepelPosition, markerPosition) -> 
    uniformV3 uRepelPosition markerPosition
  uniformF  uRepelStrength 1

  withVAO (sVAO cubeShape) $ do

    cubes <- use wldCubes
    forM_ cubes $ \obj -> do
      uniformV4 uDiffuse (obj ^. cubColor)

      let model = mkTransformation (obj ^. cubPose . posOrientation) (obj ^. cubPose . posPosition)

      drawShape model projection viewMat cubeShape



  useProgram (sProgram lineShape)
  let Uniforms{..} = sUniforms lineShape
      projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      markerPos = markerP

  uniformV3 uCamera eyePos
  uniformF  uTime time
  uniformV3 uRepelPosition1 markerPos
  uniformF  uRepelStrength 1

  closestCube <- use wldClosest
  let closestP = closestCube ^. cubPose . posPosition
  withVAO (sVAO lineShape) $ do


    lines <- use wldLines
    forM_ ( zip [0..] ( Map.toList lines ) ) $ \( i , (objID, obj) ) -> do
      uniformV4 uDiffuse (obj ^. linColor)
      uniformV3 uStartPoint ( obj ^. linStartPoint )

      if ( obj ^. linActive ) == 1
        then
          uniformV3 uEndPoint markerPos
        else
          uniformV3 uEndPoint ( obj ^. linEndPoint )

      let model = mkTransformation (obj ^. linPose . posOrientation) (obj ^. linPose . posPosition)

      drawLine model projection viewMat lineShape





  useProgram (sProgram markerShape)
  
 -- let Uniforms{..} = sUniforms markerShape
      --projectionView = projection !*! viewMat
      -- We extract eyePos from the view matrix to get Oculus offsets baked in
      --eyePos = fromMaybe viewMat (inv44 viewMat) ^. translation
      --markerPos = V3 (sin( time * 0.5 )* 10) (sin( time * 1 )* 2) (sin( time * 2 )* 2) 

  withVAO (sVAO markerShape) $ do
    let model = mkTransformation ( Quaternion 0 (V3 0 1 0) ) markerP

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



drawLine :: MonadIO m  => M44 GLfloat -> M44 GLfloat -> M44 GLfloat ->  Shape Uniforms -> m ()
drawLine model projection view shape = do 

  let Uniforms{..} = sUniforms shape

  uniformM44 uViewProjection      (projection !*! view)
  uniformM44 uModelViewProjection (projection !*! view !*! model)
  uniformM44 uInverseModel        (fromMaybe model (inv44 model))
  uniformM44 uModel               model
  uniformM44 uNormalMatrix        (transpose . safeInv44 $ view !*! model )

  let vc = vertCount (sGeometry shape)
  glDrawElements GL_LINES vc GL_UNSIGNED_INT nullPtr



getLength v = sqrt $ ( v ^. _x * v ^. _x ) +  ( v ^. _y * v ^. _y ) +  ( v ^. _z * v ^. _z )


getBasePoint i = position

  where x = sin( i * 10 ) * 2
        y = cos( i * 40 ) * 2 + 2
        z = sin( i * 100 ) * 2
        position = V3 x y z


makeNewLine cubePos = do

  id <- getRandom

  liftIO $ putStrLn $ show id

  wldActiveLine .= 1
  wldActiveLineID .= id

  wldLines . at id ?= Line
        { _linPose  = Pose { _posPosition    = V3 0 0 0
                           , _posOrientation = Quaternion 0 ( V3 0 1 0 )
                           }
        , _linStartPoint  = cubePos
        , _linEndPoint    = V3 0 0 0
        , _linColor = hslColor 1 1 0.5 1
        , _linActive = 1
        }

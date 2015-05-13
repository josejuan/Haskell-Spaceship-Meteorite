module SpaceShip.Objects.Common (
  module Graphics.UI.GLUT
, module Math.Space2D
, module SpaceShip.Class.State
, module System.Random
, update
, remove
, circle
, color3f
, color4f
) where

import Graphics.UI.GLUT hiding (position)
import Math.Space2D
import SpaceShip.Class.State
import System.Random

update :: (StateClass s, GameObjectClass go) => s -> go -> IO s
update s o = return $ updateGameObject (GameObject o) s

remove :: (StateClass s, GameObjectClass go) => s -> go -> IO s
remove s o = return $ removeGameObject (GameObject o) s

circle :: Int -> Vertexf -> GLfloat -> IO ()
circle points position radius = preservingMatrix $ do
    let k = 2 * pi / fromIntegral (points - 1)
    translate $ toVector position
    renderPrimitive LineStrip $ mapM_ (\i -> vertex $ radius ^* fromAngle (k * fromIntegral i)) [0 .. points]

color3f :: GLfloat -> GLfloat -> GLfloat -> IO ()
color3f r g b = color $ Color3 r g b

color4f :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> IO ()
color4f r g b a = color $ Color4 r g b a

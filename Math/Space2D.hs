{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Math.Space2D (
  (^.)
, (^*)
, ones3f
, zero3f
, xAxis
, yAxis
, zAxis
, Vertex3 (..)
, Vertexf
, vertex2f
, vertex3f
, dot
, angle
, diffAngle
, addAngle
, norm2
, norm
, unit
, x, y, z
, toVector
, rotatefv
, fromAngle
, toAngle
, toroidFixedCoordinate
) where

import Data.Fixed
import Control.Lens
import Graphics.Rendering.OpenGL.Raw.Types (GLfloat)
import Graphics.Rendering.OpenGL.GL (Vector3 (..))
import Graphics.Rendering.OpenGL.GL.Tensor (Vertex3 (..))
import System.Random

type Vertexf = Vertex3 GLfloat

vertex3f :: GLfloat -> GLfloat -> GLfloat -> Vertexf
vertex3f x y z = Vertex3 x y z

vertex2f :: GLfloat -> GLfloat -> Vertexf
vertex2f x y = vertex3f x y 0

ones3f = vertex3f 1 1 1
zero3f = vertex3f 0 0 0
xAxis  = vertex3f 1 0 0
yAxis  = vertex3f 0 1 0
zAxis  = vertex3f 0 0 1

vertexXf = vertex3f 1 0 0
vertexYf = vertex3f 0 1 0
vertexZf = vertex3f 0 0 1

makeLenses ''Vertex3

x, y, z :: Functor f => (a -> f a) -> Vertex3 a -> f (Vertex3 a)
x f (Vertex3 a b c) = fmap (\a' -> Vertex3 a' b c) (f a)
y f (Vertex3 b a c) = fmap (\a' -> Vertex3 b a' c) (f a)
z f (Vertex3 b c a) = fmap (\a' -> Vertex3 b c a') (f a)

instance Random (Vertex3 GLfloat) where
    randomR (a, b) g = let (r, g1) = randomR (a^.x, b^.x) g
                           (s, g2) = randomR (a^.y, b^.y) g1
                           (t, g3) = randomR (a^.z, b^.z) g2
                       in  (Vertex3 r s t, g3)
    random         g = let (r, g1) = random               g
                           (s, g2) = random               g1
                           (t, g3) = random               g2
                       in  (Vertex3 r s t, g3)

instance Num (Vertex3 GLfloat) where
    u + v = Vertex3 (u^.x + v^.x) (u^.y + v^.y) (u^.z + v^.z)
    u - v = Vertex3 (u^.x - v^.x) (u^.y - v^.y) (u^.z - v^.z)
    u * v = Vertex3 (u^.y * v^.z - u^.z * v^.y) (u^.z * v^.x - u^.x * v^.z) (u^.x * v^.y - u^.y * v^.x)
    abs u = Vertex3 (abs u^.x) (abs u^.y) (abs u^.z)
    signum = error "`Vertex3 Float` cannot implement `signum`"
    fromInteger = error "`Vertex3 Float` cannot implement `fromInteger`"

(^*) :: GLfloat -> Vertexf -> Vertexf
k ^* v = vertex3f (k * (v^.x)) (k * (v^.y)) (k * (v^.z))
infixr 8 ^*

toVector :: Vertexf -> Vector3 GLfloat
toVector (Vertex3 x y z) = Vector3 x y z

dot :: Vertexf -> Vertexf -> GLfloat
u `dot` v = u^.x * v^.x + u^.y * v^.y + u^.z * v^.z

norm2 :: Vertexf -> GLfloat
norm2 u = u `dot` u

norm :: Vertexf -> GLfloat
norm  u = sqrt (norm2 u)

unit :: Vertexf -> Vertexf
unit v = (1 / norm v) ^* v

fixedCoordinate :: GLfloat -> GLfloat -> GLfloat -> GLfloat
fixedCoordinate a b v | v < a     = v - a + b
                      | v > b     = v - b + a
                      | otherwise = v

toroidFixedCoordinate :: Vertexf -> Vertexf -> Vertexf -> Vertexf
toroidFixedCoordinate (Vertex3 ax ay _) (Vertex3 bx by _) (Vertex3 x y _) =
    Vertex3 (fixedCoordinate ax bx x) (fixedCoordinate ay by y) 0

fromAngle :: GLfloat -> Vertexf
fromAngle a = vertex2f (cos a) (sin a)

angle :: Vertexf -> Vertexf -> GLfloat
angle u v = acos $ (u `dot` v) / (norm u * norm v)

toAngle :: Vertexf -> GLfloat
toAngle v@(Vertex3 x y _) | y >= 0    = angle xAxis v
                          | otherwise = 2 * pi - angle xAxis v

diffAngle :: GLfloat -> GLfloat -> GLfloat
diffAngle a b | ad > pi   = (2 * pi - ad) * signum d
              | otherwise = d
                where d  = b - a
                      ad = abs d

addAngle :: GLfloat -> GLfloat -> GLfloat
addAngle a b = (a + b) `mod'` (2 * pi)

rotatefv :: GLfloat -> Vertexf -> Vertexf
rotatefv a v = vertex2f (v^.x * c - v^.y * s) (v^.x * s + v^.y * c)
               where (c, s) = (cos a, sin a)
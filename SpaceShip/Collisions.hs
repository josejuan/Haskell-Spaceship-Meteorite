module SpaceShip.Collisions (
  getCollisionsPairs
, resolvePureElasticCollision
) where

import Data.List
import Data.Maybe
import Data.Function
import Data.Map ((!))
import qualified Data.Map as M
import Graphics.UI.GLUT hiding (position)
import Math.Space2D
import SpaceShip.Class.State
import SpaceShip.State

getCollisionsPairs :: [GameObject] -> [(GameObject, GameObject)]
getCollisionsPairs gos =
    let sweepX :: [( GLfloat            -- sweep on x
                   , ( Bool             -- starting object coordinate
                     , GameObject ))]   -- object
        sweepX = sortBy (compare `on` fst) $ concat
                    [ [ (p - r, (True , go))
                      , (p + r, (False, go))]
                                | go <- gos
                                , let k = kind go
                                      p = position k ^. x
                                      r = bradius  k
                                , isPhysical k && not (isSmoke k) ]
    in  snd $ foldr checkCollision (M.empty, []) sweepX

checkCollision :: (GLfloat, (Bool, GameObject))
               -> (M.Map GameObjectKey GameObject, [(GameObject, GameObject)])
               -> (M.Map GameObjectKey GameObject, [(GameObject, GameObject)])
checkCollision (_, (False, go)) (m, ps) = (M.delete (key go) m, ps)
checkCollision (_, (True , go)) (m, ps) =
    case firstCollisionWith m go of
        Nothing -> (M.insert (key go) go m, ps)
        Just og -> (M.delete (key og)    m, (go, og): ps)

firstCollisionWith :: M.Map GameObjectKey GameObject -> GameObject -> Maybe GameObject
firstCollisionWith m go =
    let k0 = kind go
        u0 = position k0 ^. x
        v0 = position k0 ^. y
        r0 = bradius  k0
    in  listToMaybe [ og | og <- M.elems m
                    , let k1 = kind og
                          u1 = position k1 ^. x
                          v1 = position k1 ^. y
                          r1 = bradius  k1
                          dr = r0 + r1
                          du = u0 - u1
                          dv = v0 - v1
                    , abs dv <= dr                  -- bounding box
                    , du * du + dv * dv <= dr * dr  -- bounding circle
                    ]

resolvePureElasticCollision :: (GameObject, GameObject) -> State -> State
resolvePureElasticCollision (g1, g2) st =
    -- simple pseudo-elastic collision
    let (k1, k2) = (kind     g1, kind     g2)
        (p1, p2) = (position k1, position k2)
        (r1, r2) = (bradius  k1, bradius  k2)
        (v1, v2) = (velocity k1, velocity k2)
        (m1, m2) = (mass     k1, mass     k2)
        (i1, i2) = (m2 / (m1 + m2), m1 / (m1 + m2))
        dv       = v1 - v2
        dp       = p1 - p2
        (w1, w2) = ( v1 - m2 ^* rv, v2 + m1 ^* rv )
                   where rv = ((2 * dv `dot` dp) / ((m1 + m2) * norm2 dp)) ^* dp
        qp       = ((r1 + r2 - np) / np) ^* dp where np = norm dp
        (q1, q2) = (p1 + i1 ^* qp, p2 - i2 ^* qp)
    in  updateGameObject (updateKind g1 $ k1 { position = q1, velocity = w1, collided = Just g2 } ) $
        updateGameObject (updateKind g2 $ k2 { position = q2, velocity = w2, collided = Just g1 } ) st

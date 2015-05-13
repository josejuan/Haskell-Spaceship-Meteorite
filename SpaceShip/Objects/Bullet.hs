{-# LANGUAGE RecordWildCards #-}
module SpaceShip.Objects.Bullet (
  BulletObject
, makeBulletObject
, constructBullet
) where

import SpaceShip.Objects.Common
import SpaceShip.Objects.Particle (makeParticleObject, sprayGenerator)

maxBulletDuration = 5 -- seconds

data BulletObject =
     BulletObject_ { bulletKey          :: GameObjectKey
                   , bulletKind         :: ObjectKind
                   , bulletDuration     :: GLfloat
                   }

constructBullet :: Vertexf -> Vertexf -> IO BulletObject
constructBullet pos vel = return $ BulletObject_ 0 (Physical 10 pos vel zero3f 1.0 Nothing False) 0

makeBulletObject :: BulletObject -> GameObjectKey -> GameObject
makeBulletObject m k = GameObject $ m { bulletKey = k }

instance GameObjectClass BulletObject where
    key' = bulletKey
    kind' = bulletKind
    updateKind' bullet newKind = bullet { bulletKind = newKind }
    render' (BulletObject_ {..}) = do
        lineWidth $= 2
        color3f 0.3 0.3 1
        circle 10 (position bulletKind) (bradius bulletKind)
    animateEnd' (dt, t) st o@(BulletObject_ {..}) =
        case collided k of
            Nothing     -> updateCurrent
            Just  og    -> do
                                let ogp = position $ kind og
                                    mkp = sprayGenerator 3 0.7 (vertex3f 0.9 0.0 0) 2 (pi/2) (position k) (10 ^* unit (position k - ogp))
                                ps <- mapM (const mkp) [1..20]
                                flip remove o $ foldr (addGameObject . makeParticleObject) st ps
        where k = bulletKind
              updateCurrent = if bulletDuration > maxBulletDuration
                                then remove st o
                                else update st $ o { bulletKind = toroidFixedPhysicalPosition st $ updatePhysicalMovement dt k
                                                   , bulletDuration = bulletDuration + dt }

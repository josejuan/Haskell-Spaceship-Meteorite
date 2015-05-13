{-# LANGUAGE RecordWildCards #-}
module SpaceShip.Objects.Particle (
  ParticleObject
, makeParticleObject
, sprayGenerator
, updateParticleKind
) where

import SpaceShip.Objects.Common

data ParticleObject =
     ParticleObject_ { particleKey          :: GameObjectKey
                     , particleKind         :: ObjectKind
                     , particleDuration     :: GLfloat
                     , particleColor        :: Vertexf
                     }

updateParticleKind :: ParticleObject -> ObjectKind -> ParticleObject
updateParticleKind p k = p { particleKind = k }
                     
sprayGenerator :: GLfloat -> GLfloat -> Vertexf -> Int -> GLfloat -> Vertexf -> Vertexf -> IO ParticleObject
sprayGenerator duration vrnd color presure angle pos vel = do
    t <- randomRIO (-1.0, 1.0)
    v <- randomRIO (vrnd, 1.0)
    constructParticle duration color pos (rotatefv (0.5 * angle * t^(2 * presure - 1)) (v ^* vel))
                     
constructParticle :: GLfloat -> Vertexf -> Vertexf -> Vertexf -> IO ParticleObject
constructParticle duration color pos vel = return $ ParticleObject_ 0 (Physical 10 pos vel zero3f 0.75 Nothing True) duration color

makeParticleObject :: ParticleObject -> GameObjectKey -> GameObject
makeParticleObject m k = GameObject $ m { particleKey = k }

instance GameObjectClass ParticleObject where
    key' = particleKey
    kind' = particleKind
    updateKind' particle newKind = particle { particleKind = newKind }
    render' (ParticleObject_ {..}) = do
        let fade = min 1.0 particleDuration
            (Vertex3 r g b) = particleColor
        color3f (fade * r) (fade * g) (fade * b)
        lineWidth $= 1
        circle 10 (position particleKind) (bradius particleKind)
    animateEnd' (dt, t) st o@(ParticleObject_ {..}) =
        if particleDuration < 0
            then remove st o
            else update st $ o { particleKind = toroidFixedPhysicalPosition st $ updatePhysicalMovement dt particleKind
                               , particleDuration = particleDuration - dt }

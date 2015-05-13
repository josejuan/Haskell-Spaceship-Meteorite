{-# LANGUAGE RecordWildCards #-}
module SpaceShip.Objects.Meteor (
  MeteorObject
, makeMeteorObject
, constructMeteor
) where

import SpaceShip.Objects.Common
import Control.Monad
import SpaceShip.Objects.Particle (makeParticleObject, sprayGenerator, updateParticleKind)

meteorDensity = 2000.0 -- 2.000 Kg / m^3
minRadius = 5 -- m

data MeteorObject =
     MeteorObject_ { meteorKey      :: GameObjectKey
                   , meteorKind     :: ObjectKind
                   , meteorAngle    :: GLfloat
                   , meteorAngleV   :: GLfloat -- angle / second
                   , meteorPoints   :: [Vertexf] -- to do: replace by opengl object
                   }

randomMeteorPerimeter :: Int -> [GLfloat] -> IO [GLfloat]
randomMeteorPerimeter 0 = return
randomMeteorPerimeter n = randomMeteorPerimeter (n - 1) >=> grow
    where grow (a:b:xs) = do
                            ab <- randomRIO (a, b)
                            ys <- grow (b:xs)
                            return (a:ab:ys)
          grow xs = return xs

constructMeteor :: Vertexf -> GLfloat -> GLfloat -> IO MeteorObject
constructMeteor position minRadius maxRadius = do
    let nr = 5
    qs <- mapM (const $ randomRIO (minRadius, maxRadius)) [1..nr]
    ks <- randomMeteorPerimeter 2 (last qs : qs)
    va <- randomRIO (-270, 270)
    vp  <- randomRIO (2.5, 10 :: GLfloat)
    dir <- randomRIO (0, 2 * pi)
    let fs = fromIntegral $ length ks
        rs = [vertex2f (cos a) (sin a) | i <- [1..fs], let a = (i * 2 * pi) / (fs - 1)]
        ps = zipWith (^*) ks rs
        avgRadius = sum ks / fs
        v  = 4 * pi * avgRadius / 3
        m  = meteorDensity * v
    return $ MeteorObject_ 0 (Physical m position (vp ^* fromAngle dir) zero3f avgRadius Nothing False) 0 va ps

makeMeteorObject :: MeteorObject -> GameObjectKey -> GameObject
makeMeteorObject m k = GameObject $ m { meteorKey = k }
                  
instance GameObjectClass MeteorObject where
    key' = meteorKey
    kind' = meteorKind
    updateKind' meteor newKind = meteor { meteorKind = newKind }
    render' (MeteorObject_ {..}) =
        preservingMatrix  $ do
            color3f (189/255) (119/255) (84/255)
            lineWidth $= 4
            translate $ toVector $ position meteorKind
            rotate meteorAngle (toVector zAxis)
            renderPrimitive LineStrip $ mapM_ vertex meteorPoints
    animateStart' (dt, t) st o@(MeteorObject_ {..}) =
        update st $ o { meteorAngle = t * meteorAngleV }
    animateEnd' (dt, t) st o@(MeteorObject_ {..}) =
        case collided meteorKind of
            Nothing     -> updateCurrent
            Just og     -> splitMeteor og

        where updateCurrent = update st $ o { meteorKind = toroidFixedPhysicalPosition st $ updatePhysicalMovement dt meteorKind }
        
              splitMeteor og = do
                    let k  = meteorKind
                        p  = position k
                        v  = velocity k
                        r  = 1.4 * bradius k
                        r' = r / (1 + 1 / acos (pi / 6)) -- three balls into one
                        (d1, d2, d3) = ( unit $ velocity k
                                       , fromAngle $ addAngle (  2 *pi/3) $ toAngle d1
                                       , fromAngle $ addAngle ((-2)*pi/3) $ toAngle d1 )
                        (p1, p2, p3) = ( p + (r - r') ^* d1
                                       , p + (r - r') ^* d2
                                       , p + (r - r') ^* d3 )
                    m1 <- constructMeteor p1 (0.5 * r') r'
                    m2 <- constructMeteor p2 (0.5 * r') r'
                    m3 <- constructMeteor p3 (0.5 * r') r'
                    let adjustVelocity m@(MeteorObject_ {..}) v = m { meteorKind = meteorKind { velocity = v } }
                        st' | r' < minRadius = st
                            | otherwise = addGameObject (makeMeteorObject $ adjustVelocity m1 $ v     ) $
                                          addGameObject (makeMeteorObject $ adjustVelocity m2 $ v + d2) $
                                          addGameObject (makeMeteorObject $ adjustVelocity m3 $ v + d3) $ st
                        ogp = position $ kind og
                        mkp = do
                                 m <- sprayGenerator 1 0 (vertex3f 0.75 0.63 0.5) 1 (2 * pi) (position k) (10 ^* unit (ogp - p))
                                 let mk = kind $ GameObject m
                                     vk = velocity mk
                                 t <- randomRIO (0, 1)
                                 return $ updateParticleKind m $ mk { position = position mk + (t * bradius k / norm vk) ^* vk }
                    ps <- mapM (const mkp) [1..3 * round (mass k / meteorDensity)]
                    flip remove o $ foldr (addGameObject . makeParticleObject) st' ps

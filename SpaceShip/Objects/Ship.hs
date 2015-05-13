{-# LANGUAGE RecordWildCards #-}
module SpaceShip.Objects.Ship (
  ShipObject
, makeShipObject
, constructShip
) where

import SpaceShip.Objects.Common
import SpaceShip.Objects.Bullet
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe
import SpaceShip.Objects.Particle (makeParticleObject, sprayGenerator)

btnLEFT, btnRIGHT, btnACCELERATE, btnDECELERATE, btnFIRE, btnLEFTSTRAFE, btnRIGHTSTRAFE :: Int
btnLEFT             = 0     -- front right direction engine
btnRIGHT            = 1     -- front left direction engine
btnACCELERATE       = 2     -- both rear engines
btnDECELERATE       = 3     -- both front engines
btnFIRE             = 4     -- bullet gun
btnLEFTSTRAFE       = 5     -- front and rear right direction engine
btnRIGHTSTRAFE      = 6     -- front and rear left direction engine

data ShipObject =
     ShipObject_ { shipKey          :: GameObjectKey
                 , shipKind         :: ObjectKind
                 , shipEngineThrust :: GLfloat      -- newtons de empuje
                 , shipAngle        :: GLfloat
                 , shipAngleV       :: GLfloat
                 , shipAngleA       :: GLfloat      -- aceleración aplicada (ignoramos masa) cuando se activan motores de giro
                 , shipPoints       :: [Vertexf]    -- to do: replace by opengl object
                 , shipButtons      :: M.Map Int Bool
                 }

getBtn :: ShipObject -> Int -> Bool
getBtn (ShipObject_ {..}) btn = isJust $ M.lookup btn shipButtons

setBtn :: Int -> Bool -> ShipObject -> ShipObject
setBtn btn activated so@(ShipObject_ {..}) | activated = so { shipButtons = M.insertWith (||) btn True shipButtons }
                                           | otherwise = so { shipButtons = M.delete btn shipButtons }

constructShip :: IO ShipObject
constructShip = do
    let af = 0.6      -- las puntas traseras en los ángulos `3Pi/2 +/- af`
        sr = 6        -- radio (metros)
        a1 = 3 * pi / 2 - af
        a2 = 3 * pi / 2 + af
        ps = [vertex2f 0 sr, sr ^* fromAngle a1, zero3f, sr ^* fromAngle a2]
    return $ ShipObject_ 0
                         (Physical 200 zero3f zero3f zero3f sr Nothing False)
                         20000.0
                         0.0 0.0 1.0
                         (last ps : ps) M.empty

makeShipObject :: ShipObject -> GameObjectKey -> GameObject
makeShipObject m k = GameObject $ m { shipKey = k }

instance GameObjectClass ShipObject where
    key' = shipKey
    kind' = shipKind
    updateKind' ship newKind = ship { shipKind = newKind }
    render' (ShipObject_ {..}) =
        preservingMatrix  $ do
            let p = position shipKind
                v = velocity shipKind
                r = bradius  shipKind
                d = fromAngle shipAngle
                q = 100 ^* d -- (p + (1.1 + r) ^* d) ((5.0 + norm v) ^* d)
            lineWidth $= 2
            color3f 1 1 1
            translate $ toVector $ position shipKind
            rotate (180 * (shipAngle - pi / 2) / pi) (toVector zAxis)
            renderPrimitive LineStrip $ mapM_ vertex shipPoints
    animateStart' (dt, t) st o@(ShipObject_ {..}) =
        do
            let velocityAngle = toAngle $ velocity shipKind
                deltaVA = diffAngle shipAngle velocityAngle -- signed angle between ship's orientation and velocity direction
                shipAngleV' | getBtn o btnLEFT      = shipAngleV + shipAngleA * dt
                            | getBtn o btnRIGHT     = shipAngleV - shipAngleA * dt
                            | otherwise             = shipAngleV {- comment the right expression if you are brave! -} * (1 - dt)**3
                shipAngle'  = addAngle shipAngle (shipAngleV' * dt)
                direction   = fromAngle shipAngle'
                accforce' = accforce shipKind + f
                            where f | getBtn o btnACCELERATE    =   shipEngineThrust  ^* direction
                                    | getBtn o btnDECELERATE    = (-shipEngineThrust) ^* direction
                                    | getBtn o btnLEFTSTRAFE    =   shipEngineThrust  ^* (zAxis * direction)
                                    | getBtn o btnRIGHTSTRAFE   =   shipEngineThrust  ^* (direction * zAxis)
                                    | otherwise                 = zero3f
                mparticle | getBtn o btnACCELERATE  = Just ((-1) ^* direction)
                          | getBtn o btnDECELERATE  = Just (        direction)
                          | getBtn o btnLEFTSTRAFE  = Just (direction * zAxis)
                          | getBtn o btnRIGHTSTRAFE = Just (zAxis * direction)
                          | otherwise               = Nothing
            st' <- case mparticle of
                     Just  d -> do
                                    p <- sprayGenerator 1.5 0.5 (vertex3f 0.9 0.8 0) 2 (pi/4) (position shipKind) (60 ^* d - velocity shipKind)
                                    return $ addGameObject (makeParticleObject p) st
                     Nothing -> return st
            update st' $ o { shipAngle      = shipAngle'
                           , shipAngleV     = shipAngleV'
                           , shipKind       = shipKind { accforce = accforce' }
                           }
    animateEnd' (dt, t) st o@(ShipObject_ {..}) =
        update st $ o { shipKind = toroidFixedPhysicalPosition st $ updatePhysicalMovement dt shipKind }
    deviceInputs' key keyState modifiers vposition st o@(ShipObject_ {..}) =
        case (key, keyState, modifiers) of
            (Char        'a'        , Down, _) -> update st $ setBtn btnLEFT            True  o
            (Char        'a'        , Up  , _) -> update st $ setBtn btnLEFT            False o
            (Char        'd'        , Down, _) -> update st $ setBtn btnRIGHT           True  o
            (Char        'd'        , Up  , _) -> update st $ setBtn btnRIGHT           False o
            (SpecialKey KeyUp       , Down, _) -> update st $ setBtn btnACCELERATE      True  o
            (SpecialKey KeyUp       , Up  , _) -> update st $ setBtn btnACCELERATE      False o
            (SpecialKey KeyDown     , Down, _) -> update st $ setBtn btnDECELERATE      True  o
            (SpecialKey KeyDown     , Up  , _) -> update st $ setBtn btnDECELERATE      False o
            (SpecialKey KeyLeft     , Down, _) -> update st $ setBtn btnLEFTSTRAFE      True  o
            (SpecialKey KeyLeft     , Up  , _) -> update st $ setBtn btnLEFTSTRAFE      False o
            (SpecialKey KeyRight    , Down, _) -> update st $ setBtn btnRIGHTSTRAFE     True  o
            (SpecialKey KeyRight    , Up  , _) -> update st $ setBtn btnRIGHTSTRAFE     False o
            (Char        ' '        , Up  , _) -> do
                let p = position shipKind
                    v = velocity shipKind
                    r = bradius  shipKind
                    d = fromAngle shipAngle
                bullet <- constructBullet (p + (1.1 + r) ^* d) (max 100 (1.5 * norm v) ^* d)
                return $ addGameObject (makeBulletObject bullet) st
            _ -> return st

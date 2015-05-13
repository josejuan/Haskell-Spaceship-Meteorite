{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, GADTs, FlexibleContexts, ExistentialQuantification #-}
module SpaceShip.Class.State (
  GameObjectClass (..)
, GameObject (..)
, updatePhysicalMovement
, toroidFixedPhysicalPosition
, key
, render
, animateStart
, animateEnd
, deviceInputs
, kind
, updateKind
, GameObjectKey
, StateClass (..)
, ObjectKind (..)
, isPhantom
, isPhysical
) where

import Math.Space2D
import Graphics.UI.GLUT hiding (position)

type GameObjectKey = Int
type TimeState = (GLfloat, GLfloat)

data ObjectKind = Phantom -- ethereal entity!
                | Physical { mass       :: GLfloat          -- kg
                           , position   :: Vertexf          -- metres
                           , velocity   :: Vertexf          -- metres / seconds
                           , accforce   :: Vertexf          -- newtons (accumulated force on each step)
                           , bradius    :: GLfloat          -- bound radius (for collision purposes)
                           , collided   :: Maybe GameObject -- one pending (not processed) collision with some other object
                           , isSmoke    :: Bool             -- not collidable
                           }

isPhantom :: ObjectKind -> Bool
isPhantom Phantom = True
isPhantom _       = False

isPhysical :: ObjectKind -> Bool
isPhysical (Physical {..}) = True
isPhysical _               = False

updatePhysicalMovement :: GLfloat -> ObjectKind -> ObjectKind
updatePhysicalMovement dt go@(Physical {..}) =
    let acceleration = (1.0 / mass) ^* accforce
    in  go { -- simple Euler EDO solver
             position = position + dt ^* velocity
           , velocity = velocity + dt ^* acceleration
           , accforce = zero3f
           }

toroidFixedPhysicalPosition :: StateClass s => s -> ObjectKind -> ObjectKind
toroidFixedPhysicalPosition s go@(Physical {..}) =
    let vws = virtualViewportSize s
    in  go { position = toroidFixedCoordinate ((-0.5) ^* vws) (0.5 ^* vws) position }
    

class GameObjectClass go where
    key'            :: go -> GameObjectKey
    render'         :: go -> IO ()
    render' _ = return () -- default nothing rendered
    animateStart'        :: StateClass s => TimeState -> s -> go -> IO s
    animateStart' _ s _ = return s -- default not animated
    animateEnd'        :: StateClass s => TimeState -> s -> go -> IO s
    animateEnd' _ s _ = return s -- default not animated
    deviceInputs'   :: StateClass s => Key -> KeyState -> Modifiers -> Vertexf -> s -> go -> IO s
    deviceInputs' _ _ _ _ s _ = return s -- default ignore device inputs
    kind'           :: go -> ObjectKind
    kind' _ = Phantom
    updateKind'     :: go -> ObjectKind -> go

data GameObject where
     GameObject :: GameObjectClass go => go -> GameObject

key :: GameObject -> GameObjectKey
key (GameObject go) = key' go

render :: GameObject -> IO ()
render (GameObject go) = render' go

animateStart :: StateClass s => TimeState -> s -> GameObject -> IO s
animateStart t s (GameObject go) = animateStart' t s go

animateEnd :: StateClass s => TimeState -> s -> GameObject -> IO s
animateEnd t s (GameObject go) = animateEnd' t s go

deviceInputs :: StateClass s => Key -> KeyState -> Modifiers -> Vertexf -> s -> GameObject -> IO s
deviceInputs k ks m vp s (GameObject go) = deviceInputs' k ks m vp s go

kind :: GameObject -> ObjectKind
kind (GameObject go) = kind' go

updateKind :: GameObject -> ObjectKind -> GameObject
updateKind (GameObject go) ok = GameObject $ updateKind' go ok

class StateClass o where

    virtualViewportSize     :: o -> Vertexf -- width/height metres (0,0) centered
    physicalToVirtual       :: o -> (Position -> Vertexf)
    setPhysicalToVirtual    :: (Position -> Vertexf) -> o -> o
    
    addGameObject           :: (GameObjectKey -> GameObject) -> o -> o
    removeGameObject        :: GameObject -> o -> o
    updateGameObject        :: GameObject -> o -> o
    gameObjects             :: o -> [GameObject]
    
    getElapsedTime          :: o -> IO (TimeState, o)

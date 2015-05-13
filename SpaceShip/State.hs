{-# LANGUAGE RecordWildCards, MultiParamTypeClasses, FlexibleInstances #-}
module SpaceShip.State (
  State
, makeState
) where

import SpaceShip.Class.State
import Math.Space2D
import Graphics.UI.GLUT (Position)
import qualified Data.Map as M
import qualified TimeInterval as TI

data State = State_ { virtualViewportSize_    :: Vertexf
                    , physicalToVirtual_      :: Position -> Vertexf
                    , nextGameObjectKey_      :: GameObjectKey
                    , gameObjectsMap_         :: M.Map GameObjectKey GameObject
                    , timeInterval            :: TI.Time
                    , accumulatedInterval     :: Double
                    }

instance StateClass State where
    virtualViewportSize st = virtualViewportSize_ st
    physicalToVirtual st = physicalToVirtual_ st
    setPhysicalToVirtual f st = st { physicalToVirtual_ = f }
    addGameObject makeGameObject st@(State_ {..}) =
        st { nextGameObjectKey_ = nextGameObjectKey_ + 1
           , gameObjectsMap_    = M.insert nextGameObjectKey_ (makeGameObject nextGameObjectKey_) gameObjectsMap_ }
    removeGameObject gameObject st@(State_ {..}) =
        st { gameObjectsMap_ = M.delete (key gameObject) gameObjectsMap_ }
    updateGameObject gameObject st@(State_ {..}) =
        st { gameObjectsMap_ = M.update (const $ Just gameObject) (key gameObject) gameObjectsMap_ }
    gameObjects = M.elems . gameObjectsMap_
    getElapsedTime st@(State_ {..}) = do
        (dt, timeInterval') <- TI.getElapsed timeInterval
        let t = accumulatedInterval + dt
        return ((realToFrac dt, realToFrac t), st { timeInterval = timeInterval', accumulatedInterval = t })

makeState :: Vertexf -> IO State
makeState virtualViewportSize = do
    ti <- TI.makeTime
    return $ State_ virtualViewportSize undefined 0 M.empty ti 0

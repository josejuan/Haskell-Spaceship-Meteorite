{-# LANGUAGE ExistentialQuantification, RecordWildCards #-}
module SpaceShip where

import Control.Monad
import Graphics.UI.GLUT hiding (position)
import Data.IORef
import Math.Space2D
import SpaceShip.Class.State
import SpaceShip.State
import SpaceShip.Objects
import System.Random
import qualified SpaceShip.Objects.Ship as SO
import SpaceShip.Collisions

-- fighting area dimensions
wKm = 450 -- m
hKm = 400 -- m

onReshape :: IORef State -> Size -> IO ()
onReshape rst size@(Size w h) = do
    st0 <- readIORef rst
    viewport $= (Position 0 0, size)
    matrixMode $= Projection
    loadIdentity
    let wf = fromIntegral w
        hf = fromIntegral h
        Vertex3 vw vh _ = virtualViewportSize st0
        (xa, xb, ya, yb) = if w <= h
                                then (-0.5 * vw, 0.5 * vw, -0.5 * vh * hf / wf, 0.5 * vh * hf / wf)
                                else (-0.5 * vw * wf / hf, 0.5 * vw * wf / hf, -0.5 * vh, 0.5 * vh)
    ortho2D (realToFrac xa) (realToFrac xb) (realToFrac ya) (realToFrac yb)
    matrixMode $= Modelview 0
    loadIdentity
    writeIORef rst $ setPhysicalToVirtual
                        (\(Position x y) -> vertex2f (xa + fromIntegral x * (xb - xa) / wf)
                                                     (yb + fromIntegral y * (ya - yb) / hf))
                        st0

onIdle :: IORef State -> IO ()
onIdle rst = do
    st0 <- readIORef rst
    (ts@(dt, t), st0') <- getElapsedTime st0
    do
        let gos = gameObjects st0'
            st1 = foldr resolvePureElasticCollision st0' $ getCollisionsPairs gos
        st2 <- foldM (animateStart ts) st1 (gameObjects st1)
        st3 <- foldM (animateEnd   ts) st2 (gameObjects st2)
        st4 <- if null (tail gos) then insertMeteors 5 st3 else return st3
        writeIORef rst st4
        postRedisplay Nothing

renderCollisionBound :: GameObject -> IO ()
renderCollisionBound go =
    case kind go of
        (Physical {..}) -> circle 20 position bradius
        _               -> return ()

onDisplay :: IORef State -> IO ()
onDisplay rst = do
    st <- readIORef rst
    clearColor $= Color4 1.0 1.0 1.0 1.0
    clear [ColorBuffer]
    let gos = gameObjects st
    color3f 0 0 0
    renderPrimitive QuadStrip $ do
        let (w, h) = (0.5 *wKm, 0.5 * hKm)
        vertex $ vertex2f (-w) (-h)
        vertex $ vertex2f (-w) ( h)
        vertex $ vertex2f ( w) (-h)
        vertex $ vertex2f ( w) ( h)
    -- render objects
    mapM_ render $ filter (      isSmoke . kind) gos
    mapM_ render $ filter (not . isSmoke . kind) gos
    swapBuffers

onKeyboardMouse :: IORef State -> Key -> KeyState -> Modifiers -> Position -> IO ()
onKeyboardMouse _ (Char '\DC1') Down (Modifiers Up Down Up) _ = leaveMainLoop
onKeyboardMouse rst key state modifiers position = do
    st0 <- readIORef rst
    let vposition = physicalToVirtual st0 position
    st1 <- foldM (deviceInputs key state modifiers vposition) st0 (gameObjects st0)
    writeIORef rst st1

insertMeteors n st = do
    meteors <- mapM (\_ -> do   pos <- randomRIO ((-200.0) ^* ones3f, 200.0 ^* ones3f)
                                constructMeteor pos 15.0 25.0) [1..n]
    return $ foldr (addGameObject . makeMeteorObject) st meteors

startSpaceShipGame = do
    st0 <- makeState (vertex2f wKm hKm)
    st1 <- insertMeteors 5 st0
    ship <- constructShip
    rState <- newIORef $ addGameObject (makeShipObject ship) st1
    _ <- getArgsAndInitialize
    initialDisplayMode    $= [DoubleBuffered, RGBMode]
    _ <- createWindow "Space Ship!"
--    _ <- enterGameMode
    displayCallback       $=      (onDisplay         rState)
    keyboardMouseCallback $= Just (onKeyboardMouse   rState)
    reshapeCallback       $= Just (onReshape         rState)
    idleCallback          $= Just (onIdle            rState)
    mainLoop
--    leaveGameMode

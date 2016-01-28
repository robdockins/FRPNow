module Control.FRPNow.GLFW where

import Data.IORef
import qualified Data.Foldable as Fold
import Control.Exception
import Control.Monad
import Control.Concurrent.MVar
import Data.StateVar
import Data.Maybe
import qualified Data.Sequence as Seq

import Control.FRPNow.Core
import Control.FRPNow.Lib
import Control.FRPNow.EvStream

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL.GL as GL

type KeyEv   = (GLFW.Key, GLFW.KeyState, GLFW.ModifierKeys)

keyEvents :: GLFW.Window -> Now (EvStream KeyEv)
keyEvents win = do
   (kstr, keycb) <- callbackStream
   sync $ GLFW.setKeyCallback win (Just (\_ k _ st mod -> keycb (k,st,mod)))
   return kstr

type MouseEv = ( GLFW.MouseButton
               , GLFW.MouseButtonState
               , GLFW.ModifierKeys
               , Double
               , Double
               )

mouseButtonEvents :: GLFW.Window -> Now (EvStream MouseEv)
mouseButtonEvents win = do
    (mbstr, mbcb) <- callbackStream
    sync $ GLFW.setMouseButtonCallback win
             (Just (\_ mb btnst mod -> do
                  (x,y) <- GLFW.getCursorPos win
                  mbcb (mb,btnst,mod,x,y)))
    return mbstr

resizeEvents :: GLFW.Window -> Now (EvStream (Int,Int))
resizeEvents win = do
    (resizeStream, resizeCB) <- callbackStream
    sync $ GLFW.setWindowSizeCallback win (Just (\_ x y -> resizeCB (x,y)))
    return resizeStream

mousePos :: GLFW.Window -> Now (Behavior (Double,Double))
mousePos win = do
    curpos <- sync $ GLFW.getCursorPos win
    (str, cb) <- callbackStream
    sync $ GLFW.setCursorPosCallback win (Just $ \_ x y -> cb (x,y))
    sampleNow $ fromChanges curpos str


runGLFW :: IO (Maybe (GLFW.Window))
        -> (GLFW.Window -> Behavior Double
                        -> EvStream (Double, Double)
                        -> Now (Behavior (IO ())))
        -> IO ()
runGLFW mkWin doRender =
  GLFW.init >> bracket mkWin cleanup startup

 where
  cleanup Nothing = do
      GLFW.terminate
  cleanup (Just win) = do
      GLFW.destroyWindow win
      GLFW.terminate

  startup Nothing = fail "Failed to open main window"
  startup (Just win) = do
    GLFW.makeContextCurrent (Just win)
    scheduleRef <- newIORef Seq.empty
    timecbRef  <- newIORef (\_ -> return ())
    renderVar  <- newMVar Nothing
    initNow (schedule scheduleRef) (go win timecbRef renderVar)
    timecb <- readIORef timecbRef
    loop win renderVar scheduleRef timecb 0

  go win timecbRef renderVar = do
    (tstr, tcb)   <- callbackStream
    sync $ writeIORef timecbRef tcb
    clock <- sampleNow $ fromChanges 0 $ fmap fst tstr
    evs <- doRender win clock tstr
    let evs' = snapshots evs (fmap (const ()) tstr)
    callIOStream (pushRender renderVar) evs'
    return never

  pushRender v x = do
      _ <- swapMVar v (Just x)
      return ()

  schedule scheduleRef x =
    atomicModifyIORef scheduleRef (\s -> (s Seq.|> x, ()))

  loop win renderVar scheduleRef timecb t = do
    Just t' <- GLFW.getTime
    let dt = t' - t
    timecb (t', dt)
    rounds <- atomicModifyIORef scheduleRef (\s -> (Seq.empty, s))
    sequence_ (Fold.toList rounds)
    render <- swapMVar renderVar Nothing
    case render of
      Nothing -> return ()
      Just m -> do
          m
          GL.flush
          GLFW.swapBuffers win
    GLFW.pollEvents
    shouldClose <- GLFW.windowShouldClose win
    unless shouldClose (loop win renderVar scheduleRef timecb t')

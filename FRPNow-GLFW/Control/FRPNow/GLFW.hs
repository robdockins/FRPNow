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

mousePos :: GLFW.Window -> Now (Behavior (Double,Double))
mousePos win = do
    curpos <- sync $ GLFW.getCursorPos win
    (str, cb) <- callbackStream
    sync $ GLFW.setCursorPosCallback win (Just $ \_ x y -> cb (x,y))
    sampleNow $ fromChanges curpos str


runGLFW :: IO (Maybe (GLFW.Window))
        -> (GLFW.Window -> EvStream (Double,Double) -> Now (EvStream (IO ())))
        -> IO ()
runGLFW mkWin doRender = do
  GLFW.init
  bracket mkWin
     (maybe (return ()) GLFW.destroyWindow)
     (maybe (return ()) startup)
  GLFW.terminate

 where
  startup win = do
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
    evs <- doRender win tstr
    callIOStream (pushRender renderVar) evs
    return never

  pushRender v x = do
      _ <- swapMVar v (Just x)
      return ()

  schedule scheduleRef x =
    atomicModifyIORef scheduleRef (\s -> (s Seq.|> x, ()))

  loop win renderVar scheduleRef timecb t = do
    Just t' <- GLFW.getTime
    let dt = t' - t
    timecb (t',dt)
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

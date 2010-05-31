module Main where
import Graphics.X11.Xlib
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

main :: IO ()

main = do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
      border = blackPixel dpy dflt
      background = blackPixel dpy dflt
  rootw <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 100 100 1 border background
  setTextProperty dpy win "Hello X11" wM_NAME
  mapWindow dpy win
  sync dpy False
  threadDelay (10 * 1000000)
  exitWith ExitSuccess
{-# LANGUAGE ForeignFunctionInterface #-}

module XMonad.Util.Cairo
    ( mkXlibSurface
    , mkXlibSurface'
    , xlibSurfaceSetSize
    , xlibSurfaceGetWidth
    , xlibSurfaceGetHeight
    , clipExtents)
    where

import Control.Monad.Reader (ask)

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types
import Graphics.X11.Xlib (Display, Drawable, Visual,
                          getGeometry, defaultVisualOfScreen, defaultScreenOfDisplay)

import Foreign.C
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr

-- XXX Put into XMonad.Util.Cairo.Xlib

foreign import ccall unsafe cairo_xlib_surface_create ::
    Display -> Drawable -> Visual -> CInt -> CInt -> IO (Ptr Surface)

mkXlibSurface :: Display -> Drawable -> Visual -> Int -> Int -> IO Surface
mkXlibSurface disp d v w h = do
  sfPtr <- cairo_xlib_surface_create disp d v (fromIntegral w) (fromIntegral h)
  sf <- mkSurface sfPtr
  manageSurface sf
  return sf

-- Create a Cairo Xlib surface for the given drawable, using the
-- display's default visual and the drawable's current dimensions.
-- Unlike 'mkXlibSurface', this requires a round-trip to the X server
-- to query the drawable's dimensions.
mkXlibSurface' :: Display -> Drawable -> IO Surface
mkXlibSurface' disp d = do
  let visual = defaultVisualOfScreen (defaultScreenOfDisplay disp)
  (_, _, _, w, h, _, _) <- getGeometry disp d
  mkXlibSurface disp d visual (fromIntegral w) (fromIntegral h)

foreign import ccall unsafe cairo_xlib_surface_set_size ::
    Ptr Surface -> CInt -> CInt -> IO ()

xlibSurfaceSetSize :: Surface -> Int -> Int -> IO ()
xlibSurfaceSetSize surf w h = do
  withSurface surf (\sp -> cairo_xlib_surface_set_size sp (fromIntegral w) (fromIntegral h))

foreign import ccall unsafe cairo_xlib_surface_get_width ::
    Ptr Surface -> IO CInt

xlibSurfaceGetWidth :: Surface -> IO Int
xlibSurfaceGetWidth surf = do
  w <- withSurface surf cairo_xlib_surface_get_width
  return $ fromIntegral w

foreign import ccall unsafe cairo_xlib_surface_get_height ::
    Ptr Surface -> IO CInt

xlibSurfaceGetHeight :: Surface -> IO Int
xlibSurfaceGetHeight surf = do
  w <- withSurface surf cairo_xlib_surface_get_height
  return $ fromIntegral w

-- XXX Put into XMonad.Util.Cairo.Missing or something (or real Cairo)

foreign import ccall unsafe cairo_clip_extents ::
    Ptr Cairo -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ()

liftRender :: (Cairo -> IO a) -> Render a
liftRender f = ask >>= \context -> liftIO (f context)

-- Version 1.4
clipExtents :: Render (Double, Double, Double, Double)
clipExtents = do
  liftRender $ \cairo ->
    let pc = unCairo cairo in
    alloca $ \px1 ->
    alloca $ \py1 ->
    alloca $ \px2 ->
    alloca $ \py2 -> do
      cairo_clip_extents pc px1 py1 px2 py2
      x1 <- peekFloatConv px1
      y1 <- peekFloatConv py1
      x2 <- peekFloatConv px2
      y2 <- peekFloatConv py2
      return (x1, y1, x2, y2)

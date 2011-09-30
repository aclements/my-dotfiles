{-# LANGUAGE PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.CairoTest where

import Control.Monad (forM_)
import System.CPUTime
import System.Time

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.XUtils
import XMonad.Util.Cairo

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types
import Graphics.X11.Xlib (Rectangle(..))

data CairoTest a = CairoTest (Maybe Window) String
                 deriving (Show, Read)

instance LayoutModifier CairoTest a where
    -- Probably better in hook, but need an MVar or something
    redoLayout (CairoTest Nothing _) _ _ wrs = setup wrs
    redoLayout _ _ _ wrs = return (wrs, Nothing)

    emptyLayoutMod (CairoTest Nothing _) _ wrs = setup wrs
    emptyLayoutMod _ _ wrs = return (wrs, Nothing)

    handleMess (CairoTest (Just win) s) m
        | Just Hide <- fromMessage m = cleanup win s
        | Just ReleaseResources <- fromMessage m = cleanup win s
        | Just e <- fromMessage m =
                    do trace (show e)
                       str <- doEvent win e
                       return (Just (CairoTest (Just win) str))
        | otherwise = return Nothing

    handleMess _ _ = return Nothing

    modifierDescription ct = show ct

setup wrs = do
  win <- createNewWindow (Rectangle 40 40 400 400) (Just (buttonPressMask .|. exposureMask)) "black" True
  showWindow win
  return (wrs, Just (CairoTest (Just win) ""))

cleanup win s = do
  deleteWindow win
  return (Just (CairoTest Nothing s))

kernel7 = [[0.00000067, 0.00002292, 0.00019117, 0.00038771, 0.00019117, 0.00002292, 0.00000067],
           [0.00002292, 0.00078633, 0.00655965, 0.01330373, 0.00655965, 0.00078633, 0.00002292],
           [0.00019117, 0.00655965, 0.05472157, 0.11098164, 0.05472157, 0.00655965, 0.00019117],
           [0.00038771, 0.01330373, 0.11098164, 0.22508352, 0.11098164, 0.01330373, 0.00038771],
           [0.00019117, 0.00655965, 0.05472157, 0.11098164, 0.05472157, 0.00655965, 0.00019117],
           [0.00002292, 0.00078633, 0.00655965, 0.01330373, 0.00655965, 0.00078633, 0.00002292],
           [0.00000067, 0.00002292, 0.00019117, 0.00038771, 0.00019117, 0.00002292, 0.00000067]]

kernel13 = [[0.0, 0.0, 0.0, 0.0, 0.00026921323181439193, 0.00039170294099155986, 0.00044385758164631242, 0.00039170294099155986, 0.00026921323181439193, 0.0, 0.0, 0.0, 0.0], [0.0, 0.0, 0.00023757984321098953, 0.00056992441622341953, 0.0010647589866513228, 0.0015492151842152507, 0.0017554907894611501, 0.0015492151842152507, 0.0010647589866513228, 0.00056992441622341953, 0.00023757984321098953, 0.0, 0.0], [0.0, 0.00023757984321098953, 0.00073179743602179404, 0.0017554907894611501, 0.0032796885707202942, 0.0047719187130194672, 0.005407292307818651, 0.0047719187130194672, 0.0032796885707202942, 0.0017554907894611501, 0.00073179743602179404, 0.00023757984321098953, 0.0], [0.0, 0.00056992441622341953, 0.0017554907894611501, 0.0042112034836251488, 0.0078675638842071755, 0.01144723790548167, 0.01297141992448449, 0.01144723790548167, 0.0078675638842071755, 0.0042112034836251488, 0.0017554907894611501, 0.00056992441622341953, 0.0], [0.00026921323181439193, 0.0010647589866513228, 0.0032796885707202942, 0.0078675638842071755, 0.014698544421509811, 0.021386255940681029, 0.024233802836073935, 0.021386255940681029, 0.014698544421509811, 0.0078675638842071755, 0.0032796885707202942, 0.0010647589866513228, 0.00026921323181439193], [0.00039170294099155986, 0.0015492151842152507, 0.0047719187130194672, 0.01144723790548167, 0.021386255940681029, 0.031116818784518406, 0.035259975070037794, 0.031116818784518406, 0.021386255940681029, 0.01144723790548167, 0.0047719187130194672, 0.0015492151842152507, 0.00039170294099155986], [0.00044385758164631242, 0.0017554907894611501, 0.005407292307818651, 0.01297141992448449, 0.024233802836073935, 0.035259975070037794, 0.039954786205788184, 0.035259975070037794, 0.024233802836073935, 0.01297141992448449, 0.005407292307818651, 0.0017554907894611501, 0.00044385758164631242], [0.00039170294099155986, 0.0015492151842152507, 0.0047719187130194672, 0.01144723790548167, 0.021386255940681029, 0.031116818784518406, 0.035259975070037794, 0.031116818784518406, 0.021386255940681029, 0.01144723790548167, 0.0047719187130194672, 0.0015492151842152507, 0.00039170294099155986], [0.00026921323181439193, 0.0010647589866513228, 0.0032796885707202942, 0.0078675638842071755, 0.014698544421509811, 0.021386255940681029, 0.024233802836073935, 0.021386255940681029, 0.014698544421509811, 0.0078675638842071755, 0.0032796885707202942, 0.0010647589866513228, 0.00026921323181439193], [0.0, 0.00056992441622341953, 0.0017554907894611501, 0.0042112034836251488, 0.0078675638842071755, 0.01144723790548167, 0.01297141992448449, 0.01144723790548167, 0.0078675638842071755, 0.0042112034836251488, 0.0017554907894611501, 0.00056992441622341953, 0.0], [0.0, 0.00023757984321098953, 0.00073179743602179404, 0.0017554907894611501, 0.0032796885707202942, 0.0047719187130194672, 0.005407292307818651, 0.0047719187130194672, 0.0032796885707202942, 0.0017554907894611501, 0.00073179743602179404, 0.00023757984321098953, 0.0], [0.0, 0.0, 0.00023757984321098953, 0.00056992441622341953, 0.0010647589866513228, 0.0015492151842152507, 0.0017554907894611501, 0.0015492151842152507, 0.0010647589866513228, 0.00056992441622341953, 0.00023757984321098953, 0.0, 0.0], [0.0, 0.0, 0.0, 0.0, 0.00026921323181439193, 0.00039170294099155986, 0.00044385758164631242, 0.00039170294099155986, 0.00026921323181439193, 0.0, 0.0, 0.0, 0.0]]


blurSlow source x y = do
  save
  setSourceRGB 0 0 0
  paint
  setOperator OperatorAdd
  let kernel = kernel13
  let start = -(fromIntegral (length kernel) - 1) / 2
  forM_ (zip kernel [start ..]) $ \(row, dy) -> do
                forM_ (zip row [start ..]) $ \(v, dx) -> do
                                          setSourceSurface source (x+dx) (y+dy)
                                          paintWithAlpha v
  restore

kernelFast13 = [0.0022181958546457652, 0.0087731347915883835, 0.02702315760287952, 0.064825185138526822, 0.12110939007484811, 0.17621312278855081, 0.19967562749792106, 0.17621312278855081, 0.12110939007484811, 0.064825185138526822, 0.02702315760287952, 0.0087731347915883835, 0.0022181958546457652]

blur :: Surface -> Double -> Double -> Render ()
blur source x y = do
  let kernel = kernelFast13
      start = -((length kernel) - 1) `div` 2
      startD = fromIntegral start
  (x1,y1,x2,y2) <- clipExtents
  let offx = floor x1
      offy = floor y1
      offxD = fromIntegral offx
      offyD = fromIntegral offy
  save
  renderWithSimilarSurface ContentColor ((ceiling x2)-offx-2*start) ((ceiling y2)-offy) $ \tmp -> do
    renderWith tmp $ do
      setOperator OperatorAdd
      forM_ (zip kernel [startD ..]) $ \(v, dy) -> do
        setSourceSurface source (x-startD-offxD) (y+dy-offyD)
        paintWithAlpha v
    setSourceRGB 0 0 0
    paint
    setOperator OperatorAdd
    forM_ (zip kernel [2*startD ..]) $ \(v, dx) -> do
      setSourceSurface tmp (dx+offxD) offyD
      paintWithAlpha v
  restore

timeCPU :: MonadIO m => m a -> m a
timeCPU a = do
  start <- io getCPUTime
  v <- a
  end   <- io getCPUTime
  let diff :: Double
      diff = (fromIntegral (end - start)) / (1e12)
  trace (show diff ++ " seconds")
  return v

time :: MonadIO m => m a -> m a
time a = do
  start <- io getClockTime
  v <- a
  end   <- io getClockTime
  let diff = diffClockTimes end start
      d :: Double
      d = (fromIntegral (tdSec diff)) + ((fromIntegral (tdPicosec diff)) / 1e12)
  trace (show d ++ " seconds")
  return v

doEvent :: Window -> Event -> X String
doEvent w (ButtonEvent {ev_x = x, ev_y = y, ev_window = bw})
    | w == bw = do
  trace (show x ++ " " ++ show y)
  return (show x ++ " " ++ show y)
doEvent w (ExposeEvent {ev_window = bw})
    | w == bw = do
  d <- asks display
  -- gc <- io $ createGC d w
  -- b_color' <- stringToPixel d "black"
  -- io $ setForeground d gc b_color'
  -- io $ drawLine d w gc 0 0 40 50
  -- io $ freeGC        d gc
  -- let visual = defaultVisualOfScreen (defaultScreenOfDisplay d)
  -- sfPtr <- io $ cairo_xlib_surface_create d w visual 100 100
  -- sf <- io $ mkSurface sfPtr
  -- io $ manageSurface sf
  sf <- io $ mkXlibSurface' d w

  -- Xephyr, copy to image, draw to image: 1.933 sec
  -- Xephyr, copy to xlib, draw to image: 9.586 sec
  -- Xephyr, copy to xlib, draw to xlib: 1.357 sec
  -- Xephyr, direct, draw to xlib: 1.349 sec
  -- Xephyr, direct, draw direct: 2.437 sec
  -- Xephyr, copy to xlib, draw direct: 2.320 sec
  -- X, copy to image, draw to image: 1.977 sec
  -- X, copy to xlib, draw to xlib: 1.621 sec
  -- X, direct, draw to xlib: 1.696 sec
  -- X, direct, draw direct: 55 sec
  -- X, copy to xlib, draw direct: 48.6 sec

  -- Drawing to the screen is extremely slow.  Drawing to an image is
  -- slightly slower than drawing to an X pixmap.  Sourcing from a big
  -- pixmap is about the same as sourcing from a small pixmap.

  a <- io $ internAtom d "_XROOTPMAP_ID" False
  -- XXX Check all parents (second return value)
  (r, _, _) <- io $ queryTree d w
  rootpmap' <- io $ getWindowProperty32 d a r
  io $ sync d False
  case rootpmap' of
    Just [rootpmap] -> time $
        do rootSf <- io $ mkXlibSurface' d (fromIntegral rootpmap)
           let offx = -40
               offy = -40

           -- let realRootSf = rootSf
           -- io $ withSimilarSurface rootSf ContentColor 400 400 $ \rootSf -> do
           -- io $ withImageSurface FormatRGB24 400 400 $ \rootSf -> do
           -- renderWith rootSf (setSourceSurface realRootSf offx offy >> paint)
           -- let offx = 0
           --     offy = 0

           let realSf = sf
           io $ withSimilarSurface sf ContentColor 400 400 $ \sf -> do
           -- io $ withImageSurface FormatRGB24 400 400 $ \sf -> do
           renderWith sf $ do
                   save
                   setSourceSurface rootSf offx offy
                   paint
                   restore
                   save
                   arc 200.5 200.5 100 0 (2*pi)
                   clip
                   ce <- clipExtents
                   io $ trace (show ce)
                   blur rootSf offx offy
                   restore
           -- time $ do forM_ [1..30] $ const $ renderWith sf $ blur rootSf offx offy
           --           surfaceFlush sf
           --           io $ sync d False
           renderWith realSf $ do
                   setSourceSurface sf 0 0
                   paint
           io $ sync d False
           -- renderWith sf $ do
           --         save
           --         setOperator OperatorAdd
           --         -- XXX Figure out offset
           --         withRGBAPattern 0 1 0 0.5 $ \pat -> do
           --                   setSourceSurface rootSf (-40) (-40)
           --                   --mask pat
           --                   paintWithAlpha 0.5
           --                   --rectangle 0 0 400 400
           --                   --fillPreserve
           --                   setSourceSurface rootSf (-50) (-50)
           --                   --setSourceRGB 1 0 0
           --                   --fill
           --                   --mask pat
           --                   paintWithAlpha 0.5
           --         restore
    _ -> return ()
  -- renderWith sf $ do
  --   let w = 400
  --       h = 400
  --   -- setSourceRGB 0.2 0.3 0.8
  --   -- setAntialias AntialiasSubpixel
  --   -- newPath
  --   -- arc 40 50 20 0.4 (3.1415 - 0.4)
  --   -- stroke
  --   -- arc 30 35 3 0 (2*3.1415)
  --   -- fill
  --   -- arc 50 35 3 0 (2*3.1415)
  --   -- fill
  --   -- arc 40 50 30 0 (2*pi)
  --   -- stroke

  --   setLineWidth 2

  --   save
  --   arc (w/3) (w/4) (w/4) (-pi / 5) pi
  --   closePath
  --   setSourceRGBA 0 0.8 0 0.5
  --   fillPreserve
  --   restore
  --   stroke

  --   save
  --   arc (w/2) (w/2) (w/4) 0 (2*pi)
  --   setSourceRGBA 0 0 0.8 0.6
  --   fillPreserve
  --   restore
  --   stroke

  --   save
  --   translate (w/2) (3*h/4)
  --   scale (3*w/8) (h/6)
  --   arc 0 0 1 0 (2*pi)
  --   setSourceRGBA 0.8 0 0 0.7
  --   fillPreserve
  --   restore
  --   stroke

  --   selectFontFace "Arial" FontSlantNormal FontWeightNormal
  --   setFontSize 32
  --   moveTo 20 20
  --   showText "Hi!"
  --   fill

  return ""
doEvent _ _ = return ""

cairoTest :: l a -> ModifiedLayout CairoTest l a
cairoTest = ModifiedLayout (CairoTest Nothing "")

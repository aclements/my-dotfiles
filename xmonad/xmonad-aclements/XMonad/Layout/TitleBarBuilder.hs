{-# LANGUAGE PatternGuards, ViewPatterns, MultiParamTypeClasses, FunctionalDependencies #-}

-- Shading
-- Action bindings and buttons
-- Window icons
-- Title truncation
-- Compound text decoding? (emacs22 titles) http://euc.jp/i18n/ctext.txt

module XMonad.Layout.TitleBarBuilder where

import Control.Monad (foldM, liftM, when, zipWithM, mplus, forM_)
import Control.Monad.Reader (asks)
import Data.Function (on)
import Data.IORef
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

import XMonad.Core
import XMonad.Util.Cairo
import XMonad.Util.Invisible
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.XUtils

import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Types
import Graphics.X11.Xlib hiding (textExtents)
import Graphics.X11.Xlib.Extras


type BarWindow = Window

data Bar
    = Bar {
        bBarWin   :: BarWindow
      , bBarRect  :: Rectangle
--      , bSurface  :: Surface
      , bWin      :: Window
      , bSpec     :: TitleSpec
--      , bTitle    :: IORef (Maybe String)
--      , bState    :: bs
      }
      deriving (Show, Eq)

data TBBState = TBBS (Map.Map Window Bar) (Map.Map BarWindow Bar)

class TitleBarStyle s where
    styleInit :: s -> X s
    styleInit = return
    styleUpdate :: s -> [(Bar, Bar)] -> [Bar] -> X s
    styleUpdate s _ _ = return s
    -- styleUpdate :: ls -> Bar bs -> Rectangle -> Window -> TitleSpec -> X bs
    -- styleUpdate _ bar _ _ _ = return $ bState bar
    styleHeight :: s -> TitleSpec -> Int
    styleHeight _ _ = 12
    -- styleCreateBar :: ls -> BarWindow -> Window -> X bs
    -- styleDestroyBar :: ls -> Bar bs -> X ()
    -- styleDestroyBar _ _ = return ()
    styleDrawBar :: s -> Bar -> X ()
    styleDrawBar _ _ = return ()
    -- XXX Provide general event handler, plus a way to map
    -- windows/bar windows to Bar's?  Or perhaps a general event
    -- handler that just gets handed the bar if possible so the lookup
    -- can be done once (or zero times if not forced), instead of
    -- happening in each style tool.
--    stylePropChange :: ls -> Bar bs -> X ()
    styleHandleMessage :: s -> SomeMessage -> Maybe Bar -> X (Maybe s)
    styleHandleMessage _ _ _ = return Nothing

-- XXX Add a TitleBarStyle class
-- XXX Dispense with Invisible and just write Read/Show for TBBState

-- XXX It's really, really annoying that the bar state type variable
-- is exposed here because it propagates up to the layout and requires
-- the layout to be parameterized over bs.
data TitleBarBuilder s
    = TitleBarBuilder s (Invisible Maybe TBBState)
    deriving (Show, Read)

defTitleBarBuilder :: TitleBarBuilder (NeonStyle Arial12)
defTitleBarBuilder = TitleBarBuilder neonStyle (I Nothing)

data WindowStyle
    = WindowStyle {
        wsActive :: Bool
      , wsSecondary :: Bool
      , wsUrgent :: Bool
      , wsShaded :: Bool
      }
    deriving (Show, Read, Eq)

defWindowStyle :: WindowStyle
defWindowStyle = WindowStyle False False False False

data TitleSpec
    = TitleSpec WindowStyle
--    | TitleSpecStack [(Window, WindowStyle)] WindowStyle
-- XXX Shaded at bottom
-- XXX Ugh.  Per-bar and per-window are no longer the same
--    | TitleSpecTabs [(Window, WindowStyle)] WindowStyle [(Window, WindowStyle)]
    deriving (Show, Read, Eq)

--data TitleSpec = TitleStack [(Window, WindowStyle)] [TitleRow]

--data TitleRow   = TitleRow 

--
-- Rectangles
--

titleBarHeight :: TitleBarStyle s => TitleBarBuilder s -> TitleSpec -> Int
titleBarHeight (TitleBarBuilder style _) spec = styleHeight style spec

titleBarRect :: TitleBarStyle s => TitleBarBuilder s -> Rectangle -> TitleSpec -> Rectangle
titleBarRect tbb (Rectangle x y w _) tb =
    let tbh = titleBarHeight tbb tb
    in Rectangle x (y - fromIntegral tbh) w (fromIntegral tbh)

shrinkRect :: Int -> Rectangle -> Rectangle
shrinkRect by (Rectangle x y w h)
    | fromIntegral by > h = Rectangle x (y + fromIntegral h) w 0
    | otherwise = Rectangle x (y + fromIntegral by) w (h - fromIntegral by)

--
-- Layout handling
--

titleBarLayout :: TitleBarStyle s =>
                  TitleBarBuilder s -> [(Window, Rectangle, TitleSpec)]
               -> X ([(Window, Rectangle)], Maybe (TitleBarBuilder s))
titleBarLayout tbb@(TitleBarBuilder style st) rs =
    case st of
      I Nothing  -> do
              style' <- styleInit style
              let tbb' = TitleBarBuilder style' st
              update tbb' (TBBS Map.empty Map.empty) rs
      I (Just st') -> update tbb st' rs

-- All new bars either need their surfaces resized or I need to
-- compute the rectangle when I create the bar and force the surface
-- size.
--
-- All reused bars need their surfaces resized if the size is wrong
-- and always need to be redrawn (their state has always changed, if
-- you include the underlying window as part of the state).
--
-- All kept bars need their surfaces resized if the size is wrong and
-- need to be redrawn if they are resized or their state changes.

-- Most of this is pure, it would be nice if I could extract that out.
-- It's also too bad we can't easily pass things to styleUpdate sorted
-- by BarWindow.
update :: TitleBarStyle s =>
          TitleBarBuilder s
       -> TBBState
       -> [(Window, Rectangle, TitleSpec)]
       -> X ([(Window, Rectangle)], Maybe (TitleBarBuilder s))
update tbb@(TitleBarBuilder style _) (TBBS curBars _) rs = do
  let bars' :: Map.Map Window (Rectangle, TitleSpec)
      bars' = Map.fromList [(w, (r, tb)) | (w, r, tb) <- rs]
      -- Existing title bars for windows that no longer exist
      staleBars = Map.difference curBars bars'
      -- Existing title bars for windows that still exist
      keepBars = Map.difference curBars staleBars
      -- Windows without existing title bars
      newWins = Map.difference bars' curBars
  -- Create new title bars and destroy stale title bars
  (new, unused) <- createAndDestroyBars (Map.toAscList newWins) (Map.elems staleBars)
  -- Update bars
  let allCurBars = Map.union keepBars (Map.fromAscList new)
  newBars <- zipWithM (updateBar tbb) (Map.assocs allCurBars) (Map.assocs bars')
  style' <- styleUpdate style [(oldBar, bar) | (_, _, oldBar, bar) <- newBars] unused
  --allNewBars <- liftM Map.fromAscList $ zipWithM updateBar (Map.assocs allCurBars) (Map.assocs bars')
  -- Destroy unused bars
  mapM_ destroyBar unused
  -- Construct layout list
  let rs' = foldr getRects [] newBars
      getRects (win, winRect, _, bar) rest =
          (bBarWin bar, bBarRect bar):
          case bSpec bar of
            TitleSpec ws | wsShaded ws -> rest
            --TitleSpecStack _ ws | wsShaded ws -> rest
            _ -> (win, winRect):rest
  -- Construct new state
  let winMap = Map.fromAscList [(win, bar) | (win, _, _, bar) <- newBars]
      barMap = Map.fromList [(bBarWin bar, bar) | (_, _, _, bar) <- newBars]
      state' = TitleBarBuilder style' (I (Just (TBBS winMap barMap)))

  -- -- Compute new title bar rectangles
  -- let rects :: [(Window, Rectangle, Bar, TitleSpec, Rectangle)]
  --     rects = map getRect rs
  --     getRect (win, rect, spec) =
  --         let bar = allBars Map.! win
  --         in (win, rect, bar, spec, titleBarRect tbb rect spec)
  -- -- Resize Cairo surfaces (XXX Redraw?)
  -- forM_ rects $ \(_, _, bar, _, (Rectangle _ _ w' h')) -> do
  --   w <- xlibSurfaceGetWidth (bSurface bar)
  --   h <- xlibSurfaceGetHeight (bSurface bar)
  --   when (w /= fromIntegral w' || h /= fromIntegral h') $
  --        xlibSurfaceSetSize (bSurface bar) w' h'
  -- XXX Redraw title bars whose state has changed (after move!)
  -- XXX Update bar state
  -- Map.fromAscList zipWithM XXX (Map.toAscList bars') (Map.toAscList allBars)
  -- Or Map.unionWith if I don't need X
  -- Construct layout list
  -- let rs' = foldr layoutRects [] rects
  --     layoutRects (win, rect, bar, _, barRect) rest =
  --         (bBarWin bar, barRect):(win, rect):rest
  -- let rs' = foldr getRects [] rs
  --     -- XXX It's weird that we ignore the TitleSpec from rs here.
  --     -- Perhaps I don't need to keep it around as much as I do?  Or
  --     -- perhaps I need both to figure out which I need to redraw.
  --     getRects (win, rect, _) rest =
  --         let bar = allBars Map.! win
  --         in (((bBarWin bar), titleBarRect state' rect (bSpec bar)):(win, rect):rest)
  -- Return everything
  -- let rmap = Map.fromList [(bBarWin bar, bar) | bar <- Map.elems allBars]
  --     state' = TitleBarBuilder (I (Just (TBBS allBars rmap)))
--  trace $ "Before: " ++ show rs
--  trace $ "After: " ++ show rs'
  return $ (rs', Just state')

-- | The returned new bars will be sorted in the same order as the new
-- windows and the unused bars will be sorted in the same order as the
-- spare bars.
createAndDestroyBars :: [(Window, (Rectangle, TitleSpec))]  -- ^ New windows
                     -> [Bar]                               -- ^ Spare title bars
                     -> X ([(Window, Bar)], [Bar])          -- ^ New bars and unused bars
createAndDestroyBars newWins spareBars = do
  (newBars, extraBars) <- foldM create ([], spareBars) newWins
--  mapM_ (destroyBar ls) extraBars
  return (reverse newBars, extraBars)
    where
      create (rest, []) (win, (_, spec)) = do
        -- Create a new bar off screen so it will be moved into place
        -- and painted along with everything else.
        let rect = Rectangle (-1) (-1) 1 1
        --tbWin <- createNewWindow rect (Just exposureMask) "black" True
        tbWin <- createBarWindow rect
        showWindow tbWin
        -- Create bar
--        bs <- styleCreateBar ls tbWin win
        -- -- Create a Cairo surface
        -- d <- asks display
        -- let visual = defaultVisualOfScreen (defaultScreenOfDisplay d)
        -- surf <- io $ mkXlibSurface d tbWin visual 1 1
        -- -- Create window title cache
        -- title <- io $ newIORef Nothing
        -- Return new bar
        -- return ((win, Bar tbWin surf win spec title):rest, [])
        return ((win, Bar tbWin rect win spec):rest, [])
      create (rest, (spare:spares)) (win, (_, spec)) = do
        -- Recycle the existing bar
        -- XXX Redraw spare
        -- XXX Don't update bar, just associate it with a new window?
        -- Clear title cache (XXX could be done in updateBar)
--        io $ writeIORef (bTitle spare) Nothing
        return ((win, spare {bWin=win, bSpec=spec}):rest, spares)

-- | Create a title bar window at the specified location.  The title
-- bar window will be set as override redirect and will have no
-- background.  Only the exposure mask will selected for input.
createBarWindow :: Rectangle -> X Window
createBarWindow (Rectangle x y w h) = do
  d <- asks display
  rw <- asks theRoot
  let s = defaultScreenOfDisplay d
      visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  win <- io $ allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes True
    createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                 inputOutput visual attrmask attributes
  io $ selectInput d win exposureMask
  return win

destroyBar :: Bar -> X ()
destroyBar bar = do
--  styleDestroyBar ls bar
  deleteWindow (bBarWin bar)

-- XXX This function is now pure (and rather uninteresting)
updateBar :: TitleBarStyle s =>
             TitleBarBuilder s
          -> (Window, Bar)
          -> (Window, (Rectangle, TitleSpec))
          -> X (Window, Rectangle, Bar, Bar)
updateBar tbb (oldWin, oldBar) (win, (winRect, spec)) = do
--  let Bar {bBarWin = barWin, bSurface = surface, bTitle = title} = oldBar
--  let Bar {bBarWin = barWin} = oldBar
  -- Sanity check
  when (oldWin /= win) (error "BUG: TitleBarBuilder window maps misaligned")
  -- Compute the new title bar rectangle
  let barRect = titleBarRect tbb winRect spec
--      bar = Bar barWin win spec XXX
--  bs <- styleUpdate ls oldBar barRect win spec
  let bar = Bar (bBarWin oldBar) barRect win spec
  -- ACTION: Update bar (possible state update, possible redraw)
  -- -- Resize the Cairo surface
  -- oldW <- io $ xlibSurfaceGetWidth  surface
  -- oldH <- io $ xlibSurfaceGetHeight surface
  -- let Rectangle _ _ (fromIntegral -> w) (fromIntegral -> h) = barRect
  --     resized = (oldW /= w || oldH /= h)
  -- when resized $ io $ xlibSurfaceSetSize surface w h
  -- -- XXX Redraw bars (after move!)
  -- let needRedraw = resized || bWin oldBar /= win || bSpec oldBar /= spec
  -- when needRedraw $ trace ("Redraw " ++ show barWin)
  -- Return updated bar
  return (win, winRect, oldBar, bar)

--
-- Bar info
--

-- barGetTitle :: Bar -> X String
-- barGetTitle bar = do
--   cached <- io $ readIORef (bTitle bar)
--   case cached of
--     Just t  -> return t
--     Nothing -> do
--       t <- liftM show $ getName (bWin bar)
--       io $ writeIORef (bTitle bar) $ Just t
--       return t

--
-- Message handling
--

titleBarHandleMessage :: TitleBarStyle s =>
                         TitleBarBuilder s -> SomeMessage
                      -> X (Maybe (TitleBarBuilder s))
titleBarHandleMessage tbb@(TitleBarBuilder style st) m
    | Just e                <- fromMessage m =
        handleEvent tbb e
    | otherwise = do
        style' <- styleHandleMessage style m Nothing
        let isDestroy
                -- XXX Might want to keep windows around for Hide
              | Just Hide             <- fromMessage m = True
              | Just ReleaseResources <- fromMessage m = True
              | otherwise = False
            (tbb', tbb'') = case style' of
                              Nothing -> (tbb, Nothing)
                              Just s  -> (TitleBarBuilder s st, Just tbb')
        if isDestroy
           then destroy tbb'
           else return tbb''

destroy :: TitleBarStyle s =>
           TitleBarBuilder s -> X (Maybe (TitleBarBuilder s))
destroy (TitleBarBuilder style st) =
  case st of
    I Nothing -> return Nothing
    I (Just (TBBS wins _)) -> do
      style' <- styleUpdate style [] (Map.elems wins)
      mapM_ destroyBar (Map.elems wins)
      return $ Just $ TitleBarBuilder style' (I (Just (TBBS Map.empty Map.empty)))

-- drawBar :: Bar -> X ()
-- drawBar bar = do
--   name <- barGetTitle bar
--   renderWith (bSurface bar) $ do
--     save
--     let TitleSpec (WindowStyle active _ _) = bSpec bar
--     if active
--        then setSourceRGB 0.5 0.5 1
--        else setSourceRGB 0.5 0.5 0.5
--     paint
--     setSourceRGB 0 0 0
--     selectFontFace "Arial" FontSlantNormal FontWeightNormal
--     setFontSize 12
--     moveTo 5 12
--     showText (show name)
--     restore
--   return ()

windowFromEvent :: Event -> Maybe Window
windowFromEvent e =
    case e of
      AnyEvent {}              -> Just $ ev_window e
      ConfigureRequestEvent {} -> Just $ ev_window e
      ConfigureEvent {}        -> Just $ ev_window e
      MapRequestEvent {}       -> Just $ ev_window e
      KeyEvent {}              -> Just $ ev_window e
      ButtonEvent {}           -> Just $ ev_window e
      MotionEvent {}           -> Just $ ev_window e
      DestroyWindowEvent {}    -> Just $ ev_window e
      UnmapEvent {}            -> Just $ ev_window e
      MapNotifyEvent {}        -> Just $ ev_window e
      MappingNotifyEvent {}    -> Just $ ev_window e
      CrossingEvent {}         -> Just $ ev_window e
      PropertyEvent {}         -> Just $ ev_window e
      ExposeEvent {}           -> Just $ ev_window e
      ClientMessageEvent {}    -> Just $ ev_window e
      _                        -> Nothing

handleEvent :: TitleBarStyle s =>
               TitleBarBuilder s -> Event -> X (Maybe (TitleBarBuilder s))
handleEvent (TitleBarBuilder style st) (ExposeEvent {ev_window = w}) =
    case st of
      I Nothing -> return Nothing
      I (Just (TBBS _ barMap))
        -- Look up the exposed window in the reverse map
        | Just bar <- Map.lookup w barMap -> do
            trace $ "Expose " ++ show (bBarWin bar)
            -- Redraw the title bar
            styleDrawBar style bar
            -- XXX Just the exposed region
            -- XXX Only if ev_count > 1?
            --drawBar bar
            return Nothing
        | otherwise -> return Nothing
-- handleEvent tbb@(TitleBarBuilder st) (PropertyEvent {ev_window = w, ev_atom = a}) = do
--   case st of
--     I Nothing -> return Nothing
--     I (Just (TBBS winMap _))
--       | Just bar <- Map.lookup w winMap -> do
--           -- Unfortunately, xmonad's main event handler eats WM_NAME
--           -- property changes.  Luckily, every client I've ever seen
--           -- also changes at least WM_ICON_NAME, so we'll still
--           -- probably get some notification.  Just in case, always
--           -- wipe out the title cache.
--           -- ACTION: Property change on bar
--           trace $ "Property change " ++ show (bBarWin bar) ++ " " ++ show a
--           io $ writeIORef (bTitle bar) Nothing
--           -- XXX Only redraw if something it actually cares about
--           -- changed.  Perhaps if a property that has been accessed is
--           -- changed?

--           -- XXX Don't redraw on WM_STATE change.
--           -- XMonad.Operations.windows causes this to change for every
--           -- single window on every single layout operation.
--           drawBar bar
--           return Nothing
--       | otherwise -> return Nothing
-- handleEvent _ _ = return Nothing
handleEvent (TitleBarBuilder style st) event = do
  case st of
    I Nothing -> return Nothing
    I (Just (TBBS winMap barMap)) -> do
      let bar = do w <- windowFromEvent event
                   Map.lookup w winMap `mplus` Map.lookup w barMap
      style' <- styleHandleMessage style (SomeMessage event) bar
      case style' of
        Nothing -> return Nothing
        Just s  -> return $ Just $ TitleBarBuilder s st

--
-- Theme
--

class TitleXFTFontTheme t where
    getTitleXFTFont :: t -> WindowStyle -> (String, FontSlant, FontWeight, Double)
    xftFontValue :: t

data Arial12 = Arial12

instance Read Arial12 where
    readsPrec _ s = [(Arial12, s)]

instance Show Arial12 where
    showsPrec _ _ = id

instance TitleXFTFontTheme Arial12 where
    getTitleXFTFont _ _ = ("Arial", FontSlantNormal, FontWeightNormal, 12)
    xftFontValue = Arial12

--
-- Per-bar state
--

data PerBar st
    = PerBar [(BarWindow, st)]
    deriving (Show)

perBarEmpty :: PerBar st
perBarEmpty = PerBar []

-- XXX Could be any Monad
perBarUpdate :: PerBar st                  -- ^ PerBar state
             -> [(Bar, Bar)]               -- ^ Updated bars
             -> [Bar]                      -- ^ Deleted bars
             -> (Bar -> X st)              -- ^ New function
             -> (st -> Bar -> Bar -> X st) -- ^ Update function
             -> (st -> Bar -> X ())        -- ^ Delete function
             -> X (PerBar st)
perBarUpdate (PerBar sts) ups dels newf upf delf =
  -- XXX It's too bad we have to do this every time.  Perhaps we
  -- should just sort the update and deletion list.  In fact, perhaps
  -- they should be a Map and a Set to guarantee sorting.  We'll still
  -- have to listify them to do the monadic thing, but that's local.
  let ups' = sortBy (compare `on` (bBarWin.fst)) ups
      dels' = sortBy (compare `on` bBarWin) dels
  in liftM PerBar $ walk sts ups' dels'
    where
      -- Merge current state with updates and deletions
      walk [] [] [] = return []
      walk [] ((_, new):upss') [] = do
        -- Remaining updates are new bars
        st' <- newf new
        rest <- walk [] upss' []
        return $ (bBarWin new, st'):rest
      walk ((barWin, st):stss') upss (del:delss')
           | barWin == bBarWin del = do
              -- Bar deleted
              delf st del
              walk stss' upss delss'
      walk stss@((barWin, st):stss') ((old, new):upss') delss
          | barWin <  bBarWin old = do
              -- Bar in updates but not in current state
              st' <- newf new
              rest <- walk stss upss' delss
              return $ (bBarWin new, st'):rest
          | barWin == bBarWin old = do
              -- Bar updated
              st' <- upf st old new
              rest <- walk stss' upss' delss
              return $ (bBarWin new, st'):rest
      walk stss upss delss = do
        trace $ "Walk sort invariant violated"
        trace $ "Input: " ++ show (map fst sts,  ups,  dels)
        trace $ "At:    " ++ show (map fst stss, upss, delss)
        return []

-- XXX No good!
perBarLookup :: PerBar st -> Bar -> Maybe st
perBarLookup (PerBar sts) bar = lookup (bBarWin bar) sts

-- XXX Also no good!
perBarSet :: PerBar st -> Bar -> st -> PerBar st
perBarSet (PerBar sts) bar st' = PerBar [if win == bBarWin bar then (win, st') else (win, st) | (win, st) <- sts]

doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing  _ = return ()
doMaybe (Just v) a = a v

--
-- Cairo
--

newtype CairoTool = CairoTool (PerBar Surface)

cairoTool :: CairoTool
cairoTool = CairoTool perBarEmpty

instance Read CairoTool where
    readsPrec _ s = [(cairoTool, s)]

instance Show CairoTool where
    showsPrec _ _ = id

cairoUpdate :: CairoTool -> [(Bar, Bar)] -> [Bar] -> X CairoTool
cairoUpdate (CairoTool sfs) ups dels = do
  d <- asks display
  let visual = defaultVisualOfScreen (defaultScreenOfDisplay d)
      newBar bar = do
        let win = bBarWin bar
            (Rectangle _ _ (fi -> w) (fi -> h)) = bBarRect bar
        trace $ "New surface " ++ show bar
        io $ mkXlibSurface d win visual w h
      upBar sf old new = do
        let Rectangle _ _ (fi -> w) (fi -> h) = bBarRect old
            Rectangle _ _ (fi -> w') (fi -> h') = bBarRect new
        when (w /= w' || h /= h') $ do
          io $ xlibSurfaceSetSize sf w' h'
          -- XXX Implement redraw.  Actually, maybe not.  If the style
          -- cares about the size, it should redraw itself.
          trace $ "Resize surface " ++ show old ++ " -> " ++ show new
        return sf
      delBar sf bar = do
        trace $ "Delete surface " ++ show bar
        surfaceFinish sf
  liftM CairoTool $ perBarUpdate sfs ups dels newBar upBar delBar

cairoSurface :: CairoTool -> Bar -> Maybe Surface
cairoSurface (CairoTool sfs) = perBarLookup sfs

cairoSelectXFTFont :: TitleXFTFontTheme f => f -> WindowStyle -> Render ()
cairoSelectXFTFont font ws = do
  let (face, slant, weight, size) = getTitleXFTFont font ws
  selectFontFace face slant weight
  setFontSize size

--
-- Wallpaper
--

-- XXX Cache the root pixmap
-- XXX Watch for updates and redraw all
-- newtype WallpaperTool = WallpaperTool (Just XXX)

-- wallpaperTool :: WallpaperTool
-- wallpaperTool = WallpaperTool Nothing

-- instance Read WallpaperTool where
--     readsPrec _ s = [(wallpaperTool, s)]

-- instance Show WallpaperTool where
--     showsPrec _ _ = id

wallpaperSetSource :: Display -> Bar -> Render Bool
wallpaperSetSource d bar = do
  a <- io $ internAtom d "_XROOTPMAP_ID" False
  -- XXX Check all parents (second return value)
  (r, _, _) <- io $ queryTree d (bBarWin bar)
  rootpmap' <- io $ getWindowProperty32 d a r
  case rootpmap' of
    Just [rootpmap] ->
        do rootSf <- io $ mkXlibSurface' d (fromIntegral rootpmap)
           let (Rectangle (fi -> x) (fi -> y) _ _) = bBarRect bar
           setSourceSurface rootSf (-x) (-y)
           pat <- getSource
           patternSetExtend pat ExtendRepeat
           return True
    _ -> return False

--
-- Titles
--

newtype TitleTool = TitleTool (PerBar String)

titleTool :: TitleTool
titleTool = TitleTool perBarEmpty

instance Read TitleTool where
    readsPrec _ s = [(titleTool, s)]

instance Show TitleTool where
    showsPrec _ _ = id

titleUpdate :: TitleTool -> [(Bar, Bar)] -> [Bar] -> X (TitleTool, Map.Map BarWindow Bar)
titleUpdate (TitleTool ts) ups dels = do
  updates <- io $ newIORef Map.empty
  let newBar bar = do
        io $ modifyIORef updates (Map.insert (bBarWin bar) bar)
        liftM show $ getName (bWin bar)
      upBar t old new
          | bWin old == bWin new = return t
          | otherwise            = newBar new
      delBar _ _ = return ()
  tt <- liftM TitleTool $ perBarUpdate ts ups dels newBar upBar delBar
  liftM ((,) tt) $ io $ readIORef updates

titleHandleMessage :: TitleTool -> SomeMessage -> Maybe Bar -> X (Maybe TitleTool, Maybe Bar)
titleHandleMessage (TitleTool ts) m (Just bar)
    | Just (PropertyEvent {}) <- fromMessage m = do
        -- Unfortunately, xmonad's main event handler eats WM_NAME
        -- property changes.  Luckily, almost every client I've ever
        -- seen also changes at least WM_ICON_NAME right after (except
        -- xterm!).  We do out best.

        -- XXX XMonad.Operations.windows causes a WM_STATE change for
        -- every window on every layout operation.  Perhaps ignore
        -- these.
        let t = fromMaybe "" $ perBarLookup ts bar
        t' <- liftM show $ getName (bWin bar)
        if t /= t'
           then return (Just $ TitleTool $ perBarSet ts bar t', Just bar)
           else return (Nothing, Nothing)
titleHandleMessage _ _ _ = return (Nothing, Nothing)

titleTitle :: TitleTool -> Bar -> String
titleTitle (TitleTool ts) bar = fromMaybe "" $ perBarLookup ts bar

--
-- Neon
--

redrawWhen :: TitleBarStyle s => s -> [(Bar, Bar)] -> (Bar -> Bar -> Bool) -> X ()
redrawWhen s ups p =
    forM_ ups $ \(old, new) -> when (p old new) $ styleDrawBar s new

data NeonStyle font
    = NeonStyle font CairoTool TitleTool
    deriving (Show, Read)

neonStyle :: TitleXFTFontTheme font => NeonStyle font
neonStyle = NeonStyle xftFontValue cairoTool titleTool

instance TitleXFTFontTheme font => TitleBarStyle (NeonStyle font) where
    styleUpdate (NeonStyle font cairo title) ups dels = do
      cairo' <- cairoUpdate cairo ups dels
      (title', newTitles) <- titleUpdate title ups dels
      let st' = NeonStyle font cairo' title'
          updated = [(bBarWin new, new) | (old, new) <- ups,
                     bBarRect old /= bBarRect new || bSpec old /= bSpec new]
          needDraw = Map.union newTitles $ Map.fromList updated
      mapM_ (styleDrawBar st') $ Map.elems needDraw
      return st'

    styleHeight (NeonStyle font _ _) (TitleSpec ws) =
      let (_, _, _, size) = getTitleXFTFont font ws
      in ceiling (size * 1.2 + 1)

    styleDrawBar (NeonStyle font cairo title) bar = do
      trace $ "Draw " ++ show bar
      disp <- asks display
      doMaybe (cairoSurface cairo bar) $ \sf -> renderWith sf $ do
        save
        setSourceRGB 0 0 0
        wallpaperSetSource disp bar
        paint
        let TitleSpec ws@(WindowStyle active secondary _ _) = bSpec bar
            Rectangle _ _ _ (fi -> h) = bBarRect bar
        cairoSelectXFTFont font ws
        fe <- fontExtents
        te <- textExtents (titleTitle title bar)

        let (rb, gb, bb)
              | secondary = (0.4*0.75, 0.6*0.75, 0.85*0.75)
              | otherwise = (0.4, 0.85, 0.4)
            (r, g, b)
              | active    = (rb,   gb,   bb)
              | otherwise = (rb/4, gb/4, bb/4)
            alpha
              | active && not secondary = 1
              | otherwise = 0.7

        when active $ do
          withLinearPattern 0 (0.3*h) 0 (h-1) $ \pat -> do
            patternAddColorStopRGBA pat 0.0  r g b 0
            patternAddColorStopRGB  pat 0.75 r g b
            patternAddColorStopRGB  pat 1.0  (1.2*r) (1.2*g) (1.2*b)
            setSource pat
            paint

        let mar = (fontExtentsHeight fe)/3
            cr = mar
            lw = (fontExtentsHeight fe)/8
        setSourceRGBA 0 0 0 alpha
        moveTo (lw/2) (h)
        arc (cr+lw/2) (cr+lw/2) cr pi (1.5*pi)
        lineTo (textExtentsWidth te) (lw/2)
        relCurveTo (h*1.25) (lw/2) (h*0.75) (h) (3*h) (h)
        fillPreserve
        setLineWidth lw
        setSourceRGB (1.2*r) (1.2*g) (1.2*b)
        stroke

        if active && not secondary
           then setSourceRGB 1 1 1
           else setSourceRGB 0.5 0.5 0.5
        moveTo mar (fontExtentsAscent fe + 1)
        showText (titleTitle title bar)
        restore

    styleHandleMessage (NeonStyle font cairo title) m bar = do
      (title', bar') <- titleHandleMessage title m bar
      let st' = NeonStyle font cairo (fromMaybe title title')
      doMaybe bar' (styleDrawBar st')
      case title' of
        Nothing -> return Nothing
        _       -> return $ Just st'

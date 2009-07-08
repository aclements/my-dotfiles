-- Ideas
--
-- * DWIM prompt.  Combine multiple prompts in to one based on syntax.
--   Run, run in xterm, ssh, go to URL, evaluate expression, man page,
--   IM
-- * Run with output capture.  Monitor stderr, possibly only for some
--   duration, and display it.
--
-- * Get emacsclient to find the Emacs server on the current workspace
--   (if possible, accounting for Xinerama)
--
-- * Deal with Pidgin failing to activate existing conversations
--
-- * Create a temporary dzen that shows weather (anything else?
--   uptime, load average, Amarok title)
--
-- * Animated layout modifier
--
-- * Window stack layout.  Most windows appear as just title bars
--   above, below, or between visible windows.  Could have a non-zero
--   number of cursors which can be moved between or shifted to other
--   windows in the stack.  This, however, doesn't take advantage of
--   the working set.  Could have some prefix of the window list be
--   master windows which are always visible and LRU the window list.
--   It probably shouldn't be a pure LRU, since you want to be able to
--   switch to other master windows without bringing them to the top.
--   This has the nice side effect that all non-visible windows are
--   visually at the bottom of the stack.  Both of these make sizing
--   unnatural, unlike, say, the Acme interface.
--
-- * Separate dzen for each display and dynamically deal with
--   screen reconfiguration
--
-- * Full screen stack where switching windows temporarily shows a
--   stack of title bars above and below the current window.  For
--   performance, don't resize any windows, just shift them (like
--   spreading a stack of cards)
--
-- * In a stack that supports collapsed windows, be able to navigate
--   just between uncollapsed windows or between all windows.  This is
--   actually very closely related to ion's stacked tab sets
--   (especially if navigating from an uncollapsed window to a
--   collapsed window swaps their dimensions) and achieves the same
--   square root navigation efficiency but in a simpler and more
--   dynamic model.

-- Changes
--
-- * Mouse region support for dzen2.  Add an escape to specify an
--   event string that only applies to the region covered by text
--   written after the escape.  Given no arguments, it should return
--   to the default event specification.
-- ** Create a mechanism to listen on the stdout of dzen2, perhaps
--    integrated with DynamicLog, though it would be better if it can
--    be used independently.  Perhaps a simple mechanism to create an
--    event that returns a string to output to dzen2, though it would
--    need some form of GC.  Would I be able to use an honest IO
--    thread for this?
--
-- * XMonad.Prompt.Ssh should set the window title

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.SwapWorkspaces
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Util.Loggers
import XMonad.Util.Run
import XMonad.Util.NamedWindows

import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.LayoutCombinators hiding ((|||))
import XMonad.Layout.SimpleFloat
import XMonad.Layout.WindowNavigation

import XMonad.Layout.Decoration
import XMonad.Layout.DragPane
--import XMonad.Layout.SimpleDecoration

--import RaiseFocused
import XMonad.Layout.DynamicColumns
import XMonad.Layout.StackDistributed
import XMonad.Layout.StackZoomed
import XMonad.Util.DzenMux

import Control.Monad ((>=>))
import Data.List (intercalate, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)

myFont = "-*-helvetica-medium-r-*-*-10-*-*-*-*-*-*-*"

black    = "#000000"
veryDark = "#333366"
dark     = "#666699"
light    = "#9999CC"
darkGray = "#999999"
white    = "#FFFFFF"

termPrompt :: XPConfig -> X ()
termPrompt conf = do
  t <- asks (terminal . config)
  prompt (t ++ " -e") conf

myKeys x =
    -- Prompts
    [ ((modMask x,               xK_r), shellPrompt myXPConfig)
    , ((modMask x,               xK_p), submap . Map.fromList $
      [((mod, key), act)
       | mod <- [0, modMask x]
       , (key, act) <- [(xK_m, manPrompt myXPConfig),
                        (xK_o, termPrompt myXPConfig),
                        (xK_l, sshPrompt myXPConfig)]])
    , ((modMask x,               xK_s), spawn "xscreensaver-command -lock")
    ] ++
    -- 1-D window switching
    [ ((modMask x,               xK_u), windows W.focusUp)
    , ((modMask x .|. shiftMask, xK_u), windows W.swapUp)
    , ((modMask x,               xK_i), windows W.focusDown)
    , ((modMask x .|. shiftMask, xK_i), windows W.swapDown)
    ] ++
    -- 2-D WindowNavigation switching
    -- [ ((modMask x,               xK_h), sendMessage $ Go L)
    -- , ((modMask x,               xK_l), sendMessage $ Go R)
    -- , ((modMask x,               xK_j), sendMessage $ Go D)
    -- , ((modMask x,               xK_k), sendMessage $ Go U)
    -- , ((modMask x .|. shiftMask, xK_h), sendMessage $ Swap L)
    -- , ((modMask x .|. shiftMask, xK_l), sendMessage $ Swap R)
    -- , ((modMask x .|. shiftMask, xK_j), sendMessage $ Swap D)
    -- , ((modMask x .|. shiftMask, xK_k), sendMessage $ Swap U)
    -- ] ++
    -- 2-D DynamicColumns switching
    [ ((modMask x,               xK_h), modifyHS (focusUpHS))
    , ((modMask x,               xK_l), modifyHS (focusDownHS))
    -- , ((modMask x,               xK_j), modifyHS (intraHS focusDownHS) >> szZoomThis)
    -- , ((modMask x,               xK_k), modifyHS (intraHS focusUpHS) >> szZoomThis)
    , ((modMask x,               xK_j), modifyHS (intraHS focusDownHS))
    , ((modMask x,               xK_k), modifyHS (intraHS focusUpHS))
    , ((modMask x .|. shiftMask, xK_h), modifyHS (interMoveUpHS))
    , ((modMask x .|. shiftMask, xK_l), modifyHS (interMoveDownHS))
    , ((modMask x .|. shiftMask, xK_j), modifyHS (intraHS swapDownHS))
    , ((modMask x .|. shiftMask, xK_k), modifyHS (intraHS swapUpHS))
    ] ++
    -- StackDistributed
    -- [ ((modMask x,               xK_comma),  send2Messages SDExpand (IncMasterN 1))
    -- , ((modMask x,               xK_period), send2Messages SDContract (IncMasterN (-1)))
    -- ] ++
    -- StackZoomed
    [ ((modMask x,               xK_comma),  send2Messages SZContract (IncMasterN 1))
    , ((modMask x,               xK_period), send2Messages SZExpand (IncMasterN (-1)))
    , ((modMask x,               xK_Return), szZoomThis)
    ] ++
    -- Workspace switching
    [((m .|. modMask x, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces x) [xK_F1 .. xK_F9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++
    -- Workspace swapping
    [ ((modMask x .|. controlMask, xK_u), swapTo Prev)
    , ((modMask x .|. controlMask, xK_i), swapTo Next)
    ] ++
    -- HJKL workspace switching
    [ ((modMask x .|. controlMask, xK_h), prevWS)
    , ((modMask x .|. controlMask, xK_l), nextWS)
    ] ++
    -- HJKL workspace swapping
    [ ((modMask x .|. controlMask .|. shiftMask, xK_h), swapTo Prev)
    , ((modMask x .|. controlMask .|. shiftMask, xK_l), swapTo Next)
    ]

send2Messages m1 m2 = sendMessage m1 >> sendMessage m2

myAntiKeys x =
    -- Run prompts
    [ (modMask x              , xK_p)
    , (modMask x .|. shiftMask, xK_p)
    ] ++
    -- Workspace switching
    [(m .|. modMask x, k)
     | k <- [xK_1 .. xK_9]
     , m <- [0, shiftMask]]

myDecoTheme =
    defaultTheme
    { fontName = myFont
    , decoHeight = 11
    , activeColor = dark
    , inactiveColor = veryDark
    , urgentColor = "red"
    , activeBorderColor = dark
    , inactiveBorderColor = veryDark
    , activeTextColor = white
    , inactiveTextColor = dark
    }

-- myLayouts = ({-raiseFocused $-} windowNavigation $ Tall 2 (3/100) (1/2)) ||| Full ||| Circle ||| dragPane Vertical 0.1 0.5 ||| dynamicColumns (StackDistributed 5.0 1.0) ||| simpleFloat

--myLayouts = dynamicColumns (StackZoomed Nothing 5.0 1.0 ||| Full) ||| Circle ||| simpleFloat
myLayouts = dynamicColumns (decoration shrinkText myDecoTheme DefaultDecoration (StackZoomed Nothing 5.0 1.0 ||| Full)) ||| Circle ||| simpleFloat
--myLayouts = dynamicColumns (dragPane Horizontal 0.1 0.5)
--myLayouts = (dragPane Horizontal 0.1 0.5)

myManageHook =
    composeOne
    [ className =? "stalonetray" -?> doIgnore
    , isKDETrayWindow -?> doIgnore
    ]

myXPConfig =
    defaultXPConfig
    { font        = myFont
    , bgColor     = veryDark
    , borderColor = light
    , position    = Top
    , height      = 16
    }


data AllWindowsPP
    = AllWindowsPP
      { awppCurrent :: String -> String
      , awppVisible :: String -> String
      , awppSep :: String
      }

dzenAWPP =
    AllWindowsPP
    { awppCurrent = dzenColor white    "" . pad . trim 24
    , awppVisible = dzenColor darkGray "" . pad . trim 24
    , awppSep     = "  "
    }
    where trim n s
              | length s > n = trim2 n (dropSuffix s suffixes)
              | otherwise    = s
          trim2 n s
              | length s > n = take (n-3) s ++ ".."
              | otherwise    = s
          dropSuffix str (s:ss)
              | s `isSuffixOf` str = reverse $ ".." ++ (drop (length s) $ reverse $ str)
              | otherwise = dropSuffix str ss
          dropSuffix str [] = str
          suffixes = [" - Adobe Reader", " - Iceweasel", " - emacs"]

allWindows :: AllWindowsPP -> X (Maybe String)
allWindows (AllWindowsPP ppCur ppVis ppSep) = do
  winset <- gets windowset
  let windows = W.integrate' $ W.stack $ W.workspace $ W.current winset
      curWin  = W.peek winset
  strings <- mapM (\w -> do n <- getName w
                            if Just w == curWin
                               then return $ ppCur $ show n
                               else return $ ppVis $ show n)
             windows
  return $ Just $ intercalate ppSep strings

myPP =
    dzenPP
    { ppCurrent  = dzenColor white light    . pad
    , ppVisible  = dzenColor white dark     . pad
    , ppHidden   = dzenColor white veryDark . pad
    , ppHiddenNoWindows = dzenColor darkGray black . pad
    , ppUrgent   = dzenColor white "red"
    , ppLayout   = dzenColor white black
    , ppTitle    = dzenColor white black    . dzenEscape
    , ppSep      = " " ++ dzenColor "" darkGray "^r(1,1)" ++ " "
    , ppOrder    = \(ws:l:t:aw:e) -> e++[ws,l,aw]
    , ppExtras   = [allWindows dzenAWPP]
    }

main = do
  env <- getEnvironment
  hDzen <- spawnPipe ("dzen2 -e onstart=lower -ta l" ++
                      " -fn '" ++ myFont ++ "' -h 16 -bg black")
  (dynamicLogOut, prodDynamicLog) <- prodExternal
  dzenMux hDzen [ prodClock (dzenColor white "" "%a %m/%d  %l:%M %p")
                , prodConst (ppSep myPP)
                , prodDynamicLog]
  let v ^> f = f v
  xmonad (defaultConfig
          ^> \c -> c
          -- Key mappings
          { modMask = mod4Mask  {- Flag key -}
          , keys = \x -> Map.union (Map.fromList (myKeys x)) $
                   foldr Map.delete (keys c x) (myAntiKeys x) }
          -- Blue color theme
          { normalBorderColor = veryDark
          , focusedBorderColor = light }
          -- Custom layouts
          { layoutHook = {- layoutHook c ||| -} myLayouts }
          -- Save space for dock windows (must come after custom layout)
          ^> \c -> c
          { manageHook = manageDocks
          , layoutHook = avoidStruts (layoutHook c) }
          -- Assorted management hooks
          ^> \c -> c
          { manageHook = manageHook c <+> myManageHook }
          -- Derive terminal from $XTERMCMD
          { terminal = fromMaybe (terminal c) $ lookup "XTERMCMD" env }
          -- Drive dzen
          { logHook = dynamicLogWithPP $ myPP { ppOutput = dynamicLogOut } }
          -- Indicate inactive windows by fading (requires compositing manager)
--           ^> \c -> c
--           { logHook = fadeInactiveLogHook 0xaaaaaaaa >> logHook c
--           , borderWidth = 0
--           }
          -- XXX
          ^> \c -> c { borderWidth = 0 }
         )

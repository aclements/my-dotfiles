-- Ideas
--
-- * DWIM prompt.  Combine multiple prompts in to one based on syntax.
--   Run, run in xterm, ssh, go to URL, evaluate expression, man page,
--   IM
-- * Run with output capture.  Monitor stderr, possibly only for some
--   duration, and display it.
--
-- * Fix clock so it updates even if there are no window actions
--
-- * In full layout, show all window titles in dzen and highlight the
--   current one
--
-- * Get emacsclient to find the Emacs server on the current workspace
--   (if possible, accounting for Xinerama)
--
-- * Deal with Pidgin failing to activate existing conversations

-- Changes
--
-- * Why does xmonad explicitly clear the import list when calling ghc
--   on xmonad.hs?  Is there some reason it can't add ~/.xmonad and
--   thus allow configurations to be broken up into modules?
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

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.Submap
import XMonad.Hooks.DynamicLog
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
import XMonad.Layout.Dishes
import XMonad.Layout.Magnifier
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed

import Control.Monad ((>=>))
import Data.List (intercalate)
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
    [((modMask x,               xK_r  ), shellPrompt myXPConfig),
     ((modMask x,               xK_p  ), submap . Map.fromList $
      [((mod, key), act)
       | mod <- [0, modMask x]
       , (key, act) <- [(xK_m, manPrompt myXPConfig),
                        (xK_o, termPrompt myXPConfig),
                        (xK_l, sshPrompt myXPConfig)]])]
    ++
    -- 1-D window switching
    [((modMask x,               xK_u  ), windows W.focusUp),
     ((modMask x .|. shiftMask, xK_u  ), windows W.swapUp),
     ((modMask x,               xK_i  ), windows W.focusDown),
     ((modMask x .|. shiftMask, xK_i  ), windows W.swapDown)]
    ++
    -- Workspace switching
    [((m .|. modMask x, k), windows $ f i)
     | (i, k) <- zip (XMonad.workspaces x) [xK_F1 .. xK_F9]
     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

myAntiKeys x =
    -- Run prompts
    [(modMask x              , xK_p),
     (modMask x .|. shiftMask, xK_p)]
    ++
    -- Workspace switching
    [(m .|. modMask x, k)
     | k <- [xK_1 .. xK_9]
     , m <- [0, shiftMask]]

myLayouts = Accordion ||| {- magnifiercz 1.2 -} Circle ||| Dishes 2 (1/6) ||| simpleTabbed

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
    { awppCurrent = dzenColor white "" . pad
    , awppVisible = dzenColor darkGray "" . pad
    , awppSep = "  "
    }

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
    , ppExtras   = [allWindows dzenAWPP, date $ dzenColor white "" "%a %m/%d  %l:%M %p"]
    }

main = do
  env <- getEnvironment
  hDzen <- spawnPipe ("dzen2 -e onstart=lower -ta l" ++
                      " -fn '" ++ myFont ++ "' -h 16 -bg black")
  let v ^> f = f v
  xmonad (defaultConfig
          ^> \c -> c
          -- Key mappings
          { modMask = mod4Mask
          , keys = \x -> Map.union (Map.fromList (myKeys x)) $
                   foldr Map.delete (keys c x) (myAntiKeys x) }
          -- Blue color theme
          { normalBorderColor = veryDark
          , focusedBorderColor = light }
          -- Custom layouts (must come before docks)
          { layoutHook = layoutHook c ||| myLayouts }
          -- Save space for dock windows
          ^> \c -> c
          { manageHook = manageDocks
          , layoutHook = avoidStruts (layoutHook c) }
          -- Assorted management hooks
          ^> \c -> c
          { manageHook = manageHook c <+> myManageHook }
          -- Use XTERMCMD
          { terminal = fromMaybe (terminal c) $ lookup "XTERMCMD" env }
          -- Run dzen
          { logHook = dynamicLogWithPP $ myPP { ppOutput = hPutStrLn hDzen } }
         )

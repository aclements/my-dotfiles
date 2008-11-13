{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.DynamicColumns
-- Copyright   :  (c) 2008 Austin Clements
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Austin Clements <amdragon+xmonad@mit.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Dynamic columns is a layout combinator that divides the screen into
-- a variable number of columns and applies a sub-layout to each
-- column.  Each column maintains its own focus and windows can be
-- moved either within or between columns.  Columns are created by
-- moving windows off the right-most or left-most column.  Likewise,
-- columns are destroyed by moving or closing all of the windows in a
-- column.
--
-----------------------------------------------------------------------------

module XMonad.Layout.DynamicColumns
    ( -- * Usage
      -- $usage
      DynamicColumns
    , DCMessage(..)
    , dynamicColumns)
    where

import XMonad hiding (focus)
import XMonad.StackSet (Stack(..), Workspace(Workspace), modify', integrate)

import XMonad.Hooks.ManageDocks (Direction(..))

import Control.Monad (zipWithM)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Set as S

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.DynamicColumns
--
-- Then add 'dynamicColumns' to your layout.  For example
--
-- > myLayouts = dynamicColumns (Tall 0 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- will add a dynamic column layout which divides the screen into
-- columns, each themselves laid out using Tall (note that the use of
-- @0@ as the first argument to Tall will disable the master pane and
-- just stack the windows evenly).  Alternatively, dynamic columns
-- works well as the top-level layout with a choice of layouts in each
-- column.  For example,
--
-- > myLayouts = dynamicColumns (Tall 0 (3/100) (1/2) ||| Full)
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- Note that @|||@ here is used /inside/ the argument to
-- dynamicColumns.  This allows you to switch between layouts within
-- each column independently (using, for example, mod-space).  With
-- this approach, columns are always available, but you also have the
-- choice of using a single column, which reduces to simply using the
-- active sub-layout for the whole screen.
--
-- In general, dynamic columns combines well with stack-oriented
-- layouts such as "XMonad.Layout.Accordion", "XMonad.Layout.Dishes",
-- "XMonad.Layout.StackTile", or "XMonad.Layout.StackDistributed".
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- To make dynamic columns useful, you should add bindings at least
-- for moving windows between columns.  Without these, you will not be
-- able to create more than one column:
--
-- > , ((modMask x .|. shiftMask, xK_h), sendMessage $ DCMove L)
-- > , ((modMask x .|. shiftMask, xK_l), sendMessage $ DCMove R)
--
-- Likewise, the following bindings allow you to move the focus
-- between columns
--
-- > , ((modMask x,               xK_h), sendMessages $ DCGo L)
-- > , ((modMask x,               xK_l), sendMessages $ DCGo R)
--
-- For consistency, you can also add the following bindings for moving
-- windows within a column and for moving focus within a column.  The
-- default bindings of mod-shift-j and mod-shift-k still work, but can
-- occasionally cause windows to move between columns in unexpected
-- ways.  Likewise mod-j and mod-k still work, but wrap the focus
-- between columns, whereas the following stops the focus from moving
-- at the top or bottom of a column.
--
-- > , ((modMask x .|. shiftMask, xK_k), sendMessage $ DCMove U)
-- > , ((modMask x .|. shiftMask, xK_j), sendMessage $ DCMove D)
-- > , ((modMask x,               xK_k), sendMessage $ DCGo U)
-- > , ((modMask x,               xK_j), sendMessage $ DCGo D)
--
-- Note that these last four bindings, as written, will replace
-- existing general bindings, but will only work in dynamic columns
-- layouts.

-- XXX Variable column widths
data DynamicColumns sl a
    = DC (Either (Stack (Stack a, sl a)) (sl a)) (sl a)
      deriving (Show, Read)

-- | Construct a dynamic column layout.  The argument specifies the
-- default sub-layout to use for new columns.
dynamicColumns :: sl a -> DynamicColumns sl a
dynamicColumns sl = DC (Right sl) sl

-- XXX Variations on L/R window moving: Should focus stay with the
-- moving window or go to the next window in the stack?  Should a
-- window form a new stack in preference to moving into another stack?
-- There are some other options which are dictated by XMonad
-- standards, but could be done differently.  Should a window move in
-- above or below the focused window in a stack?  Should the focus in
-- the stack it left move up or down?
--
-- Variations on L/R focus moving: Should focus stop or wrap?
--
-- Variations on U/D focus moving: Should focus stop, wrap within the
-- column, or wrap to the next column?
data DCMessage
    = DCMove Direction
      -- ^ Move the current window in the given direction.  If this is
      -- an inter-column move and the window is being moved beyond the
      -- last column on either side, a new column in that direction
      -- will be created.
    | DCGo Direction
      -- ^ Move the focus in the given direction, stopping at the
      -- boundaries of the screen.
    | DCBroadcast SomeMessage
      -- ^ With the exception of 'XMonad.Core.LayoutMessage's and X
      -- events, DC only forwards messages to the sub-layout for the
      -- currently active column.  Wrapping a message in DCBroadcast
      -- will cause it to be forwarded to all of the sub-layouts.
      deriving (Typeable)
instance Message DCMessage

instance (Show (sl Window), LayoutClass sl Window)
    => LayoutClass (DynamicColumns sl) Window where

    runLayout (Workspace tag (DC oldStack def) Nothing) rect = do
      -- Reduce to a single column and run its empty layout
      let sl = case oldStack of
                 Left (Stack (_, l) _ _) -> l
                 Right l                 -> l
      (wins, sl') <- runLayout (Workspace tag sl Nothing) rect
      return (wins, Just $ DC (Right $ fromMaybe sl sl') def)

    runLayout (Workspace tag (DC oldHS' def) (Just newStack)) rect = do
      -- Update the hierarchical stack
      let hs = case oldHS' of
                 Left oldHS -> update oldHS newStack
                 Right layout  -> Stack (newStack, layout) [] []
      -- Compute the column rectangles
      let nCols = length (up hs) + length (down hs) + 1
          rects = splitHorizontally nCols rect
          (lrsR, tr:rrs) = splitAt (length (up hs)) rects
          lrs   = reverse lrsR
      -- Run sub-layouts
      let runSL (sStack, sl) sRect = do
            (wins, sl') <- runLayout (Workspace tag sl (Just sStack)) sRect
            return (wins, (sStack, fromMaybe sl sl'))
      (tWins, tSl) <- runSL (focus hs) tr
      lRes <- zipWithM runSL (up hs)   lrs
      rRes <- zipWithM runSL (down hs) rrs
      -- Construct the new layout
      let dc' = DC (Left $ Stack tSl (map snd lRes) (map snd rRes)) def
      -- Construct the window configuration
      let wins = tWins ++ concatMap fst (lRes ++ rRes)
      return (wins, Just dc')

    -- XXX Send Hide to deleted sub-layouts
    --
    -- XXX Fake XState when resending.  Or should this be controlled
    -- by the message?  For example, keep the full stack by default,
    -- but have a message wrapping (X ()) that limits the action to
    -- the focused column.  Many actions are screen-level, though
    -- those that generate messages are probably targeting them at the
    -- current sub-layout...
    handleMessage layout msg
        | Just m <- fromMessage msg = handleDCMessage layout m
        | Just _ <- fromMessage msg :: Maybe Event =
            forwardMessage layout True msg
        | Just _ <- fromMessage msg :: Maybe LayoutMessages =
            forwardMessage layout True msg
        | otherwise = forwardMessage layout False msg

    description (DC hs _) =
        let descs = case hs of
                      Right sl -> [description sl]
                      Left (Stack (_, tsl) ls rs) ->
                          (reverse (map (description.snd) ls) ++
                           (description tsl : map (description.snd) rs))
        in intercalate " | " descs -- XXX User control

-- | Handle a 'DCMessage'.
handleDCMessage :: (LayoutClass sl Window) =>
                   DynamicColumns sl Window
                -> DCMessage
                -> X (Maybe (DynamicColumns sl Window))
handleDCMessage layout msg =
    case msg of
      DCMove L -> updateHS layout interMoveUp
      DCMove R -> updateHS layout (\def -> reversed $ interMoveUp def)
      DCMove U -> updateHS layout (\_ -> intra swapUp')
      DCMove D -> updateHS layout (\_ -> intra (reversed swapUp'))
      DCGo L   -> updateHS layout (\_ -> focusUp')
      DCGo R   -> updateHS layout (\_ -> reversed focusUp')
      DCGo U   -> updateHS layout (\_ -> intra focusUp')
      DCGo D   -> updateHS layout (\_ -> intra (reversed focusUp'))
      DCBroadcast m -> forwardMessage layout True m

-- | Forward a message to either the focused sub-layout or to all
-- sub-layouts and update the sub-layout state.
forwardMessage :: (LayoutClass sl Window) =>
                  DynamicColumns sl Window  -- ^ Layout state
               -> Bool                      -- ^ True to broadcast
               -> SomeMessage               -- ^ Message to forward
               -> X (Maybe (DynamicColumns sl Window))
forwardMessage (DC hs def) broadcast msg = do
  case hs of
    Right sl ->
        do def' <- if broadcast then send def else return Nothing
           sl' <- send sl
           case (def', sl') of
             (Nothing, Nothing) -> return Nothing
             _ -> return $ Just $
                  DC (Right (fromMaybe sl sl')) (fromMaybe def def')
    Left (Stack t@(_, tsl) ls rs)
      | broadcast ->
        do def' <- send def
           tsl' <- send tsl
           ls' <- mapM (send.snd) ls
           rs' <- mapM (send.snd) rs
           case (def', tsl') of
             (Nothing, Nothing)
                 | all isNothing ls' && all isNothing rs' ->
                     return Nothing
             _ -> return $ Just $
                  DC (Left (Stack (updateSL t tsl')
                                  (zipWith updateSL ls ls')
                                  (zipWith updateSL rs rs')))
                     (fromMaybe def def')
      | otherwise ->
        do tsl' <- send tsl
           case tsl' of
             Nothing -> return Nothing
             _ -> return $ Just $ DC (Left (Stack (updateSL t tsl') ls rs)) def
    where send sl = handleMessage sl msg `catchX` return Nothing
          updateSL (ss, sl) sl' = (ss, fromMaybe sl sl')

-- | Transform a function that manipulates a stack into a function
-- that manipulates the focused stack of a hierarchical stack.
intra :: (Stack a -> Stack a) -> (Stack (Stack a, b) -> Stack (Stack a, b))
intra func (Stack (sf, arg) ls rs) = Stack (func sf, arg) ls rs

-- | Transform a function that manipulates a stack into a function
-- that manipulates the reversed stack.
reversed :: (Stack a -> Stack a) -> (Stack a -> Stack a)
reversed func (Stack f u d) =
    let (Stack f' d' u') = func (Stack f d u)
    in Stack f' u' d'

-- | Move a stack's focus up one window, stopping at the edge.
focusUp' :: Stack a -> Stack a
focusUp' (Stack f (f':u) d) = Stack f' u (f:d)
focusUp' st = st

-- | Swap the focused window with the one up from it, stopping at the
-- edge.  Keeps the window focused.
swapUp' :: Stack a -> Stack a
swapUp' (Stack f (u:us) d) = Stack f us (u:d)
swapUp' st = st

-- | Move the focused window from the focused column to the next
-- column up, constructing a new column if there is no column to the
-- left.  Keeps the window focused and moves the column focus
-- accordingly.
interMoveUp :: sl a -> Stack (Stack a, sl a) -> Stack (Stack a, sl a)
interMoveUp def hs =
    let win = focus $ fst $ focus hs
        (focus':up') =
            case up hs of
              [] -> [(Stack win [] [], def)]
              (Stack f su sd, sl):ss ->
                  (Stack win su (f:sd), sl):ss
        down' =
            case focus hs of
              (Stack _ [] [],     _)  -> down hs
              (Stack _ (f:su) [], sl) -> (Stack f su [], sl) : down hs
              (Stack _ su (f:sd), sl) -> (Stack f su sd, sl) : down hs
    in Stack focus' up' down'

-- | Apply a transformation to the current hierarchical stack,
-- updating both the layout state and the active 1-D stack to reflect
-- the new hierarchical stack.  Note that this updates the 1-D stack
-- without forcing a refresh so it can be used in message handlers
-- without resulting in extra refreshes.
--
-- XXX Does 'XMonad.Operations.windows' assume that the windowset
-- hasn't been modified behind its back in order to figure out which
-- windows to hide?  If so, then this can only be used if the set of
-- windows will not be changed.
updateHS :: DynamicColumns sl Window  -- ^ Current layout state
         -> (sl Window
                 -> Stack (Stack Window, sl Window)
                 -> Stack (Stack Window, sl Window))
            -- ^ Transformation function.  The first argument is the
            -- default layout
         -> X (Maybe (DynamicColumns sl Window))
updateHS (DC (Right _) _)   _ = return Nothing
updateHS (DC (Left hs) def) f = do
  let hs' = f def hs
      dc' = DC (Left hs') def
      stack = flatten hs'
  -- XXX Could use a flag to indicate that the stack and the
  -- hierarchical stack are known to be in sync.
  modify $ \xstate ->
    xstate { windowset = modify' (const stack) (windowset xstate) }
  return $ Just dc'

-- | Flatten a hierarchical stack, retaining its order and primary
-- focus.
flatten :: Stack (Stack a, b) -> Stack a
flatten (Stack (Stack t tls trs, _) ls rs) =
    -- XXX Could be much more efficient.
    let ls' = tls ++ reverse (concatMap (integrate.fst) (reverse ls))
        rs' = trs ++ concatMap (integrate.fst) rs
    in Stack t ls' rs'

-- | Update a hierarchical stack to reflect a new set of objects,
-- retaining the structure of the old hierarchical stack as much as
-- possible.  This operation is necessarily heuristic.  Ideally, the
-- double integration of the old hierarchical stack should be similar
-- to the integration of the new stack.  The heuristics of this
-- function are designed such that a single up-insertion, deletion, or
-- swap of two objects from the old stack to the new stack will result
-- in a hierarchical stack with the expected structure and
-- combinations of these operations will at least do something
-- reasonable.
--
-- This function obeys the following critical rule:
--
-- * The focus of the focused stack of the updated stack will equal
--   the focus of the new stack.  The concatenation of the
--   integrations of the stacks of the up side of the updated stack
--   plus the up side of the stack focused in the updated stack will
--   equal the up side of the new stack.  Likewise for the down sides.
--
-- As corollaries,
--
-- * The double integration of the updated stack will always equal the
--   integration of the new stack.  That is, the set of windows and
--   their relative ordering will be the same in the updated
--   hierarchical stack as it was in the new stack.
--
-- * The primary focus of the result will always correspond to the
--   focus of the new stack.
--
-- This function retains the structure of the old hierarchical stack
-- according to the following rule:
--
-- * The number of stacks in the updated hierarchical stack will equal
--   the number of stacks in the old hierarchical stack, minus the
--   number of stacks in the old hierarchical stack that consisted
--   entirely of objects not present in the new stack.
--
-- This function tries to keep the number of objects in each stack of
-- the result the same as the number of objects in each stack of the
-- original, modulo objects that appear in the new stack but not the
-- old stack, and vice-versa.  Similarly, except for the stack where
-- the focus may be changing, it tries to keep the focus of the other
-- stacks at the same numerical offset, again modulo inserted and
-- deleted objects.
--
-- An alternative to this approach would be to add sentinel objects to
-- the non-hierarchical stack so it could easily be sub-divided into a
-- hierarchical stack.  Unfortunately, this causes issues with
-- anything that traverses the non-hierarchical stack, including such
-- basic things as focus.  It also does little to track the focus in
-- secondary stacks.
update :: Ord a =>
          Stack (Stack a, b) -- ^ Old hierarchical stack, plus arguments
       -> Stack a            -- ^ New stack
       -> Stack (Stack a, b) -- ^ Updated hierarchical stack
update oldStack newStack =
    let oldSet   = S.fromList (concatMap (integrate.fst) $ integrate oldStack)
        newCur   = stackToCursor newStack
        newSet   = S.fromList (fst newCur)
        deleted  = oldSet S.\\ newSet
        inserted = newSet S.\\ oldSet
        -- Like upStack, but thread an argument through and
        -- list-convert the Maybe result
        upStackArg func final new (old, arg) =
            case upStack func final new old of
              (new', Nothing) -> (new', [])
              (new', Just st) -> (new', [(st, arg)])
        (_, Just hst) = (upStack (upStackArg (upElem deleted inserted)))
                        True newCur oldStack
    in hst

-- | A cursor into a stack.  This allows us to represent any suffix of
-- a stack, making them more amenable to traversal.  The first of the
-- pair is some suffix of the integration of the stack.  The second of
-- the pair is the index into the list of the focused element, and may
-- be negative if the cursor is in the \"down\" part of the stack.
type StCur a = ([a], Int)

-- | /O(|up|)/.  Convert a stack to a cursor.
stackToCursor :: Stack a -> StCur a
stackToCursor st = (integrate st, length (up st))

-- | /O(|up|)/.  Convert a cursor to a stack.  This will return
-- 'Nothing' if the focus is outside of the cursor (including if the
-- cursor is empty)
cursorToStack :: StCur a -> Maybe (Stack a)
cursorToStack (suf, idx)
    | idx < 0   = Nothing
    | otherwise =
        case splitAt idx suf of
          (lsR, t:rs) -> Just (Stack t (reverse lsR) rs)
          _           -> Nothing

-- | Prepend a list to a cursor, keeping the focus on the same element.
curPrepend :: [a] -> StCur a -> StCur a
curPrepend begin (end, idx) = (begin ++ end, idx + length begin)

-- | Take all remaining elements from a cursor, returning the list of
-- elements and a cursor updated to point to the end.
curTakeAll :: StCur a -> (StCur a, [a])
curTakeAll (suf, idx) = (([], idx - length suf), suf)

-- | True if and only if the focused object is contained in the first
-- cursor but not the second.
cursStraddleFocus :: StCur a -> StCur a -> Bool
cursStraddleFocus (_, idx1) (_, idx2) = idx1 >= 0 && idx2 < 0

-- | Apply /func/ to each element of stack /old/ from left to right,
-- allowing it to consume arbitrary prefixes from cursor /new/.
-- Concatenate the results into a stack, putting the focus either on
-- the first element that was produced by the function when it
-- consumed the focus of /new/ or, failing that, the first element
-- that was produced by applying the function to the focus of /old/,
-- or, failing that, the last element of the resulting stack.  Returns
-- the unconsumed part of /new/, as well as the resulting stack or
-- Nothing if the function never returned any results.
--
-- Note that upStack with one curried argument is almost, but not
-- quite suitable for passing as the function to another upStack.  The
-- type given here keeps things tighter.  In order to make it
-- suitable, apply 'Data.Maybe.listToMaybe' to the second part of the
-- result.  If this becomes a problem, we could generalize the return
-- type of /func/ to any 'Data.Foldable.Foldable'.
upStack :: (Bool -> StCur a -> b -> (StCur a, [c])) -- ^ /func/
        -> Bool    -- ^ If True, pass True to /func/ for the final
                   --   element of /old/.
        -> StCur a -- ^ /new/
        -> Stack b -- ^ /old/
        -> (StCur a, Maybe (Stack c))
upStack func final new old =
    let join newCur ([], _) = (newCur, ([], -1), False)
        join newCur ((o:os), oldIdx) =
            let final' = final && null os
                (newCur', outFrag) = func final' newCur o
                (newEnd, restCur, lockFocus) = join newCur' (os, oldIdx - 1)
                (out, outFocus) = curPrepend outFrag restCur
                (outFocus', lockFocus')
                    | lockFocus                        = (outFocus, lockFocus)
                    -- XXX Without the next clause, deleting b from
                    -- [<[a,<b>]>,[<c>]] fails
                    | null out                         = (-1,       lockFocus)
                    | cursStraddleFocus newCur newCur' = (0,        True)
                    | oldIdx == 0                      = (0,        False)
                    | otherwise                        = (outFocus, lockFocus)
            in (newEnd, (out, outFocus'), lockFocus')
        (new', outCur, _) = join new (stackToCursor old)
    in (new', cursorToStack outCur)

-- | Update a single element from a stack, accounting for deleted and
-- inserted elements.  This is meant to be used as the leaf function
-- to 'upStack'.  If the given element is in the deleted set, consumes
-- nothing and returns no elements.  Otherwise, consumes elements from
-- the cursor as long as they're in the inserted set, plus one more
-- (if possible), and returns the new cursor plus the list of consumed
-- elements.
upElem :: Ord a =>
          S.Set a -- ^ Deleted set
       -> S.Set a -- ^ Inserted set
       -> Bool    -- ^ If True, consume all remaining elements
       -> StCur a -- ^ Cursor from which to consume elements
       -> a       -- ^ Element from the old stack to update
       -> (StCur a, [a])
upElem _       _        True  new _   = curTakeAll new
upElem deleted inserted False new old
    | old `S.member` deleted = (new, [])
    | otherwise =
        -- Consume as long as the element is in inserted, plus one more
        let consume cur@([], _) = (cur, [])
            consume ((x:xs), idx)
                | x `S.member` inserted =
                    let (lastCur, xs') = consume nextCur
                    in (lastCur, x:xs')
                | otherwise = (nextCur, [x])
                where nextCur = (xs, idx - 1)
        in consume new

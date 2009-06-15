{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances,
    MultiParamTypeClasses, TypeSynonymInstances, PatternGuards,
    Rank2Types, ImplicitParams #-}

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

-- XXX Switching between columns while there are floating windows
-- unmanages the floating windows?

-- XXX Put a single window in the left-most column and push it left.
-- The column will switch to the default layout and not release the
-- resources of the old layout.

module XMonad.Layout.DynamicColumns
    ( -- * Usage
      -- $usage

      -- ** Dynamic columns as a top-level layout
      -- $toplevel

      -- * Layout
      DynamicColumns, dynamicColumns
    , DCMessage(DCBroadcast)
      -- * Hierarchical stack modification
    , HSXForm, modifyHS, modifyHSOr
      -- ** Standard transformers
    , focusUpHS, focusDownHS, swapUpHS, swapDownHS, interMoveUpHS, interMoveDownHS
      -- ** Cyclical versions
    , focusUpCycleHS, focusDownCycleHS, swapUpCycleHS, swapDownCycleHS
      -- ** Higher-order transformers
    , intraHS, reversedHS)
    where

import XMonad hiding (focus)
import XMonad.StackSet (Stack(..), Workspace(Workspace))
import qualified XMonad.StackSet as W

import Control.Monad (zipWithM, liftM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
-- columns, each themselves laid out using 'XMonad.Layout.Tall' (note
-- that the use of @0@ as the first argument to 'XMonad.Layout.Tall'
-- will disable the master pane and just stack the windows equally).
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
-- > , ((modMask x .|. shiftMask, xK_h), modifyHS interMoveUpHS)
-- > , ((modMask x .|. shiftMask, xK_l), modifyHS interMoveDownHS)
--
-- Likewise, the following bindings allow you to move the focus
-- between columns
--
-- > , ((modMask x,               xK_h), modifyHS focusUpHS)
-- > , ((modMask x,               xK_l), modifyHS focusDownHS)
--
-- For consistency, you can also add the following bindings for moving
-- windows within a column and for moving focus within a column.  The
-- default bindings of mod-shift-j and mod-shift-k still work, but can
-- occasionally cause windows to move between columns in unexpected
-- ways.  Likewise mod-j and mod-k still work, but wrap the focus
-- between columns, whereas the following stops the focus from moving
-- at the top or bottom of a column.  Here we use 'modifyHSOr' to fall
-- back on the default actions in other layouts.
--
-- > , ((modMask x .|. shiftMask, xK_j), modifyHSOr (intraHS swapDownHS)  (windows W.swapDown))
-- > , ((modMask x .|. shiftMask, xK_k), modifyHSOr (intraHS swapUpHS)    (windows W.swapUp))
-- > , ((modMask x,               xK_j), modifyHSOr (intraHS focusDownHS) (windows W.focusDown))
-- > , ((modMask x,               xK_k), modifyHSOr (intraHS focusUpHS)   (windows W.focusUp))
--
-- Alternatively, you can use the cyclical variants 'focusUpCycleHS',
-- 'swapUpCycleHS', etc. if you're into that sort of thing.

-- $toplevel
--
-- Typically, the top-level layout is constructed using the
-- 'XMonad.Layout.|||' choice operator to combine multiple layouts
-- such that it's possible to switch between layouts for the entire
-- screen.  Dynamic columns is a good replacement top-level layout
-- when used with the choice operator for the sub-layout.
--
-- For example,
--
-- > myLayouts = dynamicColumns (Tall 0 (3/100) (1/2) ||| Full)
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- Note that @|||@ here is used /inside/ the argument to
-- dynamicColumns.  This allows you to switch between layouts within
-- each column independently (using, for example, mod-space).  With
-- this approach, using a single column reduces to applying the
-- selected sub-layout to the entire screen like usual, but columns
-- are always available.

-- | The dynamic columns layout.  Use 'dynamicColumns' to construct
-- layouts of this type.
data DynamicColumns sl a
    -- XXX Variable column widths
    = DC (Either (Stack (Stack a, sl a)) (sl a)) (sl a)
      deriving (Show, Read)

-- | Construct a dynamic column layout.  The argument specifies the
-- default sub-layout to use for new columns.
dynamicColumns :: sl a -> DynamicColumns sl a
dynamicColumns sl = DC (Right sl) sl

-- | A hierarchical stack transformer function.  Through the magic of
-- implicit parameters, a transformer has optional access to the
-- default sublayout.
type HSXForm = forall a sl . (?def :: sl a) =>
    Stack (Stack a, sl a) -> Stack (Stack a, sl a)

-- | The result of a 'DCModifyHS' message.
data DCModifyResult a
    = ModifyNotHandled
      -- ^ The modification message was not handled.
    | ModifyUnchanged
      -- ^ The modification message was handled, but didn't result in
      -- a change.
    | ModifyTo a
      -- ^ The modification message was handled and resulting in the
      -- given update.

data DCMessage
    = DCModifyHS HSXForm (IORef (DCModifyResult (Stack Window)))
      -- ^ Apply the given hierarchical stack transform to update the
      -- layout state and write the resulting flat stack to the given
      -- IORef.  The caller is responsible for updating the flat stack
      -- and refreshing the windows.  This should be used only
      -- internally by 'modifyHS'.
    | DCBroadcast SomeMessage
      -- ^ With the exception of 'XMonad.Core.LayoutMessage's and X
      -- events, DC only forwards messages to the sub-layout for the
      -- currently active column.  Wrapping a message in DCBroadcast
      -- will cause it to be forwarded to all of the sub-layouts.
      deriving (Typeable)
instance Message DCMessage

instance (Show (sl Window), LayoutClass sl Window)
    => LayoutClass (DynamicColumns sl) Window where

    runLayout (Workspace tag (DC oldHS def) Nothing) rect = do
      -- Reduce to a single column and run its empty layout
      let (sl, delSLs) =
              case oldHS of
                Left (Stack (_, l) ls rs) -> (l, map snd (ls ++ rs))
                Right l                   -> (l, [])
      -- Release deleted columns
      mapM_ (flip handleMessage (SomeMessage ReleaseResources)) delSLs
      -- Run the one layout
      (wins, sl') <- runLayout (Workspace tag sl Nothing) rect
      return (wins, Just $ DC (Right $ fromMaybe sl sl') def)

    runLayout (Workspace tag (DC oldHS def) (Just newStack)) rect = do
      -- Update the hierarchical stack
      let (hs, delSLs) =
              case oldHS of
                Left oldHS'  -> update oldHS' newStack
                Right layout -> (Stack (newStack, layout) [] [], [])
      -- Release deleted columns
      mapM_ (flip handleMessage (SomeMessage ReleaseResources)) delSLs
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
      DCModifyHS f rr -> updateHS layout f rr
      DCBroadcast m   -> forwardMessage layout True m

-- | Apply a transformation to the current hierarchical stack,
-- updating the given layout state and writing the resulting flat
-- stack to the given 'IORef'.
--
-- To update the hierarchical stack from an action, use 'modifyHS'.
-- This will send a 'DCModifyHS' message containing a transformation
-- and the mutable reference for the reply, which will be passed to
-- this function in turn.  Note that this message is sent without
-- refreshing.  Upon return, 'modifyHS' updates the flat stack and
-- only then refreshes the window layout.  This round-about approach
-- is necessary because all changes to the stack must be made through
-- 'windows' or changes may be lost.
updateHS :: DynamicColumns sl Window              -- ^ Current layout state
         -> HSXForm                               -- ^ Transformation
         -> IORef (DCModifyResult (Stack Window)) -- ^ Result reference
         -> X (Maybe (DynamicColumns sl Window))
updateHS (DC (Right _) _)   _ replyRef = do
  io $ writeIORef replyRef ModifyUnchanged
  return Nothing
updateHS (DC (Left hs) def) f replyRef = do
  let hs' = let ?def = def in f hs
      dc' = DC (Left hs') def
      stack = flatten hs'
  io $ writeIORef replyRef (ModifyTo stack)
  -- XXX Could use a flag to indicate that the stack and the
  -- hierarchical stack are known to be in sync.
  return $ Just dc'

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

-- | Modify the hierarchical stack according to some transformation.
modifyHS :: HSXForm -> X ()
modifyHS = (`modifyHSOr` return ())

-- | Like 'modifyHS', but can be supplied an alternate action to
-- perform if the current layout is not dynamic columns.
modifyHSOr :: HSXForm -> X () -> X ()
modifyHSOr f alt = do
  w <- liftM (W.workspace . W.current) $ gets windowset
  replyRef <- io $ newIORef ModifyNotHandled
  sendMessageWithNoRefresh (DCModifyHS f replyRef) w
  reply <- io $ readIORef replyRef
  case reply of
    ModifyTo stack   -> windows (W.modify' (const stack))
    ModifyUnchanged  -> return ()
    ModifyNotHandled -> alt

-- | Move a stack's focus up one window, stopping at the edge.
focusUpHS :: Stack a -> Stack a
focusUpHS (Stack t (l:ls) rs) = Stack l ls (t:rs)
focusUpHS st = st

-- | Reversed 'focusUpHS'.
focusDownHS :: Stack a -> Stack a
focusDownHS = reversedHS focusUpHS

-- | Like 'focusUpHS', but cycle around at the edge.
focusUpCycleHS :: Stack a -> Stack a
focusUpCycleHS (Stack t [] rs) = Stack x xs [] where (x:xs) = reverse (t:rs)
focusUpCycleHS st = focusUpHS st

-- | Reversed 'focusUpCycleHS'.
focusDownCycleHS :: Stack a -> Stack a
focusDownCycleHS = reversedHS focusDownCycleHS

-- | Swap the focused window with the one up from it, stopping at the
-- edge.  Keeps the window focused.
swapUpHS :: Stack a -> Stack a
swapUpHS (Stack t (l:ls) rs) = Stack t ls (l:rs)
swapUpHS st = st

-- | Reversed 'swapUpHS'.
swapDownHS :: Stack a -> Stack a
swapDownHS = reversedHS swapUpHS

-- | Like 'swapUpHS', but cycle around at the edge.
swapUpCycleHS :: Stack a -> Stack a
swapUpCycleHS (Stack t [] rs) = Stack t (reverse rs) []
swapUpCycleHS st = swapUpHS st

-- | Reversed 'swapDownCycleHS'.
swapDownCycleHS :: Stack a -> Stack a
swapDownCycleHS = reversedHS swapUpCycleHS

-- XXX Variations on L/R window moving: Should focus stay with the
-- moving window or go to the next window in the stack?  Should a
-- window form a new stack in preference to moving into another stack?
-- There are some other options which are dictated by XMonad
-- standards, but could be done differently.  Should a window move in
-- above or below the focused window in a stack?  Should the focus in
-- the stack it left move up or down?

-- | Move the focused window from the focused column to the next
-- column up, constructing a new column if there is no column to the
-- left.  Keeps the window focused and moves the column focus
-- accordingly.
interMoveUpHS :: (?def :: b) => Stack (Stack a, b) -> Stack (Stack a, b)
interMoveUpHS hs =
    let def = ?def
        win = focus $ fst $ focus hs
        (focus':up') =
            case up hs of
              [] -> [(Stack win [] [], def)]
              (Stack st sls srs, slay):ss ->
                  (Stack win sls (st:srs), slay):ss
        down' =
            case focus hs of
              (Stack _ [] [],        _)    -> down hs
              (Stack _ (sl:sls) [],  slay) -> (Stack sl sls [], slay) : down hs
              (Stack _ sls (sr:srs), slay) -> (Stack sr sls srs, slay) : down hs
    in Stack focus' up' down'

-- | Reversed 'interMoveUpHS'.
interMoveDownHS :: (?def :: b) => Stack (Stack a, b) -> Stack (Stack a, b)
interMoveDownHS = reversedHS interMoveUpHS

-- | Transform a function that manipulates a stack into a function
-- that manipulates the focused stack of a hierarchical stack.
intraHS :: (Stack a -> Stack a) -> (Stack (Stack a, b) -> Stack (Stack a, b))
intraHS func (Stack (t, arg) ls rs) = Stack (func t, arg) ls rs

-- | Transform a function that manipulates a stack into a function
-- that manipulates the reversed stack.
reversedHS :: (Stack a -> Stack a) -> (Stack a -> Stack a)
reversedHS func (Stack t ls rs) =
    let (Stack t' rs' ls') = func (Stack t rs ls)
    in Stack t' ls' rs'

-- | Flatten a hierarchical stack, retaining its order and primary
-- focus.
flatten :: Stack (Stack a, b) -> Stack a
flatten (Stack (Stack t tls trs, _) ls rs) =
    -- XXX Could be much more efficient.
    let ls' = tls ++ reverse (concatMap (W.integrate . fst) (reverse ls))
        rs' = trs ++ concatMap (W.integrate . fst) rs
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
       -> (Stack (Stack a, b), [b])
          -- ^ Updated hierarchical stack and arguments of deleted
          -- sub-stacks
update oldStack newStack =
    let oldSet   = S.fromList (concatMap (W.integrate . fst) $ W.integrate oldStack)
        newCur   = stackToCursor newStack
        newSet   = S.fromList (fst newCur)
        deleted  = oldSet S.\\ newSet
        inserted = newSet S.\\ oldSet
        -- Like upStack, but thread an argument through, list-convert
        -- the Maybe result, and use the auxiliary list to track
        -- arguments of deleted sub-stacks
        upStackArg func final new (old, arg) =
            case upStack func final new old of
              (new', Nothing, _) -> (new', [], [arg])
              (new', Just st, _) -> (new', [(st, arg)], [])
        (_, Just hst, delSLs) = (upStack (upStackArg (upElem deleted inserted)))
                                True newCur oldStack
    in (hst, delSLs)

-- | A cursor into a stack.  This allows us to represent any suffix of
-- a stack, making them more amenable to traversal.  The first of the
-- pair is some suffix of the integration of the stack.  The second of
-- the pair is the index into the list of the focused element, and may
-- be negative if the cursor is in the \"down\" part of the stack.
type StCur a = ([a], Int)

-- | /O(|up|)/.  Convert a stack to a cursor.
stackToCursor :: Stack a -> StCur a
stackToCursor st = (W.integrate st, length (up st))

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
-- or, failing that, the last element of the resulting stack.
-- Likewise, concatenate the returned auxiliary lists.  Returns the
-- unconsumed part of /new/, the resulting stack or Nothing if the
-- function never returned any results, and the concatenated auxiliary
-- lists.
--
-- Note that upStack with one curried argument is almost, but not
-- quite suitable for passing as the function to another upStack.  The
-- type given here keeps things tighter.  In order to make it
-- suitable, apply 'Data.Maybe.listToMaybe' to the second part of the
-- result.  If this becomes a problem, we could generalize the return
-- type of /func/ to any 'Data.Foldable.Foldable'.
upStack :: (Bool -> StCur a -> b -> (StCur a, [c], [aux])) -- ^ /func/
        -> Bool    -- ^ If True, pass True to /func/ for the final
                   --   element of /old/.
        -> StCur a -- ^ /new/
        -> Stack b -- ^ /old/
        -> (StCur a, Maybe (Stack c), [aux])
upStack func final new old =
    let join newCur ([], _) = (newCur, ([], -1), [], False)
        join newCur ((o:os), oldIdx) =
            let final' = final && null os
                (newCur', outFrag, aux) = func final' newCur o
                (newEnd, restCur, restAux, lockFocus) = join newCur' (os, oldIdx - 1)
                (out, outFocus) = curPrepend outFrag restCur
                (outFocus', lockFocus')
                    | lockFocus                        = (outFocus, lockFocus)
                    -- XXX Without the next clause, deleting b from
                    -- [<[a,<b>]>,[<c>]] fails
                    | null out                         = (-1,       lockFocus)
                    | cursStraddleFocus newCur newCur' = (0,        True)
                    | oldIdx == 0                      = (0,        False)
                    | otherwise                        = (outFocus, lockFocus)
            in (newEnd, (out, outFocus'), aux ++ restAux, lockFocus')
        (new', outCur, allAux, _) = join new (stackToCursor old)
    in (new', cursorToStack outCur, allAux)

-- | Update a single element from a stack, accounting for deleted and
-- inserted elements.  This is meant to be used as the leaf function
-- to 'upStack'.  If the given element is in the deleted set, consumes
-- nothing and returns no elements.  Otherwise, consumes elements from
-- the cursor as long as they're in the inserted set, plus one more
-- (if possible), and returns the new cursor plus the list of consumed
-- elements.  Always returns an empty auxiliary list.
upElem :: Ord a =>
          S.Set a -- ^ Deleted set
       -> S.Set a -- ^ Inserted set
       -> Bool    -- ^ If True, consume all remaining elements
       -> StCur a -- ^ Cursor from which to consume elements
       -> a       -- ^ Element from the old stack to update
       -> (StCur a, [a], [b])
upElem _       _        True  new _   =
    let (cur', rest) = curTakeAll new
    in  (cur', rest, [])
upElem deleted inserted False new old
    | old `S.member` deleted = (new, [], [])
    | otherwise =
        -- Consume as long as the element is in inserted, plus one more
        let consume cur@([], _) = (cur, [], [])
            consume ((x:xs), idx)
                | x `S.member` inserted =
                    let (lastCur, xs', _) = consume nextCur
                    in (lastCur, x:xs', [])
                | otherwise = (nextCur, [x], [])
                where nextCur = (xs, idx - 1)
        in consume new

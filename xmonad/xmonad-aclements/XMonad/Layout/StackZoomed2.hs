{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances,
    TypeSynonymInstances, PatternGuards, ParallelListComp #-}
-- XXX ParallelListComp

-- Problems
--
-- It feels like new windows should be zoomed, but how do you
-- distinguish a new window from one that simply came from another
-- layout?

module XMonad.Layout.StackZoomed2 where

import XMonad
import XMonad.StackSet
import XMonad.Layout.TitleBarBuilder

import Data.List (elemIndex)

data StackZoomed tbs a
    = StackZoomed {
        szZoom   :: Maybe a
      , szWeight :: Double
      , szDelta  :: Double
      , szTBB    :: TitleBarBuilder tbs
      }
      deriving (Show, Read)

data SZMessage
    = SZExpand
    | SZContract
    | SZSetWeight Double
    | SZZoom Window
      deriving (Typeable)
instance Message SZMessage

instance (TitleBarStyle tbs, Show tbs, Read tbs)
    => LayoutClass (StackZoomed tbs) Window where
    emptyLayout l _ = addTitles l [] Nothing

    doLayout l r ws = addTitles l (pureLayout l r ws) (Just ws)

    pureLayout (StackZoomed zoom' weight _ tbb) r st@(Stack _ ls _)
        | Just zoom <- zoom', Just idx <- elemIndex zoom ws =
            stackWindows idx ws weight tbb r
        | otherwise =
            stackWindows (length ls) ws 1.0 tbb r
        where ws = integrate st

    handleMessage l@(StackZoomed zoom weight delta tbb) msg =
        do tbb' <- titleBarHandleMessage tbb msg
           case tbb' of
             Just tbb -> return $ Just $ StackZoomed zoom weight delta tbb
             Nothing  -> return $ pureMessage l msg

    pureMessage (StackZoomed zoom weight delta tbb) msg
        | Just m <- fromMessage msg = szMessage m
        | otherwise                 = Nothing
        where
          szMessage SZExpand        = set $ weight + delta
          szMessage SZContract      = set $ weight - delta
          szMessage (SZSetWeight w) = set w
          szMessage (SZZoom w)      = Just $ StackZoomed (Just w) weight delta tbb
          set w' = Just $ StackZoomed zoom (max w' 1.0) delta tbb

    description _ = "Zoom Stack"

stackWindows :: TitleBarStyle tbs => Int -> [a] -> Double -> TitleBarBuilder tbs -> Rectangle -> [(a, Rectangle)]
stackWindows idx ws weight tbb r =
    let wL = fromIntegral idx
        wC = max weight 1.0
        idx' = length ws - idx - 1
        wR = fromIntegral idx'
        (left, notLeft) = splitVerticallyBy (wL/(wL + wC + wR)) r
        (center, right) = splitVerticallyBy (wC/(wC + wR)) notLeft
        -- Divide the regions up for the windows
        rLs = splitVertically idx  left
        rRs = splitVertically idx' right
        -- Divide the windows up
        (winLs, zoom:winRs) = splitAt idx ws
        -- Shrink rectangles for title bars
        tbh = titleBarHeight tbb (TitleSpec defWindowStyle)
        shrink = shrinkRect tbh
        (center', rLs', rRs') = (shrink center, map shrink rLs, map shrink rRs)
    in (zoom, center') : (zip winLs rLs' ++ zip winRs rRs')

addTitles :: TitleBarStyle tbs => StackZoomed tbs Window -> [(Window, Rectangle)] -> Maybe (Stack Window) -> X ([(Window, Rectangle)], Maybe (StackZoomed tbs Window))
addTitles l rs (Just (Stack f _ _)) = do
  primaryFocus <- withWindowSet $ \ws -> return $ peek ws
  let secondary = case primaryFocus of
                    Nothing -> True
                    Just pf -> not (pf `elem` map fst rs)
  (rs', tbb') <- titleBarLayout (szTBB l) [(win, r, TitleSpec (defWindowStyle {wsActive=(win==f), wsSecondary=secondary})) | (win, r) <- rs]
  case tbb' of
    Just tbb -> return (rs', Just $ l {szTBB = tbb})
    Nothing  -> return (rs', Nothing)
addTitles l [] Nothing = do
  (rs', tbb') <- titleBarLayout (szTBB l) []
  case tbb' of
    Just tbb -> return (rs', Just $ l {szTBB = tbb})
    Nothing  -> return (rs', Nothing)

szZoomThis :: X ()
szZoomThis = withFocused (sendMessage . SZZoom)

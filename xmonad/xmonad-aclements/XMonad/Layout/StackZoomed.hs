{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances,
    TypeSynonymInstances, PatternGuards #-}

-- Problems
-- * It feels like new windows should be zoomed, but how do you
--   distinguish a new window from one that simply came from another
--   layout?

module XMonad.Layout.StackZoomed where

import XMonad
import XMonad.StackSet

import Data.List (elemIndex)

data StackZoomed a
    = StackZoomed (Maybe a) Double Double
      deriving (Show, Read)

data SZMessage
    = SZExpand
    | SZContract
    | SZSetWeight Double
    | SZZoom Window
      deriving (Typeable)
instance Message SZMessage

instance LayoutClass StackZoomed Window where
    pureLayout (StackZoomed zoom' weight _) r st@(Stack _ ls _)
        | Just zoom <- zoom', Just idx <- elemIndex zoom ws =
            stackWindows idx ws weight r
        | otherwise =
            stackWindows (length ls) ws 1.0 r
        where ws = integrate st

    pureMessage (StackZoomed zoom weight delta) msg
        | Just m <- fromMessage msg = szMessage m
        | otherwise                 = Nothing
        where
          szMessage SZExpand        = set $ weight + delta
          szMessage SZContract      = set $ weight - delta
          szMessage (SZSetWeight w) = set w
          szMessage (SZZoom w)      = Just $ StackZoomed (Just w) weight delta
          set w' = Just $ StackZoomed zoom (max w' 1.0) delta

    description _ = "Zoom Stack"

stackWindows :: Int -> [a] -> Double -> Rectangle -> [(a, Rectangle)]
stackWindows idx ws weight r =
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
    in (zoom, center) : (zip winLs rLs ++ zip winRs rRs)

szZoomThis :: X ()
szZoomThis = withFocused (sendMessage . SZZoom)

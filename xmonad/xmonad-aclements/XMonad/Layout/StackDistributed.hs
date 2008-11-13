{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances,
    PatternGuards #-}

-- StackSpread?  StackWeighted?  StackWeightedFocus?

module XMonad.Layout.StackDistributed where

import XMonad
import XMonad.StackSet

data StackDistributed a
    = StackDistributed Double Double
      deriving (Show, Read)

data SDMessage
    = SDExpand
    | SDContract
    | SDSetWeight Double
      deriving (Typeable)
instance Message SDMessage

instance LayoutClass StackDistributed a where
    pureLayout (StackDistributed weight _) r (Stack t ls rs) =
        let -- Compute the left, center, and right regions
            wL = fromIntegral $ length ls
            wC = max weight 1.0
            wR = fromIntegral $ length rs
            (left, notLeft) = splitVerticallyBy (wL/(wL + wC + wR)) r
            (center, right) = splitVerticallyBy (wC/(wC + wR)) notLeft
            -- Divide the regions up for the windows
            lefts  = splitVertically (length ls) left
            rights = splitVertically (length rs) right
        in (t, center) : (zip ls (reverse lefts) ++ zip rs rights)

    pureMessage (StackDistributed weight delta) msg
        | Just m <- fromMessage msg = sdMessage m
        | otherwise                 = Nothing
        where
          sdMessage SDExpand        = set $ weight + delta
          sdMessage SDContract      = set $ weight - delta
          sdMessage (SDSetWeight w) = set w
          set w' = Just $ StackDistributed (max w' 1.0) delta

    description _ = "Distributed Stack"

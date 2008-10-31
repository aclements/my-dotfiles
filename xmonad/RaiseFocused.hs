{-# LANGUAGE TypeSynonymInstances,MultiParamTypeClasses #-}

module RaiseFocused(raiseFocused) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Layout.LayoutModifier

import Data.List (break)

data RaiseFocused a = RaiseFocused deriving (Show, Read)

instance LayoutModifier RaiseFocused Window where
    pureModifier _ _ st layout =
        case break ((== W.focus st) . fst) layout of
          (all, [])       -> (layout, Nothing)
          (pre, cur:post) -> (cur:(pre++post), Nothing)

raiseFocused :: l a -> ModifiedLayout RaiseFocused l a
raiseFocused = ModifiedLayout RaiseFocused

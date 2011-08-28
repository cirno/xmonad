{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns #-}

import XMonad

import XMonad.StackSet as S

import XMonad.Layout.Named
import XMonad.Layout.Decoration
import XMonad.Layout.NoBorders

import Data.Ratio ((%))
import System

import Graphics.X11 (Rectangle)

-- Simple Wmii-like Stacked layout
data Stacked a = Stacked Int deriving (Read, Show)

instance LayoutClass Stacked Window where
    pureLayout (Stacked indent) sc@(Rectangle _ _ _ (fi -> h)) ws =
        zip ups tops ++ [(S.focus ws, mainPane)] ++ zip dns bottoms
      where
        ups = reverse $ S.up ws
        dns = S.down ws

        -- Top/bottom stack length
        tsl = stackLen ups
        bsl = stackLen dns

        stackLen = (* indent) . fi . length

        (top,  allButTop) = splitVerticallyBy (tsl % h) sc
        (center,  bottom) = splitVerticallyBy ((h - tsl - bsl) % (h - tsl)) allButTop
        (allButBottom, _) = splitVerticallyBy ((h - bsl) % h) sc

        mainPane | null ups && null dns = sc
                 | null ups             = allButBottom
                 | null dns             = allButTop
                 | otherwise            = center

        tops    = split ups top
        bottoms = split dns bottom
        
        split []              _    = []
        split (length -> num) part = map infExpand $ splitVertically num part
       
        infExpand (Rectangle x y ww _) = Rectangle x y ww (-1)

-- Simple decorations for Stacked layout
data StackedDecoration a = StackedDecoration deriving (Show, Read)

instance Eq a => DecorationStyle StackedDecoration a where
    describeDeco _ = "Simple decoration for Stacked layout"

    shrink _ (Rectangle _ _ _ dh) (Rectangle x y w (-1)) = Rectangle x y w 1
    shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h)    = Rectangle x (y + fi dh) w (h - dh)

stackedDeco s t@(fi . decoHeight -> dh) =
    named "Stacked" $ decoration s t StackedDecoration (Stacked dh)

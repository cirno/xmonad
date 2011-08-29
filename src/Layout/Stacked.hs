{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, ViewPatterns #-}

import XMonad

import XMonad.StackSet as S

import XMonad.Layout.Named
import XMonad.Layout.Decoration

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
       
        infExpand (Rectangle x y ww _) = Rectangle x y ww 1

-- Simple decoration for Stacked layout
data SimpleDecoration a = SimpleDecoration deriving (Show, Read)

instance Eq a => DecorationStyle SimpleDecoration a where
    describeDeco _ = "Simple decoration"

    shrink _ (Rectangle _ _ _ dh) r@(Rectangle x y w h)
        | h == 1    = r
        | otherwise = Rectangle x (y + fi dh) w (h - dh)

stackedDeco s t@(fi . decoHeight -> dh) =
    named "Stacked" $ decoration s t SimpleDecoration (Stacked dh)

simpleDeco s t = named "SimpleDeco" $ decoration s t SimpleDecoration

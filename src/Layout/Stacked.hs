{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances ViewPatterns #-}

import XMonad

import XMonad.StackSet as W

import XMonad.Layout.Named
import XMonad.Layout.Decoration
import XMonad.Layout.NoBorders

import Data.Ratio ((%))
import System

import Graphics.X11 (Rectangle)

-- Simple Wmii-like Stacked layout
data Stacked a = Stacked Int deriving (Read, Show)

instance LayoutClass Stacked Window where
    pureLayout (Stacked indent) sc@(Rectangle _ _ _ height) ws =
        zip ups tops ++ [(W.focus ws, mainPane)] ++ zip dns bottoms
      where
        h = fromIntegral height :: Int
        ups = reverse $ W.up ws
        dns = W.down ws

        -- Top/bottom stack length
        tsl = indent * (fromIntegral $ length ups)
        bsl = indent * (fromIntegral $ length dns)

        (top,  allButTop) = splitVerticallyBy (tsl % h) sc
        (center,  bottom) = splitVerticallyBy ((h - tsl - bsl) % (h - tsl)) allButTop
        (allButBottom, _) = splitVerticallyBy ((h - bsl) % h) sc

        mainPane | ups /= [] && dns /= [] = center
                 | ups /= []              = allButTop
                 | dns /= []              = allButBottom
                 | otherwise              = sc
        tops    = if ups /= [] then splitVertically (length ups) top    else []
        bottoms = if dns /= [] then splitVertically (length dns) bottom else []

-- Simple borderless decorations for Stacked layout
data StackedDecoration a = StackedDecoration Bool deriving (Show, Read)

instance Eq a => DecorationStyle StackedDecoration a where
    describeDeco _ = "Simple borderless decoration for Stacked layout"

    shrink _ (Rectangle _ _ _ (pred -> dh)) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

stackedDeco s t@(Stacked . fi . decoHeight -> l) =
    named "Stacked" . noBorders $ decoration s t StackedDecoration l

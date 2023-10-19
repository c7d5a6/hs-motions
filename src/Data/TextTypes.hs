module Data.TextTypes where

data Point = Point {col :: Int} deriving (Eq)
instance Show Point where
    show = show . col

type Text = String

type MotionAction = String
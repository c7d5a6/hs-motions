module Data.TextTypes where

data Point = Point {col :: Int} deriving (Eq, Show)

type Text = String

type MotionAction = String
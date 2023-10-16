module Data.TextTypes
    ( Point, Text, MotionAction
    ) where

newtype Point = Point {col :: Int} deriving (Eq)

type Text = String

type MotionAction = String
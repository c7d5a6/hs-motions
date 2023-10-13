module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Point = Point {col :: Int}
type Text = String
type MotionAction = String

printMotions :: Point -> Point -> Text -> String
printMotions coursor goal text = foldl (<>) "" $ fmap printMotionAction $ findMotions coursor goal text

printMotionAction :: MotionAction -> String
printMotionAction = id

findMotions :: Point -> Point -> Text -> [MotionAction]
findMotions _ _ _ = []
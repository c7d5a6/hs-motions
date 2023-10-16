module Lib
    ( someFunc,
    findMotions,
    aStar
    ) where

import Data.TextTypes
-- | I am the 'foo' function's Haddock pre-comment.
{-          +--> some info
            |
            + -}
someFunc :: IO ()
someFunc = putStrLn "someFunc"

printMotions :: Point -> Point -> Text -> String -- ^ The answer
printMotions coursor goal text = foldl (<>) "" (printMotionAction <$> findMotions coursor goal text)

printMotionAction :: MotionAction -> String
printMotionAction = id

-- | This is findMotions function
findMotions :: Point -- ^ Initial point
            -> Point -- ^ Goal Point
            -> Text -> [MotionAction]
findMotions _ _ _ = []

aStar :: Point -- ^ Initial point
      -> Point -- ^ Goal point
      -> (Point -> Point -> Int) -- ^ h(x) function of cost from point to point
      -> (Point -> Point -> Int) -- ^ g(x) function of cost to move from point to point
      -> Text -- ^ Text used for navigation
      -> (Point -> [Point]) -- ^ neighbours generator
      -> [MotionAction] -- ^ Route to navigate to goal
aStar _ _ _ _ _ _ = []

astar :: (Point -> Bool) -- Is point a goal
       -> [Point] -- Next to check
       -> [Point] -- Visited
       -> (Point -> [Point]) -- neighbours generator
       -> Maybe [Point] -- ^ Route to navigate to goal
astar _ [] _ _ = Nothing
astar isGoal (p:xs) seen getNext
    | isGoal p = Just [p]
    | p `elem` seen = astar isGoal xs seen getNext
    | otherwise = astar isGoal xs' (p:seen) getNext
    where
        xs' = xs <> getNext p
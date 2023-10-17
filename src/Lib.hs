module Lib
    ( someFunc,
    findMotions,
    aStar,
    astar,
    isGoal10,
    lenToGoal,
    generateNextN
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

astar' :: (Point -> Bool) -- Is point a goal
       -> (Point -> Int) -- heuristic function
       -> [(Point, Int)] -- Next to check
       -> [Point] -- Visited
       -> ((Point, Int) -> [(Point, Int)]) -- neighbours generator
       -> Maybe [Point] -- ^ Route to navigate to goal
astar' _ _ [] _ _ = Nothing
astar' isGoal h next seen getNext
    | isGoal p = Just [p]
    | p `elem` seen = astar' isGoal h nextTail seen getNext
    | otherwise = fmap (p :) (astar' isGoal h next' (p:seen) getNext)
    where
        nextPoint' [x] = x
        nextPoint' list = foldr1 minPair list
        nextPoint = nextPoint' next
        p = fst nextPoint
        nextTail = removeItem nextPoint next
        minPair (x1,y1) (x2,y2) = if (h x1 + y1)<(h x2 + y2) then (x1,y1) else (x2,y2)
        next' = nextTail <> getNext nextPoint
        removeItem _ []                 = []
        removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys

generateNextN (p,f) = [(Point {col=col p-1},f+1),(Point {col=col p+1}, f+1)]
isGoal10 p = col p == 10
lenToGoal p = abs $ col p - 10
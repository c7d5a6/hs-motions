module Lib
  ( someFunc,
    findMotions,
    aStar,
    astar',
    astar'',
    astarBench,
    astarBench2,
    generateNextN,
  )
where

import Data.TextTypes
import Data.List (foldl')
import qualified Data.PQueue.Prio.Min as PQ

-- | I am the 'foo' function's Haddock pre-comment.

{-          +--> some info
            |
            + -}
someFunc :: IO ()
someFunc = putStrLn "someFunc"

printMotions ::
  Point ->
  Point ->
  Text ->
  -- | The answer
  String
printMotions coursor goal text = foldl (<>) "" (printMotionAction <$> findMotions coursor goal text)

printMotionAction :: MotionAction -> String
printMotionAction = id

-- | This is findMotions function
findMotions ::
  -- | Initial point
  Point ->
  -- | Goal Point
  Point ->
  Text ->
  [MotionAction]
findMotions _ _ _ = []

aStar ::
  -- | Initial point
  Point ->
  -- | Goal point
  Point ->
  -- | h(x) function of cost from point to point
  (Point -> Point -> Int) ->
  -- | g(x) function of cost to move from point to point
  (Point -> Point -> Int) ->
  -- | Text used for navigation
  Text ->
  -- | neighbours generator
  (Point -> [Point]) ->
  -- | Route to navigate to goal
  [MotionAction]
aStar _ _ _ _ _ _ = []

astar' ::
  (Point -> Bool) -> -- Is point a goal
  (Point -> Int) -> -- heuristic function
  PQ.MinPQueue Int Point -> -- Next to check
  [Point] -> -- Visited
  ((Point, Int) -> [(Point, Int)]) -> -- neighbours generator
  -- | Route to navigate to goal
  Maybe [Point]
astar' isGoal h next seen getNext
  | PQ.null next = Nothing
  | isGoal p = Just [p]
  | p `elem` seen = astar' isGoal h next' seen getNext
  | otherwise = fmap (p :) (astar' isGoal h next' (p : seen) getNext)
  where
    nextPoint = PQ.findMin next
    nextwithout = PQ.deleteMin next    
    p = snd nextPoint
    neighbours = getNext (snd nextPoint, fst nextPoint)
    next' = foldl' (\q (pn,f) -> PQ.insert (f-h p + h pn) pn q) nextwithout (filter (\(pn,f)->pn `notElem` seen) neighbours)

astar'' ::
  (Point -> Bool) -> -- Is point a goal
  (Point -> Int) -> -- heuristic function
  PQ.MinPQueue Int Point -> -- Next to check
  [Point] -> -- Visited
  ((Point, Int) -> [(Point, Int)]) -> -- neighbours generator
  -- | Route to navigate to goal
  Maybe [Point]
astar'' isGoal h next seen getNext
  | PQ.null next = Nothing
  | isGoal p = Just [p]
  | p `elem` seen = astar'' isGoal h next' seen getNext
  | otherwise = fmap (p :) (astar'' isGoal h next' (p : seen) getNext)
  where
    (nextPoint,nextwithout) = PQ.deleteFindMin next
    p = snd nextPoint
    neighbours = filter (\(pn,f)->pn `notElem` seen) $ getNext (snd nextPoint, fst nextPoint)
    filterN' i pn = fmap ((>) i) (lookup pn neighbours)
    filterN  i pn = maybe True id (filterN' i pn)
    nextwithout' = PQ.filterWithKey filterN nextwithout
    -- next'' = PQ.fromList (map swap neighbours)
    next'' = foldl' (\q (pn,f) -> PQ.insert (f-h p + h pn) pn q) PQ.empty neighbours
    next' = PQ.union nextwithout' next''
    

astarBench x = astar' isGoalX lenToGoalX (PQ.singleton (lenToGoalX start) start) [] generateNextN
  where 
        start = Point {col=0}
        isGoalX p = col p == x
        lenToGoalX p = abs $ col p - x   

astarBench2 x = astar'' isGoalX lenToGoalX (PQ.singleton (lenToGoalX start) start) [] generateNextN
  where 
        start = Point {col=0}
        isGoalX p = col p == x
        lenToGoalX p = abs $ col p - x        

generateNextN (p, f) = [(Point {col = col p - 1}, f + 1)
 , (Point {col = col p - 2}, f + 1)
 , (Point {col = col p `div` 5}, f + 1)
 , (Point {col = if col p == 0 then 1 else if even (col p) then col p `div` 2 else 3*(col p)+1}, f + 1)]

swap (a, b) = (b, a)
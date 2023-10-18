import Criterion.Main
import Lib

-- Our benchmark harness.
main =
  defaultMain
    [ bgroup
        "astar/elem"
        [ 
          bench "0" $ whnf astarBench 0,
          bench "1" $ whnf astarBench 1,
          bench "10" $ whnf astarBench 10,
          bench "100" $ whnf astarBench 100,
          bench "1000" $ whnf astarBench 1000
        ],
      bgroup
        "astar/PQ"
        [ 
          bench "0" $ whnf astarBench2 0,
          bench "1" $ whnf astarBench2 1,
          bench "10" $ whnf astarBench2 10,
          bench "100" $ whnf astarBench2 100,
          bench "1000" $ whnf astarBench2 1000
        ]
    ]

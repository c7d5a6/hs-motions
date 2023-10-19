import Criterion.Main
import Lib

-- Our benchmark harness.
main =
  defaultMain
    [ bgroup
        "astar'"
        [ 
          bench "0" $ whnf astarBench 0,
          bench "10" $ whnf astarBench 10,
          bench "100" $ whnf astarBench 100,
          bench "500" $ whnf astarBench 500
        ],
      bgroup
        "astar''"
        [ 
          bench "0" $ whnf astarBench2 0,
          bench "10" $ whnf astarBench2 10,
          bench "100" $ whnf astarBench2 100,
          bench "500" $ whnf astarBench2 500,
          bench "1000" $ whnf astarBench2 16000
        ]
    ]

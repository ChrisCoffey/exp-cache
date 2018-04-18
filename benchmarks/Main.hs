import Benchmarks.LRU
import Benchmarks.RR

import Criterion.Main



main = defaultMain [
    lruBenchmarks,
    rrBenchmarks
    ]

import Benchmarks.LRU
import Benchmarks.RR
import Benchmarks.LFU

import Criterion.Main



main = defaultMain [
    lruBenchmarks,
    rrBenchmarks,
    lfuBenchmarks
    ]

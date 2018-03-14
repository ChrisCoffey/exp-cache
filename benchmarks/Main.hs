import Benchmarks.LRU

import Criterion.Main



main = defaultMain [
    lruBenchmarks
    ]

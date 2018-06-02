# Expressive Caching

[![Build Status](https://travis-ci.org/ChrisCoffey/Cache-Eviction-Strategies.svg?branch=master)](https://travis-ci.org/ChrisCoffey/Cache-Eviction-Strategies)

Calling this library "Expressive Caching" perhaps implies <Fill this in>. So why does this library exist? Basically, I wanted to explore how different caching strategies behave under simliar workloads. While its generally true that given enough time and effort you can think through how a particular workload will behave, its usually simpler to just try a few represenative samples. This library provides the ability to swap out the expiration strategy for a given cache with a single line of code.
Traditionally you'll find an LRU or MRU cache library with all of the expiration logic inlined with the retrival logic. Admittedly, this is more memory efficient, but since caches are all about trading memory for CPU cycles anyways, I decided to separate the expiration logic from the retrival logic. This accomplished my initial goal of making it easy to play the same workload across different strategies, but it also had the hidden benefit of making it easier to implement each eviction strategy.

The library currently supports:
- FIFO : a queue
- LRU  : Least Recently Used *TODO add a link
- MRU  : Most Recently Used *TODO add a link
- LFU  : Least Frequently Used *TODO add a link
- RR   : Random Replacement *TODO add a link

Its trivial to define alternative eviction strategies using the `EvictionStrategy` class.



# Performance and Optimization

The code from the talk regarding tips & tricks for performance and optimization by thinking about data first. The talk
was not recorded and does not contain slides.

To test the different piece of code you may use commands such as 

```sh
perf stat                                                                          \
    -e L1-icache-loads,L1-icache-load-misses,L1-dcache-loads,L1-dcache-load-misses \
    ./simple_loop                                                                  \
    --benchmark_filter=normal                                                      \
    --benchmark_min_time=2                                                         \
```

To get general cache related stats from the different examples which may be filtered using the `--benchmark_filter`
(regex based filtering) option.

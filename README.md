# Metrics

[![Hackage](https://budueba.com/hackage/metrics)](https://hackage.haskell.org/package/metrics)
![License](https://img.shields.io/github/license/iand675/metrics.svg?style=flat)
[![Circle CI](https://circleci.com/gh/iand675/metrics/tree/master.svg?style=svg)](https://circleci.com/gh/iand675/metrics/tree/master)

## Metrics is a port of the eponymous [Java library](https://dropwizard.github.io/metrics/3.1.0/) to help you understand what your code is doing in production.

Metrics provides a set of measurement tools for various scenarios:

* Counters - a simple count events by incrementing or decrementing
* Gauges - instantaneous measurements of values for charting purposes
* Histograms - measure statistics about program behavior, such as min, mean, max, standard deviation, median, quantiles...
* Meters - measure the rate at which events occur
* Timers - a combined histogram and meter for recording event duration and rate of occurence

## Contributing

PRs are welcome!
Issues are located in the GitHub issue tracker.

Areas that could use contributions:

* Performance improvements for metrics under high contention
* Any area that's missing parity with the Java library & makes sense for Haskell
* Examples in the docs
* More reporters! Would pretty much auto-merge support for:
  - [ ] StatsD
  - [ ] Riemann
  - [ ] Librato
  - [ ] Graphite
  - [ ] Datadog
  - [ ] InfluxDB
  - [ ] ... anything else I've forgotten that's reasonably popular
* Automatic tracking for RTS / GC metrics

## License

Copyright (c) 2013-2015 Ian Duncan

Published under MIT License, see LICENSE

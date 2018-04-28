stopifnot(require("microbenchmark"))
# Benchmark [.xts using ISO8601 range on large objects
N <- 2e7
secPerYr <- 86400*365
x <- xts::.xts(1:N, 1.0*seq(secPerYr*20, secPerYr*40, length.out = N))
rng <- "1990/2000"

# warmup, in case there's any JIT
for (i in 1:2) {
  x[rng,]
}

profile <- FALSE
if (profile) {
  # Use loop if profiling, so microbenchmark calls aren't included
  Rprof(line.profiling = TRUE)
  for(i in 1:10) {
    x[rng,]
  }
  Rprof(NULL)
  print(srp <- summaryRprof())
} else {
  microbenchmark(x[rng,], times = 10)
}

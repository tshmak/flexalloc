waterfill <- function(water, capacity) {
  # water <- 10; capacity <- c(1,2,2,4,5)
  x <- capacity * 0
  v <- unique(capacity)
  vs <- sort(v)
  stopifnot(water <= sum(capacity))
  steps <- vs - c(0,vs[-length(vs)])
  for(i in 1:length(vs)) {
    # i <- 1
    vss <- vs[i]
    s <- steps[i]
    tofill <- capacity >= vss
    n <- sum(tofill)
    total <- n * s
    remaining <- water - sum(x)
    if(remaining < total) {
      x[tofill] <- x[tofill] + remaining / n
      break
    } else {
      x[tofill] <- x[tofill] + s
    }
  }
  return(x)
}


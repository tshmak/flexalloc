MyersonPay1 <- function(b, alloc.fun, ..., f=dunif, F=punif, bmax=1,
                       subset=1:length(b), interpolate=FALSE,
                       npoints=200) {

  #' @description This obtains Myerson's Pay using integration by the *rectangle* rule.
  #' Using fixed quadrature, it ensures that it is monotonic with respect to
  #' b. However, the allocation vector x is based on the interpolated x, rather
  #' than the true x calculated from alloc.fun.

  n <- length(b)
  stopifnot(b <= bmax)
  x0i <- int1 <- int2 <- b*NA
  v <- delta(b, f=f, F=F)
  x0 <- alloc.fun(v, ...)

  if(interpolate) {
    # Get the max and min x first
    i_min <- which(x0 == min(x0))[1]
    i_max <- which(x0 == max(x0))[1]
    subset <- union(subset, c(i_min, i_max))
  }

  #' I'm using fixed quadrature points to ensure that the integration is
  #' monotonic with respect to the lower limit.
  qpoints <- seq(0, bmax, length=npoints)
  d <- qpoints[2] - qpoints[1]
  boundaries <- c(0, seq(min(qpoints) + d/2, max(qpoints), by=d))

  for(i in subset) {
    # i <- 1
    int1bin <- sum(b[i] >= boundaries)
    int2startbin <- int1bin + 1
    gt1 <- int1bin:npoints
    y <- qpoints[gt1]

    stopifnot(b[i] <= bmax)

    np <- length(y)
    x <- sapply(y, function(yi, b2) {
      b2[i] <- yi
      v2 <- delta(b2, f=f, F=F)
      x <- alloc.fun(v2, ...)
      return(x[i])
    }, b)
    # Integration by rectangle rule
    if(np >= 2) {
      x0i[i] <- x[1] # x0i[i]: Interpolated x
      int1[i] <- x0i[i] * boundaries[bin+1]
      int2[i] <- 0
      if(np > 2) {
        int2[i] <- sum(x[-1]) - 0.5*x[-length(x)]
      }
    } else {
      int2[i] <- 0
      x0i[i] <- x[1]
      int1[i] <- bmax * x[1]
    }
  }
  pay <- int1 + int2
  if(interpolate) {
    if(any(is.na(pay))) {
      ok <- !is.na(pay)
      # sf <- splinefun(x=x0[ok], y=pay[ok])
      sf <- monospline(x=x0[ok], y=pay[ok], bs="mpi")
      pay[!ok] <- sf(x0[!ok])
    }
  }
  # attr(pay, "int2") <- data.frame(x=x,y=y)
  attributes(pay) <- list(x0=x0, x0i=x0i)
  return(pay)
}


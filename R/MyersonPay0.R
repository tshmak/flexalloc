MyersonPay0 <- function(b, alloc.fun, ..., f=dunif, F=punif, bmax=1,
                       subset=1:length(b), interpolate=FALSE,
                       npoints=200) {

  #' @description This obtains Myerson's Pay using integration by the *rectangle* rule.
  #' Using fixed quadrature, it ensures that it is monotonic with respect to
  #' b. However, the allocation vector x is based on the interpolated x, rather
  #' than the true x calculated from alloc.fun.

  n <- length(b)
  stopifnot(b <= bmax & b >= 0)
  pay <- x0i <- b*NA
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

  #' The range [0,bmax] is divided into npoints bins. The width of each bin
  #' is centered at qpoints, and has width d, except for the first and last bin,
  #' which is bounded by 0 and bmax respectively and have width d/2.

  stopifnot(length(qpoints) == npoints) # sanity checks
  stopifnot(length(boundaries) == npoints)

  for(i in subset) {
    # i <- 1

    bin1 <- sum(b[i] >= boundaries) # The index of the first bin
    bins <- bin1:npoints # The indices of all the bins used in integration.

    b2 <- b # Make a copy first

    # Rectangle integration...
    for(j in 1:length(bins)) {
      bin <- bins[j]
      b2[i] <- qpoints[bin]
      v2 <- delta(b2, f = f, F=F)
      x2 <- alloc.fun(v2, ...)[i]
      if(j == 1) {
        pay[i] <- ((bins[1]-1)*d + d/2)*x2 # Half width for the first rectangle. Everything else full width.
        x0i[i] <- x2 # The interpolated x.
      } else {
        pay[i] <- pay[i] + d*x2
      }

      if(j == length(bins)) {
        pay[i] <- pay[i] - x2*d/2 # The last rectangle is only half width
      }
    }
  }

  if(interpolate) {
    if(any(is.na(pay))) {
      ok <- !is.na(pay)
      # sf <- splinefun(x=x0[ok], y=pay[ok])
      sf <- monospline(x=x0[ok], y=pay[ok], bs="mpi")
      pay[!ok] <- sf(x0[!ok])
    }
  }
  attributes(pay) <- list(x0=x0, x0i=x0i)
  return(pay)
}



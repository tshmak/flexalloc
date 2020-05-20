MyersonPay1 <- function(b, alloc.fun, ..., f=dunif, F=punif, bmax=1,
                       subset=1:length(b), interpolate=FALSE,
                       npoints=200) {

  #' @description This obtains Myerson's Pay using integration by the trapezium rule.
  #' Using fixed quadrature, it ensures that it is monotonic with respect to
  #' b. However, the allocation vector x is based on the interpolated x, rather
  #' than the true x calculated from alloc.fun. Moreover, this does not guarantee
  #' utility is monotonic with respect to maxx. To ensure that, we need to use
  #' MyersonPay0 instead.

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


  for(i in subset) {

    gt <- which(qpoints > b[i])
    if(length(gt) > 0) {
      gt1 <- c(min(gt)-1, gt)
      y <- qpoints[gt1]
    } else if(b[i] == bmax) {
      y <- bmax
    } else {
      stop(paste0("b[",i,"] is greater than bmax."))
    }
    np <- length(y)
    x <- sapply(y, function(yi, b2) {
      b2[i] <- yi
      v2 <- delta(b2, f=f, F=F)
      x <- alloc.fun(v2, ...)
      return(x[i])
    }, b)
    # Integration by trapezium rule
    if(np >= 2) {
      d <- y[2] - y[1]
      z <- (b[i] - y[1]) / d
      x0i[i] <- x[1]*(1-z) + x[2]*z # x0i[i]: Interpolated x
      int1[i] <- x0i[i] * b[i]
      int2[i] <- d*(1-z) * (x0i[i]+x[2])/2
      if(np > 2) {
        int2[i] <- int2[i] + d*(sum(x[-1]) - 0.5*(x[2]+x[np]))
      }
    } else {
      int2[i] <- 0
      x0i[i] <- x[1]
      int1[i] <- bmax * x[1]
    }
  }
  pay <- int1 + int2
  if(interpolate) {
    #' Note that interpolation is only a heuristic and is not mathematically sound
    #' In particular, the relationship between x and pay is not the same for 
    #' different b and x^max. 
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


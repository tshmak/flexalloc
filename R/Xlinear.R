Xlinear <- function(v, sumx=1, maxx=sumx, ..., round=FALSE) {
  n <- length(v)
  stopifnot(v > 0)
  stopifnot(maxx > 0)
  if(length(maxx) == 1) maxx <- rep(maxx, n)
  stopifnot(length(maxx) == n)
  stopifnot(sum(maxx) >= sumx)
  
  # n <- 10; v <- ceiling(runif(n) * 8); maxx <- ceiling(runif(n) * 2); sumx <- sum(maxx)*0.6
  xo <- rep(0, n)
  
  o <- order(v)
  oo <- order(o) # To reverse the ordering 
  
  vo <- v[o]
  maxxo <- maxx[o]
  
  ro <- rank(vo, ties.method = "min")
  tiedo <- ro == c(ro[-1], 0) | ro == c(0, ro[-length(ro)])
  cmaxxo <- cumsum(maxxo)
  
  within_quota <- cmaxxo <= sumx
  if(all(within_quota == FALSE)) {
    xo[1] <- sumx
  } else {
    within_quota_max <- max(which(within_quota))
    if(within_quota_max == n) {
      xo <- maxxo
    } else if(ro[within_quota_max] == ro[within_quota_max+1]) {
      full_assign <- ro < ro[within_quota_max]
      xo[full_assign] <- maxxo[full_assign]
      split_group <- ro == ro[within_quota_max]
      remaining <- sumx - sum(xo)
      xo[split_group] <- waterfill(remaining, maxxo[split_group])
    } else {
      full_assign <- ro <= ro[within_quota_max]
      xo[full_assign] <- maxxo[full_assign]
      remaining <- sumx - sum(xo)
      if(remaining > 0) {
        next_group_ro <- min(ro[(1:n) > within_quota_max])
        split_group <- ro == next_group_ro
        xo[split_group] <- waterfill(remaining, maxxo[split_group])
      }
    }
  }
  
  x <- xo[oo]
  return(x)
}


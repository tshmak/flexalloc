X1 <- function(v, sumx=1, maxx=sumx, k=2, round=FALSE) {
  #' This solves the allocation using quadratic programming and is slow.
  #' Use X2 instead, except for checking.


  library(quadprog)
  n <- length(v)
  stopifnot(v > 0)
  stopifnot(maxx > 0)
  v <- v^k
  if(length(maxx) == 1) maxx <- rep(maxx, n)
  stopifnot(length(maxx) == n)

  Dmat <- diag(v)
  dvec <- rep(0, n)
  Amat <- t(rbind(rep(1,n),
                  -diag(n)))
  bvec <- c(sumx, -maxx)

  s <- solve.QP(Dmat, dvec, Amat, bvec, meq=1)
  x <- s$solution

  if(round) x <- round(x)
  return(x)
}


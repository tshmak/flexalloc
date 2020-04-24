dlnorm2 <- function(..., max) {
  P <- plnorm(q=max, ...)
  return(dlnorm(...)/P)
}

plnorm2 <- function(..., max) {
  P <- plnorm(q=max, ...)
  return(plnorm(...)/P)
}

qlnorm2 <- function(p, ..., max) {
  P <- plnorm(q=max, ...)
  return(qlnorm(P*p, ...))
}

rlnorm2 <- function(n, ..., max) {
  u <- runif(n)
  return(qlnorm2(u, ..., max=max))
}


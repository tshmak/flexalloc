dlnorm2 <- function(x, ..., max) {
  P <- plnorm(q=max, ...)
  return(ifelse(x>max, 0, dlnorm(x, ...)/P))
}

plnorm2 <- function(q, ..., max) {
  P <- plnorm(q=max, ...)
  return(pmin(1, plnorm(q, ...)/P))
}

qlnorm2 <- function(p, ..., max) {
  P <- plnorm(q=max, ...)
  return(qlnorm(P*p, ...))
}

rlnorm2 <- function(n, ..., max) {
  u <- runif(n)
  return(qlnorm2(u, ..., max=max))
}


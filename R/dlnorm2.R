dlnorm2 <- function(..., max) {
  P <- plnorm(q=max, ...)
  return(dlnorm(...)/P)
}


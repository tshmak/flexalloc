delta <- function(b, f=dunif, F=punif, ...) {
  return(b + F(b, ...)/f(b, ...))
}


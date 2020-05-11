downweight2 <- function(x, Maxx1, slope=-1) {
  stopifnot(slope < 0)
  x2 <- pmax(x - Maxx1, 0)
  return(pmax(1 + slope / Maxx1 * x2,0))
}
# y <- downweight2(x <- seq(0, 30, len=100), Maxx1=10, slope=-1); plot(x,y, type="l")

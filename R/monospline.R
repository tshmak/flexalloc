# Fit a monotonic spline 
# Taken from https://stats.stackexchange.com/questions/197509/how-to-smooth-data-and-force-monotonicity
library(scam)
monospline <- function(x,y, bs="mpd", ...) {
  #' bs="mpd" for descending; bs="mpi" for ascending
  dat <- data.frame(x=x,y=y)
  sc <- scam(y ~ s(x, bs=bs, ...), data = dat)
  f <- function(x0) {
    return(predict(sc, data.frame(x=x0)))
  }
  return(f)
}
# Example: 
# df <- data.frame(x=1:10, y=c(100,41,22,10,6,7,2,1,3,1))
# plot(df$x,df$y)
# ms <- monospline(df$x, df$y)
# x0 <- seq(1,10, len=100)
# lines(x0, ms(x0))


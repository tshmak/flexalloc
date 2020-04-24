downweight <- function(x, Maxx1, dipp) {
  dipstart <- Maxx1*dipp
  Max <- Maxx1 + (Maxx1 - dipstart)
  stopifnot(x >= 0)
  slope <- -1/2 / (Maxx1 - dipstart)
  result <- ifelse(x <= dipstart, 1, 
                   ifelse(x >= Max, 0, 1 + (x - dipstart)* slope))
  return(result)
}
# y <- downweight(x <- seq(0, 30, len=100), Maxx1=10, dipp=0.1); plot(x,y, type="l")
# y <- downweight(x, Maxx1=10, dipp=1); lines(x,y)

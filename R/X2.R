X2 <- function(v, sumx=1, maxx=sumx, k=2, round=FALSE) {
  n <- length(v)
  stopifnot(v > 0)
  stopifnot(maxx > 0)
  v <- v^k
  if(length(maxx) == 1) maxx <- rep(maxx, n)
  stopifnot(length(maxx) == n)
  
  x <- rep(0,n)
  while(TRUE) {
    notmax <- x < maxx
    v2 <- v[notmax]
    v2inv <- 1/v2
    dist <- sumx * v2inv / sum(v2inv)
    exceeds <- dist >= maxx[notmax]
    
    if(any(exceeds)) {
      toupdate <- notmax
      toupdate[toupdate] <- exceeds
      x[toupdate] <- maxx[toupdate]
      sumx <- sumx - sum(maxx[toupdate])
    } else {
      x[notmax] <- dist
      break
    }
  }
  if(round) x <- round(x)
  return(x)
}


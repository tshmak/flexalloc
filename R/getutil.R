getutil <- function(params, ..., npoints=200) {
  
  # Options needed: V1, Maxx1, Beta1, b, maxx, sumx, k, bmax, dipp
  o <- list(...)
  
  o$b[1] <- params["b1hat"]
  o$maxx[1] <- params["maxx1hat"]
  stopifnot(is.finite(c(o$b, o$maxx, o$beta)))
  xhat1prop <- params["xhat1prop"]
  names(xhat1prop) <- NULL
  
  util1 <- with(o, {
    delt <- delta(b,f=ffun, F=Ffun)
    
    if(k < Inf) {
      Pay <- MyersonPay(b, X2, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                         bmax=bmax, subset=1, npoints=npoints)
    } else {
      Pay <- MyersonPay(b, Xlinear, sumx=sumx, maxx=maxx, k=k, f=ffun, F=Ffun, 
                         bmax=bmax, subset=1, npoints=npoints)
    }
    
    pay1 <- Pay[1]
    # x1 <- attr(Pay, "x0")[1] 
    x1 <- attr(Pay, "x0i")[1] # Use interpolated x rather than real x to enforce monotonicity of util on b
    x1real <- attr(Pay, "x0")[1]
    
    xhat1 <- x1 * xhat1prop
    cost1 <- xhat1*V1
    # dw1 <- downweight(xhat1, Maxx1 = Maxx1, dipp = dipp)
    dw1 <- downweight(x1real*xhat1prop, Maxx1=Maxx1, dipp=dipp) # We need to use the real x for downweight, however, to avoid mis-adjustment
    
    actualpay1 <- pay1 * xhat1prop * Beta1
    util1 <- dw1 * (actualpay1 - cost1)
    
    details <- list(x1=x1, x1real=x1real, xhat1=xhat1, cost1=cost1, dw1=dw1, pay1=pay1,
                    actualpay1=actualpay1)
    attributes(util1) <- details
    util1
  } )
  
  return(util1)
}

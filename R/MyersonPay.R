MyersonPay <- function(..., integration=c("rectangle", "trapezium")) {

  integration <- match.arg(integration)
  if(integration == "rectangle") {
    return(MyersonPay0(...))
  } else if(integration == "trapezium") {
    return(MyersonPay1(...))
  }

}


require (quantmod)


getTicker <- function (sym, auto.assign=FALSE,
                       from="1999-03-10", to="2015-06-04") {
  fileName <- paste (sym, ".RData", sep='')
  if (length (grep (fileName, dir())) == 0) {
    stock_data <- getSymbols (sym, src="yahoo", from=from, to=to, auto.assign=FALSE)
    stock_data <- to.monthly (stock_data)
    save (stock_data, file=fileName)
  } else {
    load (fileName)
  }
  
  adj <- Ad (stock_data)
  result <- list()
  result$the_months <- index (adj)
  result$price <- coredata (adj)
  m = dim (result$price)[1]
  result$m <- m
  result$Rt <- result$price[2:m,1]/result$price[1:(m-1),1] - 1
  result$rt <- log (1 + result$Rt)
  result$ticker <- sym
  
  result$call <- match.call()
  class (result) <- "MonthlyLogReturnsType"
  
  if (auto.assign) {
    assign (sym, result, envir=parent.env (environment()))
  } else {
    result
  }
}
source ("./hp_loess.R")

getCtSt <- function (MLR, ...) { "... arguments to pass to hp_loess"
  stopifnot (class(MLR) == "MonthlyLogReturnsType")
  
  m <- MLR$m
  result <- list()
  result$the_months <- MLR$the_months
  
  df.rt <- data.frame (t=c(1:length(MLR$rt)),
                       rt = MLR$rt)
  
  loc.trend <- hp_loess (rt~t, dat=df.rt, ...)
  result$ct <- loc.trend$fitted
  
  # See 87 of Wasserman's All of Nonparametric Statistics
  Z <- log (loc.trend$residuals^2)
  df.Z <- data.frame (t=df.rt$t, Z=Z)
  sc.trend <- hp_loess (Z~t, dat=df.Z, ...)
  result$st <- sqrt (exp (sc.trend$fitted)) # This is the estimate of the scale function.
  
  result$zt <- (MLR$rt - result$ct) / result$st
  
  result$call <- match.call()
  class (result) <- "trendType"
  
  result
}


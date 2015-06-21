# This function is meant to test whether the input vector entries
# are independent samples from the same distribution.
require (gtools)
isIID <- function (x, CL=0.95)  {
  bestPerBin <- function (N) {
    s <- 1
    while (s * (4 * factorial (s) - 4) < N) s <- s+1
    ans <- list()
    ans$perBin <- s - 1
    ans$bins <- as.integer (N/ans$perBin)
    ans$perms <- factorial (ans$perBin)
    
    ans$p <- 1/ans$perms
    ans$CI <- sqrt ((1-ans$p)/(ans$p * ans$bins))
    ans
  }
  
  N <- length(x)
  ans <- bestPerBin (N=N)
  
  lu <- defmacro (j, b, expr={
    l = b*(j-1)+1; u = b*j;
    c(l:u)
  })
  
  
  counts <- list()
  
  for (i in 1:ans$bins) {
    tmp <- x[ lu (i, ans$perBin)]
    tmp <- paste0 (order(tmp), sep='', collapse='')
    if (!(tmp %in% names(counts))) {
      counts[[tmp]] <-1
    } else {
      counts[[tmp]] <- counts[[tmp]] + 1
    }
  }
  
  degfree <- ans$perms-1
  
  one <- 0.0
  two <- 0.0 - ans$bins * log(ans$bins) + ans$bins * log(ans$perms)
  
  for (i in 1:length(counts)) {
    ki <- counts[[i]]
    if (ki != 0) {
      one <- one + ki * log(ki)
    }
  }
  
  result <- list()
  result$df <- degfree
  result$CL <- CL
  result$statistic <- 2 * (one + two)
  result$crit.val <- qchisq (p=CL, df=degfree)
  result$p.val <- 1 - pchisq (q=result$statistic, df=degfree)
  result$reject <- FALSE
  if (result$statistic > qchisq (p=0.95,df=degfree)) {
    result$reject <- TRUE
  }
  
  result
}




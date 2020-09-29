#' Plotting Implied Volatility of European Option
#'
#' To understand how moneyness affect option volatility,
#' we plot Implied Volatility against different strike prices.
#'
#' plotImplied function is a function that will return the implied volatility using bisection method
#' and plot the result
#'
#' BS function is a self-written to calculate option price using Black-Scholes equation
#'
#' genOptionDF provides a sample of dataframe
#'
#' @param df This dataframe should contain strike, type, optionPrice, futurePrice, and time_to_expiry of the option
#'
#'
plotImpliedVol <- function(df) {
  library(ggplot2)
  n <- dim(df)[1]
  eps <- .000001
  r <- 0
  ans <- c()
  for (i in 1:n) {
    strike <- as.numeric(as.character(df$strike[i]))
    type <- as.character(df$type[i])
    optionPrice <- as.numeric(as.character(df$optionPrice[i]))
    futurePrice <- as.numeric(as.character(df$futurePrice[i]))
    time_to_expiry <- as.numeric(as.character(df$time_to_expiry[i]))
    #Here, I implement bisection method to find the implied volatility that equates BS value to observed option value
    #To be safe, I make the maximum of volatility to be extremely large, i.e. 10
    sigMin <- .000001
    sigMax <- 10
    sigMid <- (sigMin+sigMax)/2
    while (abs(BS(strike,type,sigMid,futurePrice,time_to_expiry,r)-optionPrice)>eps) {
      if((BS(strike,type,sigMin,futurePrice,time_to_expiry,r)-optionPrice)*(BS(strike,type,sigMid,futurePrice,time_to_expiry,r)-optionPrice)<0) sigMax <- sigMid
      else sigMin <- sigMid
      sigMid <- (sigMin+sigMax)/2
    }
    ans[i] <- sigMid
  }

  p <- ggplot2::qplot(as.numeric(as.character(df$strike)),ans,main="Scatterplots of Implied Volatility under Different Strike Price",
                      xlab="Strike Price", ylab="Implied Volatility")
  p <- p + theme_bw()
  p <- p + theme(axis.title=element_text(face="bold",
                                         size="12", color="red"), legend.position="top")
  p <- p + geom_line()
  print(p)
  return(ans)
}

BS <- function(strike, type, sigEst, futurePrice, time_to_expiry,r) {
  #BS will take strike, sigma, futurePrice, time_to_expiry, r as inputs, and then calculate option
  #price depending whether it is call or put
  d1 <- (log(futurePrice/strike)+time_to_expiry*sigEst^2/2)/(sigEst*sqrt(time_to_expiry))
  d2 <- (log(futurePrice/strike)-time_to_expiry*sigEst^2/2)/(sigEst*sqrt(time_to_expiry))
  if (type=="C") return(exp(-r*time_to_expiry)*(futurePrice*pnorm(d1)-strike*pnorm(d2)))
  else if (type=="P") return(exp(-r*time_to_expiry)*(-futurePrice*pnorm(-d1)+strike*pnorm(-d2)))
}

genOptionDF <- function() {
  #This is just a sample dataframe generator
  strike <- c(187.5,192.5,195,197.5,197.5,200,202.5,212.5,217.5)
  type <- c("C","C","P","C","P","C","C","C","C")
  optionPrice <- c(16.18,13.25,11.13,10.75,12.5,9.68,8.2,5.05,4.3)
  futurePrice <- rep(195.69,9)
  time_to_expiry <- rep(.25,9)
  df <- data.frame(strike,type,optionPrice,futurePrice,time_to_expiry)
  return(df)
}

library(plotImpliedVol)

df = data.frame(strike = c(50, 20), # the option strike - in $
    type = c("C", "P"), # either “c” for call option or “p” for a put option
    optionPrice = c(1.62,0.01), # the option price - in $
    futurePrice = c(48.03, 48.03), # the price of the underlying future - in $
    time_to_expiry = c(0.1423, 0.1423)) # the option time to expiry - in year

plotImpliedVol(df)

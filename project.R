library(quantmod)

# get daily S&P500 index
SPX <- getSymbols("^GSPC", auto.assign = FALSE, from = "1980-01-01")
head(SPX)

# calculate daily log returns of the adjusted closing prices
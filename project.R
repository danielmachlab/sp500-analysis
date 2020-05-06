library(quantmod)
library(forecast)
library(MSGARCH)

# get daily S&P500 index
sp500 <- getSymbols("^GSPC", auto.assign = FALSE, from = "1990-01-01")
head(sp500)

# calculate daily log returns of the adjusted closing prices
sp500.lrtn = diff(log(sp500$GSPC.Adjusted))[-1,]
head(sp500.lrtn)

# select the best ARMA model
arma = auto.arima(sp500.lrtn, ic="bic", trace = F, stepwise = F, seasonal= F)
arma

data = arma$residuals

length(data)

############### Harsh's Models
# 1-state GARCH normal dist model
ms1.grach.norm <- CreateSpec(variance.spec = list(model = "sGARCH"), 
                             distribution.spec = list(distribution = "norm"))
summary(ms1.grach.norm)
fit.ms1.garch.norm <- FitML(spec = ms1.grach.norm, data = data)
summary(fit.ms1.garch.norm)


# 1-state GJR normal dist model
ms1.gjr.norm <- CreateSpec(variance.spec = list(model = "gjrGARCH"), 
                           distribution.spec = list(distribution = "norm"))
fit.ms1.gjr.norm <- FitML(spec = ms1.gjr.norm, data = data)
summary(fit.ms1.gjr.norm)


# 2-state GARCH normal dist model
ms2.grach.norm <- CreateSpec(variance.spec = list(model = "sGARCH"), 
                             distribution.spec = list(distribution = "norm"),
                             switch.spec = list(K = 2))
fit.ms2.garch.norm <- FitML(spec = ms2.grach.norm, data =data)
summary(fit.ms2.garch.norm)


# 2-state GJR normal dist model
ms2.gjr.norm <- CreateSpec(variance.spec = list(model = "gjrGARCH"), 
                           distribution.spec = list(distribution = "norm"),
                           switch.spec = list(K = 2))
fit.ms2.gjr.norm <- FitML(spec = ms2.gjr.norm, data = data)
summary(fit.ms2.gjr.norm)


############### Daniel's Models
# 1 state - GARCH - student t
ms1.garch.student_t <- CreateSpec(variance.spec = list(model = "sGARCH"),
                               distribution.spec = list(distribution = "std"))
fit.ms1.garch.student_t <- FitML(spec = ms1.garch.student_t, data = data)
summary(fit.ms1.garch.student_t)

# 1 state - GJR - student t
ms1.gjr.student_t <- CreateSpec(variance.spec = list(model = "sGARCH"),
                               distribution.spec = list(distribution = "std"))
fit.ms1.gjr.student_t <- FitML(spec = ms1.gjr.student_t, data =data)
summary(fit.ms1.gjr.student_t)

# 2 state - GARCH - student t
ms2.garch.student_t <- CreateSpec(variance.spec = list(model = "sGARCH"),
                               distribution.spec = list(distribution = "std"),
                               switch.spec = list(K=2))
fit.ms2.garch.student_t <- FitML(spec = ms2.garch.student_t, data = data)
summary(fit.ms2.garch.student_t)


# 2 state - GJR - student t
ms2.gjr.student_t <- CreateSpec(variance.spec = list(model = "sGARCH"),
                               distribution.spec = list(distribution = "std"),
                               switch.spec = list(K=2))
fit.ms2.gjr.student_t <- FitML(spec = ms2.gjr.student_t, data = data)
summary(fit.ms2.gjr.student_t)


############### Rizwan's Models
#1-state GARCH skewstd dist model
ms1.garch.sstd <- CreateSpec(variance.spec = list(model = "sGARCH"),
                             distribution.spec = list(distribution = "sstd"))


fit.ms1.garch.sstd <- FitML(spec = ms1.garch.sstd, data = data)
summary(fit.ms1.garch.sstd)

#1-state GJR skewstd dist model
ms1.gjr.sstd <- CreateSpec(variance.spec = list(model = "gjrGARCH"),
                           distribution.spec = list(distribution = "sstd"))


fit.ms1.gjr.sstd <- FitML(spec = ms1.garch.sstd, data = data)
summary(fit.ms1.garch.sstd)

#2-state GARCH skewstd dist model
ms2.garch.sstd <- CreateSpec(variance.spec = list(model = "sGARCH"),
                             distribution.spec = list(distribution = "sstd"),
                             switch.spec = list(K = 2))

fit.ms2.garch.sstd <- FitML(spec = ms2.garch.sstd, data = data)
summary(fit.ms2.garch.sstd)

#2-state GJR skewstd dist model
ms2.gjr.sstd <- CreateSpec(variance.spec = list(model = "gjrGARCH"),
                           distribution.spec = list(distribution = "sstd"),
                           switch.spec = list(K = 2))

fit.ms2.gjr.sstd <- FitML(spec = ms2.gjr.sstd, data = data)
summary(fit.ms2.gjr.sstd)

### PART 4
models = list(ms1.grach.norm, ms1.gjr.norm, ms2.grach.norm, ms2.gjr.norm,
              ms1.garch.student_t, ms1.gjr.student_t, ms2.garch.student_t, ms2.gjr.student_t,
              ms1.garch.sstd, ms1.gjr.sstd, ms2.garch.sstd, ms2.gjr.sstd)

# sample size for out-of sample testing 
n.ots <- 2000
# in sample window size is first xxxx data. (# data) 7644 - 2000 = 5644
n.its <- length(data) - n.ots
k.update <- 100

## Create forcast function
forcast <- function(alpha) { 
  cat("Forcasting... Alpha = ", alpha, "\n")
  VaR <- matrix(NA, nrow = n.ots, ncol = length(models))
  y.ots <- matrix(NA, nrow = n.ots, ncol = 1)
  model.fit <- vector(mode = "list", length = length(models))
  for (i in 1:n.ots) {
    cat("Backtest - Iteration: ", i, "\n")
    y.its <- data[i:(n.its + i - 1)]
    y.ots[i] <- data[n.its + i]
    for (j in 1:length(models)) {
      if (k.update == 1 || i %% k.update == 1) {
        cat("Model", j, "is reestimated\n")
        model.fit[[j]] <- FitML(spec = models[[j]], data = y.its,
                                ctr = list(do.se = FALSE))
      }
      VaR[i, j] <- Risk(model.fit[[j]]$spec, par = model.fit[[j]]$par,
                        data = y.its, n.ahead = 1, alpha = alpha, do.es = FALSE,
                        do.its = FALSE)$VaR
      }
  }
  return(VaR)
}


calcPercentUnder = function(VaR, recent2kdata){
  percentUnder = list()
  for(j in 1:ncol(VaR)){
    counter = 0
    v = VaR[,j]
    for (i in 1:length(v)){
      if(recent2kdata[i] < v[i]){
        counter=counter+1
      }
    }
    percentUnder[[j]] <- counter/length(recent2kdata)  
  }
  return(percentUnder)
}
## Forcast at 5%
# VaR5 = forcast(0.05)
recent2kdata = data[(n.its+1):length(data)]
pU5 = calcPercentUnder(VaR5, recent2kdata)
print(pU5)


## Forcast at 1%
VaR1 = forcast(0.01)
pU1 = calcPercentUnder(VaR1, recent2kdata)
print(pU1)








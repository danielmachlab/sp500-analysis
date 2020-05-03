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


############### Harsh's Models
# 1-state GARCH normal dist model
ms1.grach.norm <- CreateSpec(variance.spec = list(model = "sGARCH"), 
                             distribution.spec = list(distribution = "norm"))
fit.ms1.garch.norm <- FitML(spec = ms1.grach.norm, data = sp500.lrtn)
summary(fit.ms1.garch.norm)


# 1-state GJR normal dist model
ms1.gjr.norm <- CreateSpec(variance.spec = list(model = "gjrGARCH"), 
                           distribution.spec = list(distribution = "norm"))
fit.ms1.gjr.norm <- FitML(spec = ms1.gjr.norm, data = sp500.lrtn)
summary(fit.ms1.gjr.norm)


# 2-state GARCH normal dist model
ms2.grach.norm <- CreateSpec(variance.spec = list(model = "sGARCH"), 
                             distribution.spec = list(distribution = "norm"),
                             switch.spec = list(K = 2))
fit.ms2.garch.norm <- FitML(spec = ms2.grach.norm, data = sp500.lrtn)
summary(fit.ms2.garch.norm)


# 2-state GJR normal dist model
ms2.gjr.norm <- CreateSpec(variance.spec = list(model = "gjrGARCH"), 
                           distribution.spec = list(distribution = "norm"),
                           switch.spec = list(K = 2))
fit.ms2.gjr.norm <- FitML(spec = ms2.gjr.norm, data = sp500.lrtn)
summary(fit.ms2.gjr.norm)


############### Daniel's Models
# 1 state - GARCH - student t
ms1.garch.student_t <- CreateSpec(variance.spec = list(model = "tGARCH"),
                               distribution.spec = list(distribution = "std"))
fit.ms1.garch.student_t <- FitML(spec = ms1.garch.student_t, data = sp500.lrtn)
summary(fit.ms1.garch.student_t)

# 2 state - GARCH - student t
ms2.garch.student_t <- CreateSpec(variance.spec = list(model = c("tGARCH", "tGARCH")),
                               distribution.spec = list(distribution = c("std", "std")),
                               switch.spec = list(k=2))
fit.ms2.garch.student_t <- FitML(spec = ms2.garch.student_t, data = sp500.lrtn)
summary(fit.ms2.garch.student_t)

# 1 state - GJR - student t
ms1.garch.student_t <- CreateSpec(variance.spec = list(model = "tGARCH"),
                               distribution.spec = list(distribution = "std"))
fit.ms1.garch.student_t <- FitML(spec = ms1.garch.student_t, data = sp500.lrtn)
summary(fit.ms1.garch.student_t)

# 2 state - GJR - student t
ms2.garch.student_t <- CreateSpec(variance.spec = list(model = c("tGARCH", "tGARCH")),
                               distribution.spec = list(distribution = c("std", "std")),
                               switch.spec = list(k=2))
fit.ms2.garch.student_t <- FitML(spec = ms2.garch.student_t, data = sp500.lrtn)
summary(fit.ms2.garch.student_t)


############### Rizwan's Models


#1-state GARCH skewstd dist model
ms1.garch.sstd <- CreateSpec(variance.spec = list(model = "tGARCH"),
                             distribution.spec = list(distribution = "sstd"))


fit.ms1.garch.sstd <- FitML(spec = ms1.garch.sstd, data = sp500.lrtn)
summary(fit.ms1.garch.sstd)

#1-state GJR skewstd dist model
ms1.gjr.sstd <- CreateSpec(variance.spec = list(model = "gjrGARCH"),
                           distribution.spec = list(distribution = "sstd"))


fit.ms1.gjr.sstd <- FitML(spec = ms1.garch.sstd, data = sp500.lrtn)
summary(fit.ms1.garch.sstd)

#2-state GARCH skewstd dist model
ms2.garch.sstd <- CreateSpec(variance.spec = list(model = "tGARCH"),
                             distribution.spec = list(distribution = "sstd"),
                             switch.spec = list(K = 2))

fit.ms2.garch.sstd <- FitML(spec = ms2.garch.sstd, data = sp500.lrtn)
summary(fit.ms2.garch.sstd)

#2-state GJR skewstd dist model
ms2.gjr.sstd <- CreateSpec(variance.spec = list(model = "gjrGARCH"),
                           distribution.spec = list(distribution = "sstd"),
                           switch.spec = list(K = 2))

fit.ms2.gjr.sstd <- FitML(spec = ms2.gjr.sstd, data = sp500.lrtn)
summary(fit.ms2.gjr.sstd)

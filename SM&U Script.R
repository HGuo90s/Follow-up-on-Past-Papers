# Econ 191 Codes and Graphs
# Unemployment News and Stock Market

# Import all raw data
library(tseries)
library(vars)
data.ur = read.table("Unemployment_Rate_Dep_of_Labor.txt", header = T)
data.ip = read.table("Industrial_Production_Index_FRED.txt", header = T)
data.tb = read.table("3_Month_T_Bill_FRED.txt", header = T)
data.spr = read.table("Baa_Aaa_Yield_Spread_FRED.txt", header = T)
data.sp500 = read.table("S&P500_Global Financial.txt", header = T) # Interpolation Needed
data.inf = read.table("Consumer_Price_Index_FRED.txt", header = T)
# Convert to time series
urate.full = ts(data.ur[,2], start = 1948, end = c(2019, 1), frequency = 12)
durate.full = diff(urate.full)
indprod.full = ts(data.ip[,2], start = 1919, end = c(2019, 1), frequency = 12)
dindprod.full = diff(log(indprod.full))*100
tbill.full = ts(data.tb[,2], start = 1934, end = c(2019, 2), frequency = 12)
dtbill.full = diff(tbill.full)
yspread.full = ts(data.spr[,2], start = 1970, end = c(2012, 12), frequency = 12)
dyspread.full = diff(yspread.full)
inf.full = ts(data.inf[,2], start = 1947, end = c(2019,2), frequency = 12)
dinf.full = diff(log(inf.full))


# Part 1 Independent Variable: Unemployment News
plot(unemployment.full, col = "orange", lwd = 3, main = "Unemployment Rate Since 1948")
plot(durate.full, col = "orange", lwd = 3, main = "Macro Variables for Unemployment Forecasting",
     ylim = c(-3,3))
lines(dindprod.full, col = "plum", lwd = 3)
lines(dtbill.full, col = "royalblue", lwd = 2)
lines(dyspread.full, col = "palegreen", lwd = 3)
legend("topright", legend=c("Change in Unemployment Rate", "Growth Rate of Industrial Production Index", 
                            "Change in 3 month T-bill", "Change in Baa and Aaa default yield spread"),
       col=c("orange", "plum", "royalblue", "palegreen"), lty=1:1, cex=0.8)
# Regression
durate = window(durate.full,start = c(1970,2),end=c(2012, 12))
dindprod = window(dindprod.full,start = c(1970,2),end=c(2012, 12))
dtbill = window(dtbill.full,start = c(1970,2),end=c(2012, 12))
dyspread = window(dyspread.full,start = c(1970,2),end=c(2012, 12))
reg.urate = ts(cbind(durate, dindprod, dtbill, dyspread))
VARselect(reg.urate, lag.max = 10, type = "const")
VARselect(reg.urate, lag.max = 20, type = "const")
# Given lag = 10
reg.urate.2 = VAR(reg.urate, p = 2)
summary(reg.urate.2)
reg.urate.6 = VAR(reg.urate, p = 6)
summary(reg.urate.6)
# Given max lag = 17
reg.urate.17 = VAR(reg.urate, p = 17)
summary(reg.urate.17)
# Estimate Unemployment Rate
list = as.data.frame(reg.urate) # Length 515 per column
residual = resid(reg.urate.6)
res = as.data.frame(residual) # Length of urate residual 509, 515-509 = 6
     # Remember that the time covered here is Feb 1970 to Dec 2012
     # Calculate UR from URt-6, so the starting point is 1970,8
estimate.dur = c()
for (i in 7:515) {
  estimate.dur[i] = 0.027700 - 7.577840*list$dindprod[i-1] - 0.139611*list$durate[i-1] 
                  + 0.196483*list$dyspread[i-1] - 2.694089*list$dindprod[i-2]  
                  + 0.040022*list$dtbill[i-5] + 0.099328*list$durate[i-6] + res$durate[i-6]
} # Applying the Regression Equation obtained in previous section
estimate = c()
for (i in 1:508) {
  estimate[i] = estimate.dur[i+7]
} # Removing the NAs
est.durate = ts(estimate, start = c(1970,8), end = c(2012,12), frequency = 12)
# Unemployment Shock
durateshrink = window(durate,start = c(1970,8),end=c(2012, 12))
listdurate = as.data.frame(durateshrink)
listestimate = as.data.frame(est.durate)
est.shock = c()
for (i in 1:509) {
  est.shock[i] = listdurate$x[i] - listestimate$x[i]
}
shock = ts(est.shock, start = c(1970,8), end = c(2012,12), frequency = 12)
plot(shock, col = "orange", lwd = 2, ylab = "Unemployment Shock V1", 
     main = "Unemployment Shock under Version 1 Estimation")
lines(durateshrink, col = "plum", lwd = 2)
lines(est.durate, col = "palegreen", lwd = 2)
legend("topright", legend=c("Unemployment Difference", "Estimated Difference", "Unemployment Shock"),
       col=c("plum", "palegreen", "orange"), lty=1:1, cex=0.8)
# Removing first lags of urate, 
estimate.dur1 = c()
for (i in 7:515) {
  estimate.dur1[i] = 0.027700 - 2.694089*list$dindprod[i-2]  
  + 0.040022*list$dtbill[i-5] + 0.099328*list$durate[i-6] + res$durate[i-6]
} # Applying the Regression Equation obtained in previous section
estimate1 = c()
for (i in 1:508) {
  estimate1[i] = estimate.dur1[i+7]
} # Removing the NAs
est.durate1 = ts(estimate1, start = c(1970,8), end = c(2012,12), frequency = 12)
# Unemployment Shock
durateshrink1 = window(durate,start = c(1970,8),end=c(2012, 12))
listdurate1 = as.data.frame(durateshrink1)
listestimate1 = as.data.frame(est.durate1)
est.shock1 = c()
for (i in 1:509) {
  est.shock1[i] = listdurate1$x[i] - listestimate1$x[i]
}
shock1 = ts(est.shock1, start = c(1970,8), end = c(2012,12), frequency = 12)
plot(shock, col = "orange", lwd = 2, ylab = "Unemployment Shock V1", 
     main = "Unemployment Shock under Version 1 Estimation")
lines(shock1, col = "brown", lwd = 2)
legend("topright", legend=c("Remaining First Lag", "Deleting First Lag"),
       col=c("orange", "brown"), lty=1:1, cex=0.8)
# Adding Inflation to the graph
dinf = window(dinf.full,start = c(1970,2),end=c(2012, 12))
reg.urate1 = ts(cbind(durate, dindprod, dtbill, dyspread, dinf))
VARselect(reg.urate, lag.max = 10, type = "const")
reg.urate.9 = VAR(reg.urate, p = 9)
summary(reg.urate.9)
list = as.data.frame(reg.urate) # Length 515 per column
residual2 = resid(reg.urate.9)
res2 = as.data.frame(residual2)
estimate.dur2 = c()
for (i in 8:515) {
  estimate.dur2[i] = 0.729763 *list$dindprod[i-1] -1.156072 *list$dyspread[i-1] 
  + 0.091342*list$dindprod[i-1] - 0.509456*list$durate[i-2] - 0.233034*list$dinf[i-2] 
  - 0.439844*list$durate[i-3] - 0.772003*list$dyspread[i-3] - 0.145658*list$dtbill[i-3]   
  + 0.432515*list$durate[i-7] + res2$durate[i]
} # Applying the Regression Equation obtained in previous section
estimate2 = c()
for (i in 1:507) {
  estimate2[i] = estimate.dur2[i+8]
} # Removing the NAs
est.durate2 = ts(estimate2, start = c(1970,8), end = c(2012,12), frequency = 12)
# Unemployment Shock
durateshrink1 = window(durate,start = c(1970,8),end=c(2012, 12))
listdurate1 = as.data.frame(durateshrink1)
listestimate2 = as.data.frame(est.durate2)
est.shock2 = c()
for (i in 1:509) {
  est.shock2[i] = listdurate1$x[i] - listestimate2$x[i]
}
shock2 = ts(est.shock2, start = c(1970,8), end = c(2012,12), frequency = 12)
plot(shock2, col="cyan", lwd = 2, ylab = "shock estimate", main = "Different estimates")
lines(shock, col = "orange", lwd = 2)
lines(durateshrink, col = "plum", lwd = 2)
legend("topright", legend=c("Three Variables", "Added Inflation"),
       col=c("cyan","orange"), lty=1:1, cex=0.8)
# Part 2 Stock Price and Unemployment Shock
# Plot a graph of Stock price and 
plot(data.sp500)

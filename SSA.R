library(quantmod)
library(forecast)
library(Rssa)

#load data
getSymbols("AAPL", src = "yahoo", from = as.Date("2017-01-01"), to = as.Date("2018-01-01"))
data <- AAPL$AAPL.Close

#decompose
decompose <- ssa(data)

#eigen plots
plot(decompose) #eigenvalues
#plot(decompose, type = "vectors", groups = as.list(1:5)) #eigenvectors
plot(decompose, type = "paired", groups = as.list(1:20)) #pairs of eigenvectors
plot(decompose, type = "wcor", groups = as.list(1:20)) #wcor

#reconstruction
recon <- reconstruct(decompose, groups = list(1, c(2,3), 4, 5, c(6,7), c(8,9), c(10,11)))

#plot 1st eigenvalue on time series
plot (data, lwd=3, col="gray")
lines(recon$F1, col="red", lwd= 2) #1st eigen explains the trend in the time series, 

#overfit reconstruction
recon_all <- reconstruct(decompose, groups = list(1:11))

#plot overfit reconsruction on time series
plot (data, lwd=3, col="gray")
lines(recon_all$F1, col="red", lwd= 2) 

#residuals
res <- residuals(recon)

#residuals somewhat normally distributed
hist(res)
spec.pgram(res,detrend = FALSE,log = "no") #periodogram shows seasonality due to repeating patterns at 0.1 & 0.2

#reconstruction 2 to remove seasonality
recon_2 <- ssa(res)
plot(recon_2) #eigenvalues
plot(recon_2, type = "paired", idx = 1:20, plot.contrib = FALSE) #eigenvectors pair

#Recreating the seasonality using forst 10 eigenvectors
res_2 <- reconstruct(recon_2, groups=list(1:10))
res_final <- residuals(res_2);

#plot residuals
plot(res_final)

#checking residuals to make sure it is white noise
plot(density(res_final))



#clear everything
rm(list = ls())

# checks for installed packages
installed.packages()

# loads the needed libraries
library(fBasics)
#library(graphics)

# reads the data
data = read.csv("ngdp.csv")

# creates a time series object
help(ts)
help(c)

# forecasts from modeld
modeld = ts(data[,2],start=c(2001,1), frequency=12) 
# forecasts from modelb
modelb = ts(data[,3],start=c(2001,1), frequency=12)
# realizations
modelr = ts(data[,4],start=c(2001,1), frequency=12)

# squared forecast errors
d1 = ts((modelr - modeld)^2)
d2 = ts((modelr - modelb)^2)

# plotting
dataforplot = cbind(d1,d2)
dev.new()
plot(dataforplot, plot.type= c("single"), ylab = "Squared Forecast Errors", xlab="Time", col="blue",lty= 1:2)
title("NGDP Forecasting",ylab = "Squared Forecast Errors")
legend("topleft",legend=c("Model D","Model B"),col="blue",lty=1:2)

# calculates the mean squared forecast error
mean(d1)
mean(d2)
# calculates the root mean squared forecast error
sqrt(mean(d1))
sqrt(mean(d2))
t.test(d1-d2)

# generate a random variable
set.seed(1)
epsilon = rnorm(100)
print(length(epsilon))
m1 = mean(epsilon)
m2 = sd(epsilon)
m3 = max(epsilon)
print(c(m1,m2,m3))

# plots
dev.new()
hist(epsilon)
dev.new()
plot(epsilon,type = "l")

# get the descriptive statistics, you need the basicStats package for this
ds = basicStats(epsilon)
ds[16,1]

# write a loop
T = 100;
epsilon_alt =  matrix(NA,nrow=T, ncol=1)
for (i in 1:T)
{
print("I am randomizing...")
print(i*i)
epsilon_alt[i] = rnorm(1)
}

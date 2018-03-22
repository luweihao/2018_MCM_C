library(forecast)
library(tseries)
AZ.info=read.csv("AZ.info.csv")


### AZ's Coal Price Prediction ###
plot(AZ.info$CLTCD)

ts2=ts(AZ.info$CLTCD[1:50])
lts2=log(ts2)
ndiffs(lts2)
dlts=diff(lts2,2)
plot(dlts)
adf.test(dlts)
fit=auto.arima(lts2)
fit
jpeg("coal.jpeg", height=600, width=1000, quality = 1500)
plot(foreAZst(fit,41))
points(51,log(1.8),pch=10,col="red")
points(52,log(1.98),pch=10,col="red")
points(53,log(2.08),pch=10,col="red")
points(54,log(2.07),pch=10,col="red")
points(55,log(2.1),pch=10,col="red")
legend(1,12,legend = c("The bule line is the predicted value",
                       "The red points are the real value"))
dev.off()
plot(forecast(fit,41))
forecast(fit,41)
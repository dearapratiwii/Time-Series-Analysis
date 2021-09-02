# Menghapus volume = 0 dan close = null
data = read.csv("D:/Semester 6/Analisis Deret Waktu/EAS/SRTG.JK.csv")
data
data[data$Volume==0,]
data[data$Close=="null",]
data1 = data[-c(3,7,12,14,22,23,33,43,45,48,51,53,54,55,57,60,61,64,68,74,76,84,105,107,108,109,110,111,292),]
data1

# Deklarasi Variabel ihsg dan date
srtg = as.numeric(as.character(data1$Close))
date = as.Date(data1$Date, origin = "2019-01-04")
write.table(ihsg, "D:/srtg.txt")

# Time Series Plot
library(ggplot2)
tmp <- data.frame(SRTG=srtg,
                  Tanggal=date)
p <- ggplot(tmp, aes(Tanggal, SRTG)) +
  geom_line(colour = "blue") +
  geom_vline(xintercept= tmp$Tanggal[269],
             linetype=1, colour="black", show.legend = TRUE)+
  geom_point(colour = "red", size = 1)
p + theme_bw()

data2 = data1[1:268]

par(mfrow=c(1,2))
acf(training, lag.max = 48, type = "correlation", ylim=c(-1,1))
pacf(training, lag.max = 48, ylim=c(-1,1))

library(forecast)
library(tseries)
best = auto.arima(training)
summary(best)

best = auto.arima(ihsg,stationary = TRUE, seasonal = FALSE, allowmean= TRUE, lambda = "auto")
summary(best)

#CHECKING FOR STATIONARY USING ADF TEST#
adf.test(training)
#differencing 1
Ytrain_1=diff(training,lag=1)
par(mfrow=c(1,1))
plot(Ytrain_1)

adf.test(Ytrain_1)
#model arima
#ORDER IDENTIFICATION USING ACF AND PACF FROM STATIONARY DATA
par(mfrow=c(1,2))
acf(Ytrain_1, lag.max = 30, ylim=c(-1,1))
pacf(Ytrain_1, lag.max = 30, ylim=c(-1,1))

modelARIMA=arima(training, order = c(1,1,1), include.mean=FALSE)
summary(modelARIMA)                         
coeftest(modelARIMA)                        #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)      #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))   
acf(resi.ARIMA, lag.max = 48, type = "correlation", ylim=c(-1,1))

lags <- c(9,11,19,24,30,36,42,48)                     #lag we used
p=1                                                   #the number of ar parameter
q=1                                                   #the number of ma parameter
LB.result<-matrix(0,length(lags),2)
for(i in seq_along(lags))
{
  LB.test=Box.test (resi.ARIMA, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
}
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result
Box.test(resi.ARIMA)
ks.test(resi.ARIMA,"pnorm",mean=mean(resi.ARIMA),sd=sd(resi.ARIMA))

#FORECAST FOR TESTING DATA
fore.ARIMA=predict(modelARIMA, 7)$pred        #define forecast value for testing data
se.fore.ARIMA=predict(modelARIMA, 7)$se       #define standard error for forecasting result

#CALCULATE RMSE, MAE, AND MAPE CRITERIA
accuracies=matrix(0,3,2)
colnames(accuracies)=c("Training","Testing")
rownames(accuracies)=c("RMSE","MAE","MAPE")

accuracies[1,1]=accuracy(fits.ARIMA,training)[1,2]
accuracies[2,1]=accuracy(fits.ARIMA,training)[1,3]
accuracies[3,1]=accuracy(fits.ARIMA,training)[1,5]
accuracies[1,2]=accuracy(as.vector(fore.ARIMA),testing)[1,2]
accuracies[2,2]=accuracy(as.vector(fore.ARIMA),testing)[1,3]
accuracies[3,2]=accuracy(as.vector(fore.ARIMA),testing)[1,5]
accuracies

#arima subset
modelARIMA=arima(training, order = c(20,1,19),transform.pars = FALSE, 
                 fixed=c(NA,rep(0,3),NA,rep(0,3),NA,0,0,0,0,0,0,0,0,0,0,0,0,0,NA,0,rep(0,7),NA,rep(0,6),NA), #NA was the estimated lag, that is : 1,2,11,12 and 13
                 include.mean=FALSE, method = c("ML"))
coeftest(modelARIMA)                        #significance test for parameter
resi.ARIMA=as.ts(modelARIMA$residuals)      #define the residual value
fits.ARIMA=as.ts(fitted(modelARIMA))   
acf(resi.ARIMA, lag.max = 48, type = "correlation", ylim=c(-1,1))
pacf(resi.ARIMA, lag.max = 30, ylim=c(-1,1))
lags <- c(7,8,9,11,12,13,14,19,21,24)                     #lag we used
p=3                                                  #the number of ar parameter
q=3                                                   #the number of ma parameter
LB.result<-matrix(0,length(lags),2)
for(i in seq_along(lags))
{
  LB.test=Box.test (resi.ARIMA, lag = lags[i],type = c("Ljung-Box"),fitdf=p+q)
  LB.result[i,1]=LB.test$statistic
  LB.result[i,2]=LB.test$p.value
}
rownames(LB.result)<-lags
colnames(LB.result)<-c("statistics","p.value")
LB.result
Box.test(resi.ARIMA)
ks.test(resi.ARIMA,"pnorm",mean=mean(resi.ARIMA),sd=sd(resi.ARIMA))


#FORECAST FOR TESTING DATA
fore.ARIMA=predict(modelARIMA, 7)$pred        #define forecast value for testing data
se.fore.ARIMA=predict(modelARIMA, 7)$se       #define standard error for forecasting result

#CALCULATE RMSE, MAE, AND MAPE CRITERIA
accuracies=matrix(0,3,2)
colnames(accuracies)=c("Training","Testing")
rownames(accuracies)=c("RMSE","MAE","MAPE")

accuracies[1,1]=accuracy(fits.ARIMA,training)[1,2]
accuracies[2,1]=accuracy(fits.ARIMA,training)[1,3]
accuracies[3,1]=accuracy(fits.ARIMA,training)[1,5]
accuracies[1,2]=accuracy(as.vector(fore.ARIMA),testing)[1,2]
accuracies[2,2]=accuracy(as.vector(fore.ARIMA),testing)[1,3]
accuracies[3,2]=accuracy(as.vector(fore.ARIMA),testing)[1,5]
accuracies

#CONSTRUCT INTERVAL PREDICTION
lower=fore.ARIMA-1.96*se.fore.ARIMA
upper=fore.ARIMA+1.96*se.fore.ARIMA

#COMPARISON BETWEEN ACTUAL AND FORECAST VALUE
a=min(min(fits.ARIMA),min(training))              #lower bound for training data
b=max(max(fits.ARIMA),max(training))              #upper bound for training data
c=min(min(fore.ARIMA),min(lower),min(testing))    #lower bound for testing data
d=max(max(fore.ARIMA),max(upper),max(testing))    #upper bound for testing data

par(mfrow=c(1,2),mar=c(2.3,2.7,1.2,0.4))  #the number of picture and its margin
par(mgp=c(1.3,0.5,0))                     #the distance between labels and axis

#PLOTTING FOR TRAINING DATA#
plot(as.ts(training),ylab="Yt",xlab="t",lwd=2,axes=F,ylim=c(a*0.9,b*1.1))
box()
title("Training",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=seq(1,319,7))
lines(as.ts(fits.ARIMA),col="red",lwd=2)

#PLOTTING FOR TESTING DATA#
plot(as.ts(testing),ylab="Yt",xlab="t",lwd=2,ylim=c(a*0.9,b*1.1),cex.lab=0.8,axes=F)
box()
title("Testing",line=0.3,cex.main=0.9)
axis(side=2,lwd=0.5,cex.axis=0.8,las=0)
axis(side=1,lwd=0.5,cex.axis=0.8,las=0,at=c(1:7),labels=c(320:326))
lines(as.vector(fore.ARIMA),col="red",lwd=2)
lines(as.vector(lower),col="blue2",lty="dotdash",lwd=2)
lines(as.vector(upper),col="blue2",lty="dotdash",lwd=2)

#DEFINE THE LEGEND#
legend("topright",c("Actual","Forecast","Upper Bound","Lower Bound"),
       col=c("black","red","blue2","blue2"),lwd=2,cex=0.7)


#intervensi
model.p <- arimax(y1, order=c(2,0,0), xtransf=p,  
                  transfer=list(c(0,2)), include.mean = FALSE)
model.p


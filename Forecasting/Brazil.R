# Dinesh Allimuthu
# Data: Brazil Import Goods in Millions
Y <- c(686,
       569,
       741,
       645,
       739,
       762,
       768,
       783,
       842,
       801,
       677,
       671,
       805,
       633,
       745,
       647,
       702,
       732,
       715,
       812,
       692,
       775,
       775,
       797,
       741,
       633,
       686,
       716,
       723,
       737,
       729,
       859,
       732,
       706,
       747,
       764)
X <- 1:36

# To check if you already installed the package or not, type in the following:

# any(grepl("<name of your package>", installed.packages()))
# example: any(grepl("xlsx ", installed.packages()))

## Linear Trend
plot(X,Y,pch=16, xlab=c('Index'), ylab=c('Brazil Goods (Millions)'), cex=1)
linReg <- lm(Y~X)
summary(linReg)
linReg$coefficients
abline(linReg$coefficients, col='red',lwd=3,lty=1)
res <- residuals(linReg)
plot(X,res,pch=16,ylab=c('Residuals'))
abline(h=0,lwd=3,lty=1)

# Normality of the interval
ks.test(res,"pnorm",mean(res),sd(res))

# confident and prediction interval
Xnew <- data.frame(x=seq(1,36,,36))
pred.w.clim<-predict(linReg,Xnew,interval="confidence",level=0.95)
pred.w.plim<-predict(linReg,Xnew,interval="prediction",level=0.95)
matplot(Xnew$x,cbind(pred.w.clim,pred.w.plim[,-1]),lty=c(1,2,2,3,3),
        type="l",col=c("black","blue","blue","red","red"),
        ylab="Import goods",xlab="Index",lwd=c(1,2,2,2,2))

legend("topleft",lty=2:3,col=c("blue","red"),lwd=c(2,2),
       c("95 confidence interval%","Prediction interval 95%"))
points(X,Y,col="black",pch=16,cex=1.5)




## Quadratic trend
# plot(X,Y, pch=16)
X2 <- X^2
QuadModel <-lm(Y ~ X + X2)
summary(QuadModel)
timevalues <- seq(1,36,,36)
predictedY <- predict(QuadModel,list(X=timevalues, X2=timevalues^2))
lines(timevalues,predictedY, lwd=3,lty=1, col='green')

legend(1990, 21.5, c("linear regression", "quadratic regression", "data"),
       col = c("red","green", "black"), text.col = "black",
       lty = c(1, 1, 0), pch=c(NA_integer_, NA_integer_, 16),
       lwd = c(3, 3, 2), text.font=1,box.lty=0, cex=1)

pred.w.clim<-predict(QuadModel,list(X=timevalues,X2=timevalues^2),interval="confidence",level=0.95)
pred.w.plim<-predict(QuadModel,list(X=timevalues,X2=timevalues^2),interval="prediction",level=0.95)
matplot(timevalues,cbind(pred.w.clim,pred.w.plim[,-1]),lty=c(1,2,2,3,3),
        type="l",col=c("black","blue","blue","red","red"),
        ylab="Import Goods",xlab="Index",lwd=c(1,2,2,2,2))

legend(1988,20,lty=2:3,col=c("blue","red"),
       c("95 confidence interval%","Prediction interval 95%"),box.lty = 0,text.font = 1)

points(X,Y,col="black",pch=16,cex=1.5)

# ---------------------- The End ----------------------- #


# Dinesh Allimuthu
# Data: the number of murders in the United States (in thousands) for the years intervals from 1985 to 1995.
Y <- c(19.5, 21.2, 20.8, 21.4, 21.9, 23.8, 25.1, 24.2, 25.2, 24.1, 22.7)
X <- 1985:1995

# To check if you already installed the package or not, type in the following:

# any(grepl("<name of your package>", installed.packages()))
# example: any(grepl("xlsx ", installed.packages()))

## Linear Trend
plot(X,Y,pch=16, xlab=c('Years'), ylab=c('Murders (Thousands)'), cex=1)
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
Xnew <- data.frame(x=seq(1985,1995,,11))
pred.w.clim<-predict(linReg,Xnew,interval="confidence",level=0.95)
pred.w.plim<-predict(linReg,Xnew,interval="prediction",level=0.95)
matplot(Xnew$x,cbind(pred.w.clim,pred.w.plim[,-1]),lty=c(1,2,2,3,3),
                     type="l",col=c("black","blue","blue","red","red"),
                     ylab="Murders",xlab="Year",lwd=c(1,2,2,2,2))
      
legend("topleft",lty=2:3,col=c("blue","red"),lwd=c(2,2),
       c("95 confidence interval%","Prediction interval 95%"))
points(X,Y,col="black",pch=16,cex=1.5)




## Quadratic trend
# plot(X,Y, pch=16)
X2 <- X^2
QuadModel <-lm(Y ~ X + X2)
summary(QuadModel)
timevalues <- seq(1985,1995,,50)
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
        ylab="Murders",xlab="Year",lwd=c(1,2,2,2,2))

legend(1988,20,lty=2:3,col=c("blue","red"),
       c("95 confidence interval%","Prediction interval 95%"),box.lty = 0,text.font = 1)

points(X,Y,col="black",pch=16,cex=1.5)

# ---------------------- The End ----------------------- #


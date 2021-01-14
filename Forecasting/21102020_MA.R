"
Created on Tue Oct 24 16:00:00 2020

@author: Dinesh A
"

"
------->--Please do INSTALL/LOAD required packages as below--<-------
  install.packages('zoo')
  library(zoo)
"


# to check package already install or not.
# any(grep("zoo", installed.packages()))

# set & get the working directory.
setwd(dir = "H:/Ad/Education/Codes/R/FM")
getwd()

# read the input file.
my_data <- read.csv("Murders2.csv")


# deduce the moving average using the library - zoo.
ma4 <- rollmean(
  my_data$Murders,
  k = 4,
  fill = NA,
  align = "center"
)


ma5 <- rollmean(
  my_data$Murders,
  k = 5,
  fill = NA,
  align = "center"
)


# deduce the centered moving average.
centered_ma <- function(x, q) {
  # x is data and q is width of the moving window
  
  n <- length(x)
  ma <- rep(NA, n) # It just gives zeros n times.
  m <- q %/% 2  # Have to more careful its " % / % " Always takes the floor value.
  if (q %% 2 == 0) {
    for (t in ((m + 1):(n - m))) {
      val_ma <- 0.5 * x[t - m]
      for (i in ((-m + 1):(m - 1))) {
        val_ma = val_ma + x[t + i]
      }
      val_ma <- val_ma + 0.5 * x[t + m]
      ma[t] = val_ma / q
    }
  }
  else {
    for (t in ((m + 1):(n - m))) {
      val_ma = 0.0
      for (i in ((-m):m)) {
        val_ma = val_ma + x[t + i]
      }
      ma[t] = val_ma / q
    }
  }
  return(ma)
}
q = 4 # q is width of the window
ca4 <- centered_ma(my_data$Murders, q)



"
lines(X4MV,Y4MV, lwd=2,col='green')
lines(X5MV, Y5MV,lwd=2, col='blue')
lines(X4CMV, Y4CMV, lwd=2, col='red')
"

my_data$ma4 <- ma4
my_data$ma5 <- ma5
my_data$ca4 <- ca4
View(my_data)

# plot the data points against ma=4, ma=5 and centered moving = 4.
plot(my_data$Year,my_data$Murders,pch=16, xlab=c('Years'), ylab=c('Murders (Thousands)'), cex=1)
lines(my_data$Year+1, ma4, lwd=2,col='green') # to start from 1987 - used year + 1 to plot.
lines(my_data$Year, ma5, lwd=2, col='blue') # default start from 1987
lines(my_data$Year, ca4, lwd=2, col='red') # default start from 1987
legend(1985,24, col=c("black", "green", "red","blue"),
       c("Data", "4-year MA", "4-year centered MA", "5-year MA"), box.lty=1,
       lty=c(0,1,1,1), cex=0.7,
       text.font=1, lwd=c(0,2,2,2), pch=c(16,NA_integer_, NA_integer_, NA_integer_))


# graph 5-year MA vs data
plot(my_data$Year,my_data$Murders,pch=16,type='l', xlab=c('Years'), ylab=c('Murders (Thousands)'), cex=1)
lines(my_data$Year, ma5,  lty=2,lwd=2, col='blue')
legend(1985,24, col=c("black", "blue"),
       c("Data", "5-year MA"), box.lty=1,
       lty=c(1,2), cex=1,
       text.font=1, lwd=c(2,2))




 # ---------------------------------The End-----------------------------
  



#--------------Sir work--------------

X <- 1985:1995
Y <- c(19.0, 20.6, 20.1, 20.7, 21.5, 23.4, 24.7, 23.8, 24.5, 23.3, 21.6)

rollmean(Y,5)
rollmean(Y,4)

X4MV <- 1987:1994
Y4MV <- c(20.1, 20.725, 21.425, 22.575, 23.350, 24.1, 24.075, 23.3)

X4CMV <- 1987:1993
Y4CMV <- c(20.413, 21.075, 22, 22.963, 23.725, 24.088, 23.688)

X5MV <- 1987:1993
Y5MV <- c(20.38, 21.26, 22.08, 22.82, 23.58, 23.94, 23.58)


plot(X,Y,pch=16, xlab=c('Years'), ylab=c('Murders (Thousands)'), cex=1)
lines(X4MV,Y4MV, lwd=2,col='green')
lines(X5MV, Y5MV,lwd=2, col='blue')
lines(X4CMV, Y4CMV, lwd=2, col='red')
legend(1989,21, 
       col=c("black", "black", "red","blue"), 
       c("Data", "4-year MA", "4-year centered MA","5-year MA"),
       box.lty=0,lty=c(0,1,1,1), cex=1,text.font=1, lwd=c(0, 2,2,2), 
       pch=c(16,NA_integer_, NA_integer_, NA_integer_))

points(X,Y,col="black", pch=16, cex=1.5)

# Graph 5-year MA vs data
X <- 1985:1995
Y <- c(19.0, 20.6, 20.1, 20.7, 21.5, 23.4, 24.7, 23.8, 24.5, 23.3, 21.6)
plot(X,Y, type='l', xlab=c('Years'), ylab=c('Murders (Thousands)'), lwd=2)
Y5MV <- c(20.38, 21.26, 22.08, 22.82, 23.58, 23.94, 23.58)
X5MV <- 1987:1993
lines(X5MV, Y5MV, lty=2, lwd=2)
legend(1989,21, col=c("black","black"),c("Data", "5-year MA"), box.lty=0, lty=c(1,2), cex=1,text.font=1,lwd=c(2,2))






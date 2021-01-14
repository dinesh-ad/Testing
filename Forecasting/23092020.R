setwd(dir = "H:/Ad/Education/Codes/R/FM")
getwd()
my_data <- read.csv("Murders2.csv")

# Linear
plot(my_data$Year,my_data$Murders, main='Year and Murders', xlab='Year',ylab='Murders',col="red")
a1<-cov(my_data$Year, my_data$Murders)/var(my_data$Year) #Slope
a0<-mean(my_data$Murders)-a1*mean(my_data$Year)
print(a1)
print(a0)
modlin <- lm(my_data$Year~my_data$Murders)
abline(lm(my_data$Murders ~ my_data$Year))
print(modlin)
abline(modlin)

# Parabola - Quadratic
plot(my_data$Year,my_data$Murders, main='Year and Murders', xlab='Year',ylab='Murders',col="red",pch=16)
Murders2 <- my_data$Murders^2
#fit quadratic regression model
quadraticModel <- lm(my_data$Murders ~ Year + Murders2, data=my_data)
#view model summary
summary(quadraticModel)
# Call:
# lm(formula = Murders ~ Year + Murders2, data = my_data)

#create sequence of hour values
Seq_Values <- seq(0,15,,50)

#create list of predicted happiness levels using quadratic model
happinessPredict <- predict(quadraticModel,list(Year=Seq_Values, Year2=Seq_Values^2))

# create scatterplot of original data values
# plot(my_data$Year, my_data$Murders,main='Year and Murders', xlab='Year',ylab='Murders',col="red",pch=16)
# add predicted lines based on quadratic regression model
lines(Seq_Values, happinessPredict, col='blue')


# 30092020

res <- residuals(modlin)















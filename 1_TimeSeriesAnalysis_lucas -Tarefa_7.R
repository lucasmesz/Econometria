## p 8
install.packages("zoo")
library("zoo")


## p 11
install.packages("forecast")
library("forecast")
library(readxl)

UKHP_z = read_excel("C:/Users/Mesz/Desktop/ukhp/UKHP.xls", col_types = c("date", "numeric"))
hp <- read.zoo(UKHP_z, , sep = " ", header = TRUE, format = "%Y-%m", FUN = as.yearmon)

frequency(hp)

hp_ret <- diff(hp) / lag(hp, k = -1) * 100

plot(hp)

plot(hp_ret)

mod <- auto.arima(hp_ret, stationary = TRUE, seasonal = FALSE, ic="aic")

## p 12
mod
confint(mod)
tsdiag(mod)

## p 13

plot(mod$x, lty = 1, main = "UK house prices: raw data vs. fitted values", ylab = "Return in percent", xlab = "Date",)
lines(fitted(mod), lty = 2,lwd = 2, col = "red")


## p 14
accuracy(mod)

predict(mod, n.ahead=3)

plot(forecast(mod, h=36))

plot(predict(mod, n.ahead=3))

#####################################################################
####ARIMA(1,0,0)###########################################################3


#mod <- auto.arima(hp_ret, stationary = TRUE, seasonal = FALSE, ic="aic")
mod_100 = arima(hp_ret,order = c(1,0,0))
## p 12
mod_100
confint(mod_100)
tsdiag(mod_100)

## p 13

plot(mod$x, lty = 1, main = "UK house prices: raw data vs. fitted values mod_100", ylab = "Return in percent", xlab = "Date",)
lines(fitted(mod_100), lty = 2,lwd = 2, col = "red")


## p 14
accuracy(mod_100)

predict(mod_100, n.ahead=3)

plot(forecast(mod_100, h=36))

plot(predict(mod, n.ahead=3))



dm.test(residuals(mod),residuals(mod_100),h=10)

dm.test(residuals(mod),residuals(mod_100),alternative="less", h=10)

dm.test(residuals(mod),residuals(mod_100),alternative="greater", h=10)

#####################################################################
####split de data###########################################################3



train_size=as.integer(0.8*length(hp_ret))
first_test=train_size +1

hp_ret_train=hp_ret[1:train_size]
hp_ret_test=hp_ret[first_test:length(hp_ret)]

plot(hp_ret,type="n",xlab="Ano",ylab = "Retorno",main = 'Treino 0.8, Teste 0.2')
lines(hp_ret_train,col="black", )
lines(hp_ret_test,col="red")
legend("bottomright",c("train","test"), col = c("black","red"),lty=1, cex=0.8)


mod_train <- auto.arima(hp_ret_train, stationary = TRUE, seasonal = FALSE, ic="aic")

## p 12
mod_train
confint(mod_train)
tsdiag(mod_train)

## p 13

plot(mod$x, lty = 1, main = "UK house prices: raw data vs. fitted values", ylab = "Return in percent", xlab = "Date",)
lines(fitted(mod_train), lty = 2,lwd = 2, col = "red")


## p 14
accuracy(mod_train)

hp_ret_test_df = data.frame(hp_ret_test)
##Creating the test forecast
forecast_test=forecast(mod_train, h=length(hp_ret_test)) 
accuracy(f=forecast_test,x=hp_ret_test_df$hp_ret_test)

sqrt(mean(forecast_test$mean - hp_ret_test_df$hp_ret_test)^2)

#####
plot(hp_ret_test, lty = 1, main = "UK house prices: raw data vs. forecast mean", ylab = "Return in percent", xlab = "Date",) 
lines(forecast_test$mean, lty = 2,lwd = 2, col = "red")
legend("bottomright",c("série","média"), col = c("black","red"),lty=1, cex=0.8)


first_plot=first_test+1

plot(mod$x, lty = 1, main = "UK house prices: raw data vs. forecast mean", ylab = "Return in percent", xlab = "Date",) 
lines(forecast_test$mean, lty = 2,lwd = 2, col = "red")
legend("bottomright",c("série","média"), col = c("black","red"),lty=1, cex=0.8)


plot(forecast_test)


###################################Outra forma#######
train_size=as.integer(0.8*length(hp_ret))
first_test=train_size +1


training = window(hp_ret, end=index(hp_ret[train_size]))
test= window(hp_ret,start= index(hp_ret[first_test]))

#modelo auto.arima####
modelo_auto = auto.arima(training, stationary = TRUE, seasonal = FALSE, ic="aic")
tsdiag(modelo_auto)
fc_auto=forecast(modelo_auto,h = length(test))
#fc_auto=Arima(test,model=modelo_auto) modo de gerar acurácia só do teste
#accuracy(fc_auto)
#autoplot(fc) 

###modelo auto.arima.overfitting####
#modelo_over=auto.arima(training)
#tsdiag(modelo_over)
#fc_over=forecast(modelo_over,h = length(test))

###modelo AR(1,0,0)####
modelo_ar100=arima(training,order = c(1,0,0))
tsdiag(modelo_ar100)
fc_ar100=forecast(modelo_ar100,h = length(test))

minimo=min(accuracy(fc_auto,test)['Test set','RMSE'],
    
    accuracy(fc_ar100,test)['Test set','RMSE'])


accuracy(fc_auto,test)
#accuracy(fc_over,test)
accuracy(fc_ar100,test)


dm.test(residuals(Arima(test, model=fc_auto$model)), 
        residuals(Arima(test, model=fc_ar100$model)),h=10)

dm.test(residuals(Arima(training, model=fc_auto$model)), 
        residuals(Arima(training, model=fc_ar100$model)),h=10)


dm.test(residuals(Arima(training, model=fc_auto$model)), 
        residuals(Arima(training, model=fc_ar100$model)),alternative="less",h=10)


###Rolling methods
install.packages("ggplot2")
library(ggplot2)

# Compute cross-validated errors for up to 8 steps ahead

for(h in 1:8){
  far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
  e <- tsCV(hp_ret, far2, h=h)
  
}

# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)


for(h in 1:8){
  far2 <- function(x, h){forecast(Arima(x, order=c(1,0,0)), h=h)}
  e1 <- tsCV(hp_ret, far2, h=h)
  
}

# Compute the MSE values and remove missing values
mse1 <- colMeans(e1^2, na.rm = TRUE)

plot(mse)
plot(mse1)

# Plot the MSE values against the f



#Fit an AR(2) model to each rolling origin subset
far2 <- function(x, h){forecast(Arima(x, order=c(2,0,0)), h=h)}
e <- tsCV(hp_ret, far2, h=1)





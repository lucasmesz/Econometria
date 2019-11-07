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


##Creating the test forecast
forecast_test=forecast(mod_train, h=length(hp_ret_test)) 
accuracy(f=forecast_test,x=hp_ret_test)

first_plot=first_test+1

plot(mod$x, lty = 1, main = "UK house prices: raw data vs. forecast mean", ylab = "Return in percent", xlab = "Date",) 
lines(forecast_test$mean, lty = 2,lwd = 2, col = "red")
legend("bottomright",c("série","média"), col = c("black","red"),lty=1, cex=0.8)


plot(forecast_test)



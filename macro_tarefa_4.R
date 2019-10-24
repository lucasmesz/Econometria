########Install packages####
install.packages("readxl")
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("lm_msoft")
install.packages("moments")
###########################


library(readxl)
macro_lucas<- read_excel("Y:/Chrisbrooks4ed/Exercicio/macro_lucas.xls", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
View(macro_lucas)
macro_lucas$dspread = c(NA,diff(macro_lucas$BMINUSA))
macro_lucas$dcredit = c(NA,diff(macro_lucas$CCREDIT))
macro_lucas$dprod = c(NA,diff(macro_lucas$INDPRO))
macro_lucas$dmoney = c(NA,diff(macro_lucas$M1SUPPLY))
macro_lucas$inflation = c(NA,100*diff(log(macro_lucas$CPI)))
macro_lucas$rterm = c(NA,diff(macro_lucas$USTB10Y-macro_lucas$USTB3M))
macro_lucas$dinflation = c(NA,100*diff(macro_lucas$inflation))

#Premio de Risco de Mercado
macro_lucas$rsandp = c(NA,100*diff(log(macro_lucas$SANDP)))
macro_lucas$ersandp = macro_lucas$rsandp-macro_lucas$USTB3M/12

###########################################MICROSOFT#############
# Retornos dos ativos Microsoft
macro_lucas$ermsoft = c(NA,100*diff(log(macro_lucas$MICROSOFT)))-macro_lucas$USTB3M/12

#Modelo MMQ#

lm_msoft = lm(ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm, data = macro_lucas)
summary(lm_msoft)




library(car)
linearHypothesis(lm_msoft,c("dprod=0","dcredit=0","dmoney=0","dspread=0", "dinflation=0", "rterm=0"))


lm_start = lm(ermsoft~1,data=macro_lucas[-(1:2),])
step(lm_start,direction = "forward",scope = formula(lm_msoft))

############################## QUANTILE_MICROSOFT ##############################
library(quantreg)
qreg = rq(ermsoft ~ ersandp, data =macro_lucas)
summary(qreg)
qreg = rq(ermsoft ~ ersandp, data =macro_lucas, tau=seq(0.1,0.9,0.1))

plot(summary(qreg), level = 0.95)



################################FORD################################

macro_lucas$dspread = c(NA,diff(macro_lucas$BMINUSA))
macro_lucas$dcredit = c(NA,diff(macro_lucas$CCREDIT))
macro_lucas$dprod = c(NA,diff(macro_lucas$INDPRO))
macro_lucas$dmoney = c(NA,diff(macro_lucas$M1SUPPLY))
macro_lucas$inflation = c(NA,100*diff(log(macro_lucas$CPI)))
macro_lucas$rterm = c(NA,diff(macro_lucas$USTB10Y-macro_lucas$USTB3M))
macro_lucas$dinflation = c(NA,100*diff(macro_lucas$inflation))

#Premio de Risco de Mercado
macro_lucas$rsandp = c(NA,100*diff(log(macro_lucas$SANDP)))
macro_lucas$ersandp = macro_lucas$rsandp-macro_lucas$USTB3M/12

# Retornos dos ativos FORD
macro_lucas$erford = c(NA,100*diff(log(macro_lucas$FORD)))-macro_lucas$USTB3M/12

#Modelo MMQ#

lm_ford = lm(erford ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm, data = macro_lucas)
summary(lm_ford)


library(car)
linearHypothesis(lm_ford,c("dcredit=0","dmoney=0",  "rterm=0"))


lm_start = lm(erford~1,data=macro_lucas[-(1:2),])
step(lm_start,direction = "forward",scope = formula(lm_ford))


############################## QUANTILE_FORD##################################
library(quantreg)
qreg = rq(erford ~ ersandp, data =macro_lucas)
summary(qreg)
qreg = rq(erford ~ ersandp, data =macro_lucas, tau=seq(0.1,0.9,0.1))

plot(summary(qreg), level = 0.95)




################################GE#######################################


macro_lucas$dspread = c(NA,diff(macro_lucas$BMINUSA))
macro_lucas$dcredit = c(NA,diff(macro_lucas$CCREDIT))
macro_lucas$dprod = c(NA,diff(macro_lucas$INDPRO))
macro_lucas$dmoney = c(NA,diff(macro_lucas$M1SUPPLY))
macro_lucas$inflation = c(NA,100*diff(log(macro_lucas$CPI)))
macro_lucas$rterm = c(NA,diff(macro_lucas$USTB10Y-macro_lucas$USTB3M))
macro_lucas$dinflation = c(NA,100*diff(macro_lucas$inflation))

#Premio de Risco de Mercado
macro_lucas$rsandp = c(NA,100*diff(log(macro_lucas$SANDP)))
macro_lucas$ersandp = macro_lucas$rsandp-macro_lucas$USTB3M/12


#Retornos dos ativos GE#

macro_lucas$erge = c(NA,100*diff(log(macro_lucas$GE)))-macro_lucas$USTB3M/12

#Modelo MMQ#

lm_ge = lm(erge ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm, data = macro_lucas)
summary(lm_ge)



library(car)
linearHypothesis(lm_ge,c("dprod=0","dcredit=0","dmoney=0","dspread=0", "dinflation=0", "rterm=0"))


lm_start = lm(erge~1,data=macro_lucas[-(1:2),])
step(lm_start,direction = "forward",scope = formula(lm_ge))

############################## QUANTILE_GE##############################
library("quantreg")
qreg = rq(erge ~ ersandp, data =macro_lucas)
summary(qreg)
qreg = rq(erge ~ ersandp, data =macro_lucas, tau=seq(0.1,0.9,0.1))

plot(summary(qreg), level = 0.95)





##############################Microsoft################################


############################## Diagnosis tests ##############################
############################## Diagnosis tests ##############################

plot(macro_lucas$Date[-(1:3)],lm_msoft$residuals,type = "l",xlab="",ylab="")
library(lmtest)
bptest(formula(lm_msoft),data = macro_lucas,studentize = F)
bptest(formula(lm_msoft),data = macro_lucas,studentize = T)

library(sandwich)

coeftest(lm_msoft,vcov. = vcovHC(lm_msoft,type="HC1"))
coeftest(lm_msoft,vcov. = NeweyWest(lm_msoft,lag = 6,adjust = T,prewhite = F))

dwtest(lm_msoft)
bgtest(lm_msoft,order = 10)

############################## Residual normality ##############################

hist(lm_msoft$residuals,main = "", nclass = 30)
box()

library(moments)
skewness(lm_msoft$residuals)

kurtosis(lm_msoft$residuals)

jarque.test(lm_msoft$residuals)
agostino.test(lm_msoft$residuals)
anscombe.test(lm_msoft$residuals)

plot(macro_lucas$Date[-(1:3)],lm_msoft$residuals,type = "l", col="red",xlab="",ylab="")
lines(macro_lucas$Date[-(1:3)],lm_msoft$fitted.values)
legend("bottomright",c("Residuals","Fitted"), col = c("red","black"),lty=1, cex=0.5)
sort(lm_msoft$residuals)


boxplot(lm_msoft$residuals)

############################## Dummy variables ##############################

####Forma de sistematizar a Dummy########

#Dummies de mínimos
macro_lucas$Date = as.Date(macro_lucas$Date)
datas <- c(macro_lucas$Date[order(lm_msoft$residuals)[1:2]+2])
for (i in 1:length(datas))
  {macro_lucas[[paste0('DUM',months(datas[i]))]]<-as.integer(macro_lucas$Date == as.Date(datas[i]))}


#Dummies de máximos

datas_d <- c(macro_lucas$Date[order(lm_msoft$residuals, decreasing=T)[1:2]+2])
for (i in 1:length(datas_d))
  {macro_lucas[[paste0('DUM_d_',months(datas_d[i]))]]<-as.integer(macro_lucas$Date == as.Date(datas_d[i]))}



lm_micro_dummy = lm(ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney+
          dspread + rterm + DUMfevereiro +DUMjulho +DUM_d_outubro +DUM_d_abril , data = macro_lucas)
summary(lm_micro_dummy)

jarque.test(lm_micro_dummy$residuals)
agostino.test(lm_micro_dummy$residuals)
anscombe.test(lm_micro_dummy$residuals)

cor(macro_lucas[-(1:3),c("dprod","dcredit","dinflation","dmoney","dspread","rterm")])
resettest(lm_msoft,power = 2:4)


#######################################FORD#######################################


############################## Diagnosis tests ##############################
############################## Diagnosis tests ##############################

plot(macro_lucas$Date[-(1:3)],lm_ford$residuals,type = "l",xlab="",ylab="")
library(lmtest)
bptest(formula(lm_ford),data = macro_lucas,studentize = FALSE)
bptest(formula(lm_ford),data = macro_lucas,studentize = TRUE)

library(sandwich)

coeftest(lm_ford,vcov. = vcovHC(lm_ford,type="HC1"))
coeftest(lm_ford,vcov. = NeweyWest(lm_ford,lag = 6,adjust = TRUE,prewhite = FALSE))

dwtest(lm_ford)
bgtest(lm_ford,order = 10)

############################## Residual normality ##############################

hist(lm_ford$residuals,main = "", nclass = 20)
box()

library(moments)
skewness(lm_ford$residuals)

kurtosis(lm_ford$residuals)

jarque.test(lm_ford$residuals)
agostino.test(lm_ford$residuals)
anscombe.test(lm_ford$residuals)

plot(macro_lucas$Date[-(1:3)],lm_ford$residuals,type = "l", col="red",xlab="",ylab="")
lines(macro_lucas$Date[-(1:3)],lm_ford$fitted.values)
legend("bottomright",c("Residuals","Fitted"), col = c("red","black"),lty=1, cex=0.5)
sort(lm_ford$residuals)
boxplot(lm_ford$residuals)




############################## Dummy variables ##############################

####Forma de sistematizar a Dummy########

#Dummies de mínimos
macro_lucas$Date = as.Date(macro_lucas$Date)
datas <- c(macro_lucas$Date[order(lm_ford$residuals)[1:2]+2])
for (i in 1:length(datas))
{macro_lucas[[paste0('DUM',months(datas[i]))]]<-as.integer(macro_lucas$Date == as.Date(datas[i]))}


#Dummies de máximos

datas_d <- c(macro_lucas$Date[order(lm_ford$residuals, decreasing=T)[1:2]+2])
for (i in 1:length(datas_d))
{macro_lucas[[paste0('DUM_d_',months(datas_d[i]))]]<-as.integer(macro_lucas$Date == as.Date(datas_d[i]))}


lm_ford_dummy = lm(erford ~ ersandp + dprod + dcredit + dinflation + dmoney+
                      dspread + rterm + DUMoutubro +DUM_d_abril+DUM_d_novembro, data = macro_lucas)
summary(lm_ford_dummy)

jarque.test(lm_ford_dummy$residuals)
agostino.test(lm_ford_dummy$residuals)
anscombe.test(lm_ford_dummy$residuals)


cor(macro_lucas[-(1:3),c("dprod","dcredit","dinflation","dmoney","dspread","rterm")])
resettest(lm_ford,power = 2:4)



#######################################GE#########################################


############################## Diagnosis tests ##############################
############################## Diagnosis tests ##############################

plot(macro_lucas$Date[-(1:3)],lm_ge$residuals,type = "l",xlab="",ylab="")
library(lmtest)
bptest(formula(lm_ge),data = macro_lucas,studentize = FALSE)
bptest(formula(lm_ge),data = macro_lucas,studentize = TRUE)

library(sandwich)

coeftest(lm_ge,vcov. = vcovHC(lm_ge,type="HC1"))
coeftest(lm_ge,vcov. = NeweyWest(lm_ge,lag = 6,adjust = TRUE,prewhite = FALSE))

dwtest(lm_ge)
bgtest(lm_ge,order = 10)

############################## Residual normality ##############################

hist(lm_ge$residuals,main = "", nclass = 35)
box()

library(moments)
skewness(lm_ge$residuals)

kurtosis(lm_ge$residuals)

jarque.test(lm_ge$residuals)
agostino.test(lm_ge$residuals)
anscombe.test(lm_ge$residuals)

plot(macro_lucas$Date[-(1:3)],lm_ge$residuals,type = "l", col="red",xlab="",ylab="")
lines(macro_lucas$Date[-(1:3)],lm_ge$fitted.values)
legend("bottomright",c("Residuals","Fitted"), col = c("red","black"),lty=1, cex=0.5)
sort(lm_ge$residuals)

boxplot(lm_ge$residuals)





####Forma de sistematizar a Dummy########

#Dummies de mínimos
macro_lucas$Date = as.Date(macro_lucas$Date)
datas <- c(macro_lucas$Date[order(lm_ge$residuals)[1:2]+2])
for (i in 1:length(datas))
{macro_lucas[[paste0('DUM',months(datas[i]))]]<-as.integer(macro_lucas$Date == as.Date(datas[i]))}


#Dummies de máximos

datas_d <- c(macro_lucas$Date[order(lm_ge$residuals, decreasing=T)[1:2]+2])
for (i in 1:length(datas_d))
{macro_lucas[[paste0('DUM_d_',months(datas_d[i]))]]<-as.integer(macro_lucas$Date == as.Date(datas_d[i]))}



lm_ge_dummy = lm(erge ~ ersandp + dprod + dcredit + dinflation + dmoney+
                     dspread + rterm + DUMoutubro +DUMabril +DUM_d_julho +DUM_d_setembro , data = macro_lucas)
summary(lm_ge_dummy)

jarque.test(lm_ge_dummy$residuals)
agostino.test(lm_ge_dummy$residuals)
anscombe.test(lm_ge_dummy$residuals)

cor(macro[-(1:3),c("dprod","dcredit","dinflation","dmoney","dspread","rterm")])
resettest(lm_msoft,power = 2:4)


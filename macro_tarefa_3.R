########Install packages####
install.packages("readxl")
install.packages("car")
install.packages("lmtest")
install.packages("sandwich")
install.packages("lm_msoft")
install.packages("moments")
###########################


library(readxl)
macro_lucas<- read_excel("c:/Users/Mesz/Desktop/Econometria-master/macro_lucas.xls", 
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


boxplot(lm_msoft$residuals)





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



#################################FORD################################
############################## Dummy variables ##############################

macro$Date = as.Date(macro$Date)
macro$APR00DUM = as.integer(macro$Date == as.Date("2000-04-01"))
macro$DEC00DUM = as.integer(macro$Date == as.Date("2000-12-01"))

lm_dummy = lm(ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney+
                dspread + rterm + APR00DUM + DEC00DUM, data = macro)
summary(lm_dummy)

jarque.test(lm_dummy$residuals)
agostino.test(lm_dummy$residuals)
anscombe.test(lm_dummy$residuals)

############################## Dummy variables ##############################


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

hist(lm_ford$residuals,main = "")
box()

library(moments)
skewness(lm_ford$residuals)

kurtosis(lm_ford$residuals)

jarque.test(lm_ford$residuals)
agostino.test(lm_ford$residuals)
anscombe.test(lm_ford$residuals)

plot(macro_lucas$Date[-(1:3)],lm_ford$residuals,type = "l", col="red",xlab="",ylab="")
lines(macro_lucas$Date[-(1:3)],lm_ford$fitted.values)
legend("bottomright",c("Residuals","Fitted"), col = c("red","black"),lty=1)
sort(lm_ford$residuals)
boxplot(lm_ford$residuals)


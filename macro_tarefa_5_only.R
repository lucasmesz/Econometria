library(readxl)
macro <- read_excel("C:/Users/Mesz/Documents/Mestrado/IAG-PUC/Chrisbrooks4ed/excelfiles/macro.xls")
View(macro)

macro$dspread = c(NA,diff(macro$BMINUSA))
macro$dcredit = c(NA,diff(macro$CCREDIT))
macro$dprod = c(NA,diff(macro$INDPRO))
macro$dmoney = c(NA,diff(macro$M1SUPPLY))
macro$inflation = c(NA,100*diff(log(macro$CPI)))
macro$rterm = c(NA,diff(macro$USTB10Y-macro$USTB3M))
macro$dinflation = c(NA,100*diff(macro$inflation))

macro$rsandp = c(NA,100*diff(log(macro$SANDP)))
macro$ermsoft = c(NA,100*diff(log(macro$MICROSOFT)))-macro$USTB3M/12
macro$ersandp = macro$rsandp-macro$USTB3M/12

lm_msoft = lm(ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm, data = macro)
summary(lm_msoft)
library(car)
linearHypothesis(lm_msoft,c("dprod=0","dcredit=0","dmoney=0","dspread=0"))

lm_start = lm(ermsoft~1,data=macro[-(1:2),])
step(lm_start,direction = "forward",scope = formula(lm_msoft))

############################## Diagnosis tests ##############################

plot(macro$Date[-(1:2)],lm_msoft$residuals,type = "l",xlab="",ylab="")
library(lmtest)
bptest(formula(lm_msoft),data = macro,studentize = F)
bptest(formula(lm_msoft),data = macro,studentize = T)

library(sandwich)
coeftest(lm_msoft,vcov. = vcovHC(lm_msoft,type="HC1"))
coeftest(lm_msoft,vcov. = NeweyWest(lm_msoft,lag = 6,adjust = T,prewhite = F))

dwtest(lm_msoft)
bgtest(lm_msoft,order = 10)

############################## Residual normality ##############################

hist(lm_msoft$residuals,main = "")
box()

library(moments)
skewness(lm_msoft$residuals)

kurtosis(lm_msoft$residuals)

jarque.test(lm_msoft$residuals)
agostino.test(lm_msoft$residuals)
anscombe.test(lm_msoft$residuals)

plot(macro$Date[-(1:2)],lm_msoft$residuals,type = "l", col="red",xlab="",ylab="")
lines(macro$Date[-(1:2)],lm_msoft$fitted.values)
legend("bottomright",c("Residuals","Fitted"), col = c("red","black"),lty=1)
sort(lm_msoft$residuals)
boxplot(lm_msoft$residuals)
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

cor(macro[-(1:2),c("dprod","dcredit","dinflation","dmoney","dspread","rterm")])
resettest(lm_msoft,power = 2:4)

############################## Structural break tests #######################

library(strucchange)
sbtest = Fstats(formula(lm_msoft),data = macro)

jan96 = match(as.Date("1996-01-01"),macro$Date)
chow = sbtest$Fstats[jan96-2-57]
1-pchisq(chow,sbtest$nreg)

sctest(sbtest)
bp = which.max(sbtest$Fstats)+59
macro$Date[bp]

beta = NULL
for (t in 20:nrow(macro)){
  lr = summary(lm(formula(lm_msoft), data = macro[3:t,]))
  beta = rbind(beta,lr$coefficients["ersandp",1:2])
}

x_axis = macro$Date[20:nrow(macro)]
plot(x_axis,beta[,1],type = "l",ylim = c(0,3),xlab="",ylab="Beta")
lines(x_axis,beta[,1]+2*beta[,2],lty="dashed")
lines(x_axis,beta[,1]-2*beta[,2],lty="dashed")
  

############################## Structural break tests - Data #######################


plot(efp(lm_msoft,data=macro))

CUMSUM=efp(lm_msoft, data=macro)

bound.CUMSUM <- boundary(CUMSUM, alpha=0.05)

plot(range(1:length(CUMSUM$process)),range(c(bound.CUMSUM, -bound.CUMSUM)),type="n",xlab="amostras",ylab = "Empirical fluctuation process")
lines(1:length(CUMSUM$process),CUMSUM$process)
lines(1:length(CUMSUM$process),bound.cumsum,col="red")
lines(1:length(CUMSUM$process),-bound.cumsum,col="red")


logica= (cumsum$process < -bound.cumsum) | (cumsum$process > bound.cumsum)


breaking=min(which(logica==TRUE))
#São 385 amostras em Macro 
#Resíduo do lmsoft tem 383 amostras (2 a menos, devido ao diff e ln da inflação)
# efp$process tem 376 (7 a menos q o resíduo 2 -5 regressores?)
macro$Date[breaking+1+ 2+7]

breaking_2=which(cumsum(logica)>5)[1]

breaking_3=which(cumsum(logica)>10)[1]

macro$Date[breaking_2 +1+ 2+7]

macro$Date[breaking_3+1+ 2+7]


plot(range(macro$Date[10:length(macro$Date)]),range(c(bound.CUMSUM, -bound.CUMSUM)),type="n",xlab="Ano",ylab = "Empirical fluctuation process")
lines(macro$Date[10:length(macro$Date)],CUMSUM$process)
lines(macro$Date[10:length(macro$Date)],bound.cumsum,col="red")
lines(macro$Date[10:length(macro$Date)],-bound.cumsum,col="red")

###################################################################

############################## Simultaneous Equation #######################

library(AER)
inf_iv = ivreg(inflation ~ rsandp + dprod + dcredit + dmoney |
                 dcredit + dprod + rterm + dspread + dmoney, data = macro)
summary(inf_iv)
ret_iv = ivreg(rsandp ~ inflation + dprod + dspread + rterm |
                 dcredit + dprod + rterm + dspread + dmoney, data = macro)
summary(ret_iv)

inf_ols = lm(inflation ~ dprod + dspread + rterm +  dcredit + dmoney, data = macro)
ret_ols = lm(rsandp ~ dprod + dspread + rterm +  dcredit + dmoney, data = macro)
macro$inffit = c(NA,inf_ols$fitted.values)
macro$retfit = c(NA,ret_ols$fitted.values)

summary(lm(inflation ~ dprod + dcredit + dmoney + rsandp + retfit, data = macro))
summary(lm(rsandp ~ dprod + dspread + rterm + inflation + inffit, data = macro))

############################## Dummy Jnadum #######################

require(lubridate)
macro$JANDUM = as.integer(month(macro$Date) == 1)

summary(lm(ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + 
             rterm + APR00DUM + DEC00DUM +JANDUM, data = macro))



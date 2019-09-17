library(readxl)
macro_lucas<- read_excel("Y:/Chrisbrooks4ed/excelfiles/macro_lucas.xls", 
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
library("quantreg")
qreg = rq(ermsoft ~ ersandp, data =macro_lucas)
summary(qreg)
qreg = rq(ermsoft ~ ersandp, data =macro_lucas, tau=seq(0.1,0.9,0.1))

plot(summary(qreg), level = 0.95)




################################FORD################################
# Retornos dos ativos FORD
macro_lucas$erford = c(NA,100*diff(log(macro_lucas$FORD)))-macro_lucas$USTB3M/12



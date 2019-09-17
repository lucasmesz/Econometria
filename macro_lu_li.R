library(readxl)
macro_lu_li<- read_excel("C:/Users/lucas_mesz/Desktop/Econometria-master/macro_lu_li.xls", 
                          col_types = c("date", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric", "numeric", "numeric", 
                                        "numeric"))
View(macro_lu_li)
macro_lu_li$dspread = c(NA,diff(macro_lu_li$BMINUSA))
macro_lu_li$dcredit = c(NA,diff(macro_lu_li$CCREDIT))
macro_lu_li$dprod = c(NA,diff(macro_lu_li$INDPRO))
macro_lu_li$dmoney = c(NA,diff(macro_lu_li$M1SUPPLY))
macro_lu_li$inflation = c(NA,100*diff(log(macro_lu_li$CPI)))
macro_lu_li$rterm = c(NA,diff(macro_lu_li$USTB10Y-macro_lu_li$USTB3M))
macro_lu_li$dinflation = c(NA,100*diff(macro_lu_li$inflation))

#Premio de Risco de Mercado
macro_lu_li$rsandp = c(NA,100*diff(log(macro_lu_li$SANDP)))
macro_lu_li$ersandp = macro_lu_li$rsandp-macro_lu_li$USTB3M/12

###########################################MICROSOFT#############
# Retornos dos ativos Microsoft
macro_lu_li$ermsoft = c(NA,100*diff(log(macro_lu_li$MICROSOFT)))-macro_lu_li$USTB3M/12

#Modelo MMQ#

lm_msoft = lm(ermsoft ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm, data = macro_lu_li)
summary(lm_msoft)




library(car)
linearHypothesis(lm_msoft,c("dprod=0","dcredit=0","dmoney=0","dspread=0", "dinflation=0", "rterm=0"))


lm_start = lm(ermsoft~1,data=macro_lu_li[-(1:2),])
step(lm_start,direction = "forward",scope = formula(lm_msoft))

############################## QUANTILE_MICROSOFT ##############################
library("quantreg")
qreg = rq(ermsoft ~ ersandp, data =macro_lu_li)
summary(qreg)
qreg = rq(ermsoft ~ ersandp, data =macro_lu_li, tau=seq(0.1,0.9,0.1))

plot(summary(qreg), level = 0.95)




################################FORD################################
# Retornos dos ativos FORD
macro_lu_li$erford = c(NA,100*diff(log(macro_lu_li$FORD)))-macro_lu_li$USTB3M/12



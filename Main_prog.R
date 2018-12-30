################################################################################
clear()
#
## Main Program
library(readxl)
library(data.table)
library(dtplyr)    #table managment
library(pracma)   #matlab useful function
library(xts)      #eXtensiont TS
library(ggplot2)  #Plot
library(plotrix)
library(scales)    #Scalling graph
library(stargazer) # latex output for estimation and descriptive statistics"
library(texreg)    # latex output
require(rugarch)   # GARCH estimation
library(moments)   # Compute JB test
library(dplyr)
library(tibble)
library(ggpubr)   #combine plutiple ggplot
library(aTSA)     # ADF test
source("multiplot.R")
source("extract.uGARCHfit.R")
source("CountOccSign.R")
################################################################################

#import data G3487
return_daily <- read_excel(path = "../G_BRVM.xlsx" , sheet = "daily_3",  range = "A1:G3300", col_names = TRUE,
                           col_types = c("date", rep("numeric",6)) )
return_week  <- read_excel(path = "../G_BRVM.xlsx" , sheet = "weekly_2", range = "A1:E711" , col_names = TRUE)

return_daily2 <- return_daily[,c(1,4,5,6,7)]
IsNa = sapply(return_daily2,function(x) is.na(x))
return_daily2[IsNa] <- 0
return_week2 <- return_week[,c(1,4,5)]
IsNa = sapply(return_week2,function(x) is.na(x))
return_week2[IsNa] <- 0

#plot of return/index
ggplot(data = data.frame(return_daily[,1:3]), aes(x = return_daily$Date[])) + 
  geom_line(aes(y = unlist(return_daily[,2]), colour="brvmc") )+ 
  geom_line(aes(y = unlist(return_daily[,3]), colour="brvm10") )+
  labs(x = "Time",
       y = "Daily index values",
       caption="Source: BRVM Web site")+
  theme(axis.text.x = element_text(face="bold", size=8, angle=45)) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%y-%b") +
  scale_colour_manual("", values=c(brvmc="blue",brvm10="red"))+
  theme(legend.position = c(0.1,0.85))

ggsave('/Volumes/Transcend/ENSEA/Articles_Livres/Mean_Reversion/Article/graph01.png')


ggplot(data = data.frame(return_daily2[,c(1,2:3)]), aes(x = return_daily2$Date[])) + 
  geom_line(aes(y = unlist(return_daily2[,2]), colour="brvmc") )+ 
  geom_line(aes(y = unlist(return_daily2[,3]), colour="brvm10") )+
  labs(x = "Time",
       y = "Daily return",
       caption="Source: BRVM Web site")+
  theme(axis.text.x = element_text(face="bold", size=8, angle=45)) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%y-%b") +
  scale_colour_manual("", values=c(brvmc="blue",brvm10="red"))+
  theme(legend.position = c(0.1,0.85))

ggsave('/Volumes/Transcend/ENSEA/Articles_Livres/Mean_Reversion/Article/graph02.png')


ggplot(data = data.frame(return_week2[,c(1,2:3)]), aes(x = return_week2$Date[])) + 
  geom_line(aes(y = unlist(return_week2[,2]), colour="brvmc") )+ 
  geom_line(aes(y = unlist(return_week2[,3]), colour="brvm10") )+
  labs(x = "Time",
       y = "Weekly return",
       caption="Source: BRVM Web site")+
  theme(axis.text.x = element_text(face="bold", size=8, angle=45)) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%y-%b") +
  scale_colour_manual("", values=c(brvmc="blue",brvm10="red"))+
  theme(legend.position = c(0.1,0.85))

ggsave('/Volumes/Transcend/ENSEA/Articles_Livres/Mean_Reversion/Article/graph03.png')


ggplot(data = data.frame(return_daily2[,c(1,2,4)]), aes(x = return_daily2$Date[])) + 
  geom_line(aes(y = unlist(return_daily2[,2]), colour="brvmc_daily") )+ 
  geom_line(aes(y = unlist(return_daily2[,4]), colour="brvmc_weekly") )+
  labs(x = "Time",
       y = "Daily vs Weekly return",
       caption="Source: BRVM Web site")+
  theme(axis.text.x = element_text(face="bold", size=8, angle=45)) +
  scale_x_datetime(date_breaks = "1 year", date_labels = "%y-%b") +
  scale_colour_manual("", values=c(brvmc_daily="blue",brvmc_weekly="red"))+
  theme(legend.position = c(0.1,0.85))

ggsave('/Volumes/Transcend/ENSEA/Articles_Livres/Mean_Reversion/Article/graph04.png')
#multiplot(gr1, gr2, gr3, gr4, cols=2)

#Descriptive statistics
stargazer(data.frame(return_daily2[,c(2,3)]), digits=4)
stargazer(data.frame(return_week2[,c(2,3)]), digits=4)

tab1 <- zeros(15,8)

tab1[,1] <- CountOccSign(return_daily2[,2], 16, 1)
tab1[,2] <- CountOccSign(return_daily2[,2], 16, 0)
tab1[,3] <- CountOccSign(return_daily2[,3], 16, 1)
tab1[,4] <- CountOccSign(return_daily2[,3], 16, 0)
tab1[,5] <- CountOccSign(return_week2[,2], 16, 1)
tab1[,6] <- CountOccSign(return_week2[,2], 16, 0)
tab1[,7] <- CountOccSign(return_week2[,3], 16, 1)
tab1[,8] <- CountOccSign(return_week2[,3], 16, 0)

rownames(tab1) <- c("2c", "3c", "4c", "5c", "6c", "7c", "8c", "9c", "10c", "11c", "12c", "13c", "14c", "15c", "16c")
colnames(tab1) <- c("+", "-", "+", "-", "+", "-", "+", "-")
stargazer(tab1)


#ACF plot
ts.acf <- acf(ts(return_daily2[,2]), plot=FALSE)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.acf$n.used)

gr1 = ts.acf$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x=lags, y = V1)) + scale_x_continuous(breaks=seq(0,41,4)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue') +
  labs(y="Autocorrelations", x="Lag", title= "BRVMC daily, ACF") +
  geom_segment(aes(xend=lags, yend=0)) +geom_point()


ts.acf <- acf(ts(return_daily2[,3]), plot=FALSE)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.acf$n.used)

gr2 = ts.acf$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x=lags, y = V1)) + scale_x_continuous(breaks=seq(0,41,4)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue') +
  labs(y="Autocorrelations", x="Lag", title= "BRVM10 daily, ACF") +
  geom_segment(aes(xend=lags, yend=0)) +geom_point()

ts.acf <- acf(ts(return_week2[,2]), plot=FALSE)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.acf$n.used)

gr3 = ts.acf$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x=lags, y = V1)) + scale_x_continuous(breaks=seq(0,41,4)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue') +
  labs(y="Autocorrelations", x="Lag", title= "BRVMC weekly, ACF") +
  geom_segment(aes(xend=lags, yend=0)) +geom_point()


ts.acf <- acf(ts(return_week2[,3]), plot=FALSE)
alpha <- 0.95
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(ts.acf$n.used)

gr4 = ts.acf$acf %>% 
  as_tibble() %>% mutate(lags = 1:n()) %>% 
  ggplot(aes(x=lags, y = V1)) + scale_x_continuous(breaks=seq(0,41,4)) +
  geom_hline(yintercept=conf.lims, lty=2, col='blue') +
  labs(y="Autocorrelations", x="Lag", title= "BRVM10 weekly, ACF") +
  geom_segment(aes(xend=lags, yend=0)) +geom_point()

figure <- ggarrange(gr1,gr2,gr3,gr4, ncol = 2, nrow = 2)
figure
ggsave('/Volumes/Transcend/ENSEA/Articles_Livres/Mean_Reversion/Article/graph05.png')




#ANAR  vs ANARMA
tab2 <- zeros(5,8)
comp <- 0
var <- 0
for(df in list(return_daily2,return_week2)){
  var <- var + 1
  for (J in c(2,3)){
    rm(list=c("lag.return","dep","dep2"))
    dep <- df[,J]
    if (var==1 && J==2){
      dep2 <- add_row(dep, brvmc_return_d = 0, .before=1)
    }else if (var==1 && J==3){
      dep2 <- add_row(dep, brvm10_return_d = 0, .before=1)
    }else if (var==2 && J==2){
      dep2 <- add_row(dep, brvmc_return_w = 0, .before=1)
    }else if (var==2 && J==3){
      dep2 <- add_row(dep, brvm10_return_w = 0, .before=1)
    }
    lag.return <- (stats::lag(ts(dep2))<0)*stats::lag(ts(dep2)) 
    for (k in c(0,1)){
      comp <- comp + 1
      spec <- ugarchspec(variance.model = list(model = "eGARCH", 
                                           garchOrder = c(1, 1), 
                                           submodel = NULL, 
                                           external.regressors = NULL  , 
                                           variance.targeting = FALSE), 
                        mean.model     = list(armaOrder = c(1, k), 
                                           external.regressors = lag.return),
                        distribution.model = "norm", 
                        start.pars = list(), 
                        fixed.pars = list())
      garch <- ugarchfit(spec = spec, data = data.frame(dep), solver.control = list(trace=0))
      tab2[1,comp] <- round(garch@fit$LLH, digits=2)
      tab2[c(2,3,5,4),comp] <- round(infocriteria(garch), digits=2)
    }
  }
}

stargazer(tab2)


#Estimation ANARMA(1,1) et ANAR(1)
rm(list=c("dep", "depsign", "df", "garch", "garch1", "spec", "i", "J", "k", "l", "len", "var", "lag.return"))
var <- 0
for(df in list(return_daily2, return_week2)){
  var <- var + 1
  for (J in c(2,3)){
    dep <- df[,J]
    depsign <- dep<0
    len <- nrow(df[,J])
    for(i in 1:3){
      lag.return <- data.frame(rep(0, len))
      for(l in (i+1):len){
        ifelse(prod(depsign[(l-i):(l-1)])==1, lag.return[l,1] <- dep[l-1,1],0)
      }
      lag.return <- ts(lag.return)
      k <- 1
      if (var==2 && J==3){
        k <- 0
      }
      spec <- ugarchspec(variance.model = list(model = "eGARCH", 
                                               garchOrder = c(1, 1), 
                                               submodel = NULL, 
                                               external.regressors = NULL , 
                                               variance.targeting = FALSE), 
                         mean.model     = list(armaOrder = c(1, k), 
                                               external.regressors = lag.return),
                         distribution.model = "norm", 
                         start.pars = list(), 
                         fixed.pars = list())
      assign(paste("garch", var, J, i, sep = "_"), ugarchfit(spec = spec, data = data.frame(dep), solver.control = list(trace=0)))
    }
  }
}

setMethod("extract", signature = className("uGARCHfit", "rugarch"), definition = extract.uGARCHfit)

# 4 models
texreg(list(garch_1_2_1, garch_1_3_1, garch_2_2_1, garch_2_3_1),
       custom.model.names = c("brvmc daily", "brvm10 daily", "brvmc weekly", "brvm10 weekly"),
       custom.coef.names = c("$++mu$", "$++phi^+$","$++theta$", "$++rho$","$++omega$","$++alpha$", "$++beta$", "$++gamma$"))

#all models
texreg(lapply(ls(pattern="\\garch"), get),
       custom.coef.names = c("$++mu$", "$++phi^+$","$++theta$", "$++rho$","$++omega$","$++alpha$", "$++beta$", "$++gamma$"))

tab3 = zeros(36,7)
l = adf.test(return_daily2$brvmc_return_d, nlag =9)
tab3[1:9,1] = l$type1[,1]
tab3[1:9,2:3] = l$type1[,2:3]
tab3[1:9,4:5] = l$type2[,2:3]
tab3[1:9,6:7] = l$type3[,2:3]

l = adf.test(return_daily2$brvm10_return_d, nlag =9)
tab3[10:18,1] = l$type1[,1]
tab3[10:18,2:3] = l$type1[,2:3]
tab3[10:18,4:5] = l$type2[,2:3]
tab3[10:18,6:7] = l$type3[,2:3]

l = adf.test(return_week2$brvmc_return_w, nlag =9)
tab3[19:27,1] = l$type1[,1]
tab3[19:27,2:3] = l$type1[,2:3]
tab3[19:27,4:5] = l$type2[,2:3]
tab3[19:27,6:7] = l$type3[,2:3]

l = adf.test(return_week2$brvm10_return_w, nlag =9)
tab3[28:36,1] = l$type1[,1]
tab3[28:36,2:3] = l$type1[,2:3]
tab3[28:36,4:5] = l$type2[,2:3]
tab3[28:36,6:7] = l$type3[,2:3]

stargazer(tab3)

































rm(list=c("lag.return","dep","dep2"))
J <- 4
dep <- return_daily[,J]
dep2 <- add_row(dep, brvmc_return_d = 0, .before=1)
lag.return <- (stats::lag(ts(dep2))<0)*stats::lag(ts(dep2))
spec <- rugarch::ugarchspec(variance.model = list(model = "eGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL  , 
                                         variance.targeting = FALSE), 
                    mean.model     = list(armaOrder = c(1, 1), 
                                          external.regressors = lag.return, 
                                          distribution.model="norm", 
                                          start.pars = list(), 
                                          fixed.pars = list()))

garch <- ugarchfit(spec = spec, data = data.frame(dep), solver.control = list(trace=0))
garch













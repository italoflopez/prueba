library(ROracle)
library(keyring)
library(forecast)
library(zoo)
library(tseries)
library(vars)
library(portes)
library(plotly)
library(quantmod)
library(Metrics)
library(fGarch)
library(rugarch)
library(zoo)
library(tis)
library(forecast)
library(astsa)
library(plotly)
library(quantmod)
library(sarima)
library(pracma)
library(opera)
library(ForecastComb)
library(Rfast2)
library(systemfit)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)
library(glmnet)
library(xlsx)
library(mFilter)

rgdp <- getSymbols("ND000334Q",src='FRED')
rgdp <- ND000334Q
rgdp <- as.zoo(rgdp)
plotly_simple(rgdp,"Real GDP")
monthly_rgdp <- rgdp/3
colnames(monthly_rgdp) <- "Real GDP"

a <- rep("NA",length.out=length(monthly_rgdp)*3)
for (i in seq(from=3,by=3,length.out=length(monthly_rgdp))){
  a[i]<-as.numeric(monthly_rgdp[i/3])
}
monthly_values <- spline(x=1:(length(monthly_rgdp)*3),y=a)$y
monthly_values <- ts(monthly_values,start = c(2002,1),frequency=12)
monthly_values <- as.zoo(monthly_values)

monthly_rgdp <- monthly_values
rm(a)

decompose_monthly_rgdp <- decompose(as.ts(monthly_rgdp),"additive")
sa_monthly_rgdp <- as.ts(monthly_rgdp)-decompose_monthly_rgdp$seasonal
sa_monthly_rgdp <- as.zoo(sa_monthly_rgdp)
plotly_simple(sa_monthly_rgdp,"Seasonally Adjusted Monthly RGDP")

hp_decomp <- hpfilter(sa_monthly_rgdp, freq = 14400, type = "lambda")

cycle <- log(sa_monthly_rgdp/hp_decomp$trend)*100
plotly_simple(cycle,"Percentage deviation from Potential GDP")

cpi <- getSymbols("CPIAUCSL",src='FRED')
cpi <- CPIAUCSL
cpi <- as.zoo(cpi)
plotly_simple(cpi,"CPI")
colnames(cpi) <- "CPI"

inflation <- diff(log(cpi),12)*100
plotly_simple(inflation,"Interannual Inflation")

inflation_expectation <- getSymbols("MICH",src='FRED')
inflation_expectation <- MICH
inflation_expectation <- as.zoo(inflation_expectation)
colnames(inflation_expectation) <- "Inflation Expectation"

dev_inflation <- inflation-lag_vec(inflation_expectation,12)
index(dev_inflation) <- as.yearmon(index(dev_inflation))
plotly_simple(dev_inflation,"Deviation of Inflation from Expectation")
index(dev_inflation) <- as.yearmon(index(dev_inflation))

data <- merge(cycle,dev_inflation)
data <- data[complete.cases(data),]
colnames(data) <- c("cycle","dev_inflation")

cycle <- data[,"cycle"]
dev_inflation <- data[,"dev_inflation"]

fig <- plot_ly(data = data%>%as.data.frame(), x = ~dev_inflation, y = ~cycle)
fig

regimes_plotly <- plot_ly(data = data%>%as.data.frame(), x = ~dev_inflation, y = ~cycle)
print(regimes_plotly)

unrate <- getSymbols("UNRATE",src='FRED')
unrate <- UNRATE
unrate <- as.zoo(unrate)
index(unrate) <- as.yearmon(index(unrate))
plotly_simple(unrate,"Unemployment Rate")
colnames(unrate) <- "unrate"

data_unrate <- merge(data,unrate)
data_unrate <- data_unrate[complete.cases(data_unrate),]
date_data_unrate <- index(data_unrate)
data_unrate <-data_unrate%>%as.data.frame()


regimes_with_unrate_plotly <- plot_ly(data = data_unrate, 
                                      x = ~dev_inflation,
                                      y = ~cycle,
                                      color=~unrate,
                                      type = "scatter",
                                      text=as.character(as.Date(date_data_unrate))
                                      )
print(regimes_with_unrate_plotly)
  #Good

us_dollar_index <- getSymbols("DTWEXBGS",src='FRED')
us_dollar_index <- DTWEXBGS
us_dollar_index <- as.zoo(us_dollar_index)
us_dollar_index <- aggregate(us_dollar_index,as.yearmon,mean,
                        na.rm=T)
plotly_simple(us_dollar_index,"US Dollar Index")
colnames(us_dollar_index) <- "us_dollar_index"

dollar_apreciation <- diff(log(us_dollar_index),12)*100
colnames(dollar_apreciation) <- "dollar_apreciation"

data_dollar_apreciation <- merge(data,dollar_apreciation)
data_dollar_apreciation <- data_dollar_apreciation[complete.cases(data_dollar_apreciation),]
date_data_dollar_apreciation <- index(data_dollar_apreciation)
data_dollar_apreciation <-data_dollar_apreciation%>%as.data.frame()


regimes_with_dollar_plotly <- plot_ly(data = data_dollar_apreciation, 
                                      x = ~dev_inflation,
                                      y = ~cycle,
                                      color=~dollar_apreciation,
                                      type = "scatter",
                                      text=as.character(as.Date(date_data_dollar_apreciation)))
print(regimes_with_dollar_plotly)

yield_10 <- getSymbols("DGS10",src='FRED')
yield_10 <- DGS10
yield_10 <- as.zoo(yield_10)
yield_10 <- aggregate(yield_10,as.yearmon,mean,
                             na.rm=T)
plotly_simple(yield_10,"10-year Treasury Yield")
colnames(yield_10) <- "yield_10"

data_yield_10 <- merge(data,yield_10)
data_yield_10 <- data_yield_10[complete.cases(data_yield_10),]
date_data_yield_10 <- index(data_yield_10)
data_yield_10 <-data_yield_10%>%as.data.frame()

regimes_with_yield_10_plotly <- plot_ly(data = data_yield_10, 
                                      x = ~dev_inflation,
                                      y = ~cycle,
                                      color=~yield_10,
                                      type = "scatter",
                                      text=as.character(as.Date(date_data_yield_10)))
print(regimes_with_yield_10_plotly)

s_p_500 <- getSymbols("SP500",src='FRED')
s_p_500 <- SP500
s_p_500 <- as.zoo(s_p_500)
s_p_500 <- aggregate(s_p_500,as.yearmon,mean,
                      na.rm=T)
plotly_simple(s_p_500,"S&P 500 Index")
colnames(s_p_500) <- "s_p_500"

return_s_p_500 <- diff(log(s_p_500),12)*100
plotly_simple(return_s_p_500,"Annual Return S&P 500 Index")
colnames(return_s_p_500) <- "return_s_p_500"

data_return_s_p_500 <- merge(data,return_s_p_500)
data_return_s_p_500 <- data_return_s_p_500[complete.cases(data_return_s_p_500),]
date_data_return_s_p_500 <- index(data_return_s_p_500)
data_return_s_p_500 <-data_return_s_p_500%>%as.data.frame()

regimes_with_return_s_p_500_plotly <- plot_ly(data = data_return_s_p_500, 
                                        x = ~dev_inflation,
                                        y = ~cycle,
                                        color=~return_s_p_500,
                                        type = "scatter",
                                        text=as.character(as.Date(date_data_return_s_p_500)))
print(regimes_with_return_s_p_500_plotly)

mean(data_unrate[data_unrate$`Inflation Deviation`<0&data_unrate$Cycle<0,"unrate"])
data_dollar_apreciation[data_dollar_apreciation$dev_inflation<0&data_dollar_apreciation$cycle<0,
                        "dollar_apreciation"]%>%mean()

data_full <- merge(data,unrate,dollar_apreciation,yield_10,return_s_p_500)

colnames(data_full)

# table <-as.data.frame(data_full) %>% filter(-1*cycle > 0)%>%filter(-1*dev_inflation > 0)
# 
# table[,3]

# states <- c("Low","High")
regimes_tables <- vector("list",4)
table_bla <- 1
  
for (i in c(-1,1)){
for (j in c(-1,1)){
    table <-as.data.frame(data_full) %>% filter(i*cycle > 0)%>%filter(j*dev_inflation > 0)
    table <- table%>%select(c(-1,-2))
    table_bla <- table_bla+ncol(table[,-1:-2])-1
    regimes_tables[[table_bla-1]] <- colMeans(table,na.rm = T)
  }
}

names(regimes_tables) <- c("Low-Low","Low-High","High-Low","High-High")

quantiles_table <- vector("list",9)

quantiles <- c(0,0.25,0.75,1)

quantiles_table_bla <- 1


for (i in 1:3){
  for (j in 1:3){
    table_quantiles <-as.data.frame(data_full) %>% filter(quantile(cycle,quantiles[i+1],na.rm=T)>cycle& cycle> quantile(cycle,quantiles[i],na.rm=T))%>%
      filter(quantile(dev_inflation,quantiles[j+1],na.rm=T)>dev_inflation& dev_inflation > quantile(dev_inflation,quantiles[j],na.rm=T))
    table_quantiles <- table_quantiles%>%select(c(-1,-2))
    quantiles_table_bla <- quantiles_table_bla+ncol(table_quantiles[,-1:-2])-1
    quantiles_table[[quantiles_table_bla-1]] <- colMeans(table_quantiles,na.rm = T)
  }
}

names(quantiles_table) <- c("Low-Low","Low-Medium","Low-High",
                            "Medium-Low","Medium-Medium","Medium-High","High-Low","High-Medium","High-High")

us_pmi <- dbSendQuery(db_con, "select fecha, px_last
from bloomberg_data
where codigo='NAPMPMI Index'
order by fecha")


us_pmi <- fetch(us_pmi)
us_pmi

us_pmi <- ts(data = us_pmi[,2],start = c(2008,1),frequency = 12,end = c(2022,5))
us_pmi <- as.zoo(us_pmi)
index(us_pmi) <- as.yearmon(index(us_pmi))
plotly_simple(us_pmi,"US PMI")

decompose_us_pmi <- decompose(as.ts(us_pmi),"additive")
sa_us_pmi <- as.ts(us_pmi)-decompose_us_pmi$seasonal
sa_us_pmi <- as.zoo(sa_us_pmi)
plotly_simple(sa_us_pmi,"Seasonally Adjusted US PMI")

hp_decomp_us_pmi <- hpfilter(sa_us_pmi, freq = 14400, type = "lambda")

cycle_us_pmi <- log(sa_us_pmi/hp_decomp_us_pmi$trend)*100
plotly_simple(cycle_us_pmi,"Percentage deviation from Trend US PMI")
plotly_simple(hp_decomp_us_pmi$trend,"US PMI Trend")

data_us_pmi <- merge(cycle_us_pmi,dev_inflation)
data_us_pmi <- data_us_pmi[complete.cases(data_us_pmi),]
colnames(data_us_pmi) <- c("cycle_us_pmi","dev_inflation")

cycle_us_pmi <- data_us_pmi[,"cycle_us_pmi"]
dev_inflation <- data_us_pmi[,"dev_inflation"]

date_data_us_pmi <- index(data_us_pmi)

regimes_us_pmi_plotly <- plot_ly(data = data_us_pmi%>%as.data.frame(), x = ~dev_inflation, y = ~cycle_us_pmi,
                                 text=as.character(as.Date(date_data_us_pmi)))
print(regimes_us_pmi_plotly)

data_us_pmi_unrate <- merge(data_us_pmi,unrate)
data_us_pmi_unrate <- data_us_pmi_unrate[complete.cases(data_us_pmi_unrate),]
# data_us_pmi_unrate <-data_us_pmi_unrate%>%as.data.frame()
date_data_us_pmi_unrate <- index(data_us_pmi_unrate) 
colnames(data_us_pmi_unrate) <- c("cycle_us_pmi","dev_inflation","unrate")

regimes_us_pmi_with_unrate_plotly <- plot_ly(data = data_us_pmi_unrate%>%as.data.frame(), 
                                      x = ~dev_inflation,
                                      y = ~cycle_us_pmi,
                                      color=~unrate,
                                      type = "scatter",
                                      text=as.character(as.Date(date_data_us_pmi_unrate)))

print(regimes_us_pmi_with_unrate_plotly)

data_us_pmi_dollar_apreciation <- merge(data_us_pmi,dollar_apreciation)
data_us_pmi_dollar_apreciation <- data_us_pmi_dollar_apreciation[complete.cases(data_us_pmi_dollar_apreciation),]
date_data_us_pmi_dollar_apreciation <- index(data_us_pmi_dollar_apreciation) 
colnames(data_us_pmi_dollar_apreciation) <- c("cycle_us_pmi","dev_inflation","dollar_apreciation")

regimes_us_pmi_with_dollar_apreciation_plotly <- plot_ly(data = data_us_pmi_dollar_apreciation%>%as.data.frame(), 
                                             x = ~dev_inflation,
                                             y = ~cycle_us_pmi,
                                             color=~dollar_apreciation,
                                             type = "scatter",
                                             text=as.character(as.Date(date_data_us_pmi_dollar_apreciation)))

print(regimes_us_pmi_with_dollar_apreciation_plotly)

data_us_pmi_yield_10 <- merge(data_us_pmi,yield_10)
data_us_pmi_yield_10 <- data_us_pmi_yield_10[complete.cases(data_us_pmi_yield_10),]
date_data_us_pmi_yield_10 <- index(data_us_pmi_yield_10) 
colnames(data_us_pmi_yield_10) <- c("cycle_us_pmi","dev_inflation","yield_10")

regimes_us_pmi_with_yield_10_plotly <- plot_ly(data = data_us_pmi_yield_10%>%as.data.frame(), 
                                                         x = ~dev_inflation,
                                                         y = ~cycle_us_pmi,
                                                         color=~yield_10,
                                                         type = "scatter",
                                                         text=as.character(as.Date(date_data_us_pmi_yield_10)))

print(regimes_us_pmi_with_yield_10_plotly)

data_us_pmi_return_s_p_500 <- merge(data_us_pmi,return_s_p_500)
data_us_pmi_return_s_p_500 <- data_us_pmi_return_s_p_500[complete.cases(data_us_pmi_return_s_p_500),]
date_data_us_pmi_return_s_p_500 <- index(data_us_pmi_return_s_p_500) 
colnames(data_us_pmi_return_s_p_500) <- c("cycle_us_pmi","dev_inflation","return_s_p_500")

regimes_us_pmi_with_return_s_p_500_plotly <- plot_ly(data = data_us_pmi_return_s_p_500%>%as.data.frame(), 
                                               x = ~dev_inflation,
                                               y = ~cycle_us_pmi,
                                               color=~return_s_p_500,
                                               type = "scatter",
                                               text=as.character(as.Date(date_data_us_pmi_return_s_p_500)))

print(regimes_us_pmi_with_return_s_p_500_plotly)

library(markovchain)
#First, we have to create the sequence of states
high_cycle <- cycle>0
high_inflation <- dev_inflation>0
states <- paste(high_cycle,high_inflation)

estimates <- markovchainFit(data = states, method = "mle")
transition_matrix <- estimates$estimate
names(transition_matrix) <- c("Low-Low","Low-High","High-Low","High-High")
transition_matrix

estimates_6 <- fitHigherOrder(states, 6)
# transition_matrix <- 
# names(transition_matrix) <- c("Low-Low","Low-High","High-Low","High-High")
# transition_matrix

high_cycle_us_pmi <- cycle_us_pmi>0
states_us_pmi <- paste(high_cycle_us_pmi,high_inflation)

estimates_us_pmi <- markovchainFit(data = states_us_pmi, method = "mle")
transition_matrix_us_pmi <- estimates_us_pmi$estimate
names(transition_matrix_us_pmi) <- c("Low-Low","Low-High","High-Low","High-High")
transition_matrix_us_pmi

us_pmi_50 <- us_pmi-50

data_us_pmi_50 <- merge(us_pmi_50,dev_inflation)
data_us_pmi_50 <- data_us_pmi_50[complete.cases(data_us_pmi_50),]
colnames(data_us_pmi_50) <- c("us_pmi_50","dev_inflation")

us_pmi_50 <- data_us_pmi_50[,"us_pmi_50"]
dev_inflation <- data_us_pmi_50[,"dev_inflation"]

date_data_us_pmi_50 <- index(data_us_pmi_50)

regimes_us_pmi_50_plotly <- plot_ly(data = data_us_pmi_50%>%as.data.frame(), x = ~dev_inflation, y = ~us_pmi_50,
                                 text=as.character(as.Date(date_data_us_pmi_50)))
print(regimes_us_pmi_50_plotly)

data_us_pmi_50_unrate <- merge(data_us_pmi_50,unrate)
data_us_pmi_50_unrate <- data_us_pmi_50_unrate[complete.cases(data_us_pmi_50_unrate),]
# data_us_pmi_unrate <-data_us_pmi_unrate%>%as.data.frame()
date_data_us_pmi_50_unrate <- index(data_us_pmi_50_unrate) 
colnames(data_us_pmi_50_unrate) <- c("us_pmi_50","dev_inflation","unrate")


regimes_us_pmi_50_with_unrate_plotly <- plot_ly(data = data_us_pmi_50_unrate%>%as.data.frame(), 
                                             x = ~dev_inflation,
                                             y = ~us_pmi_50,
                                             color=~unrate,
                                             type = "scatter",
                                             text=as.character(as.Date(date_data_us_pmi_50_unrate)))

print(regimes_us_pmi_50_with_unrate_plotly)

data_us_pmi_50_dollar_apreciation <- merge(data_us_pmi_50,dollar_apreciation)
data_us_pmi_50_dollar_apreciation <- data_us_pmi_50_dollar_apreciation[complete.cases(data_us_pmi_50_dollar_apreciation),]
date_data_us_pmi_50_dollar_apreciation <- index(data_us_pmi_50_dollar_apreciation) 
colnames(data_us_pmi_50_dollar_apreciation) <- c("us_pmi_50","dev_inflation","dollar_apreciation")

regimes_us_pmi_50_with_dollar_apreciation_plotly <- plot_ly(data = data_us_pmi_50_dollar_apreciation%>%as.data.frame(), 
                                                         x = ~dev_inflation,
                                                         y = ~us_pmi_50,
                                                         color=~dollar_apreciation,
                                                         type = "scatter",
                                                         text=as.character(as.Date(date_data_us_pmi_50_dollar_apreciation)))

print(regimes_us_pmi_50_with_dollar_apreciation_plotly)

data_us_pmi_50_yield_10 <- merge(data_us_pmi_50,yield_10)
data_us_pmi_50_yield_10 <- data_us_pmi_50_yield_10[complete.cases(data_us_pmi_50_yield_10),]
date_data_us_pmi_50_yield_10 <- index(data_us_pmi_50_yield_10) 
colnames(data_us_pmi_yield_10) <- c("us_pmi_50","dev_inflation","yield_10")

regimes_us_pmi_50_with_yield_10_plotly <- plot_ly(data = data_us_pmi_50_yield_10%>%as.data.frame(), 
                                               x = ~dev_inflation,
                                               y = ~us_pmi_50,
                                               color=~yield_10,
                                               type = "scatter",
                                               text=as.character(as.Date(date_data_us_pmi_50_yield_10)))

print(regimes_us_pmi_50_with_yield_10_plotly)

data_us_pmi_50_return_s_p_500 <- merge(data_us_pmi_50,return_s_p_500)
data_us_pmi_50_return_s_p_500 <- data_us_pmi_50_return_s_p_500[complete.cases(data_us_pmi_50_return_s_p_500),]
date_data_us_pmi_50_return_s_p_500 <- index(data_us_pmi_50_return_s_p_500) 
colnames(data_us_pmi_50_return_s_p_500) <- c("us_pmi_50","dev_inflation","return_s_p_500")

regimes_us_pmi_50_with_return_s_p_500_plotly <- plot_ly(data = data_us_pmi_50_return_s_p_500%>%as.data.frame(), 
                                                     x = ~dev_inflation,
                                                     y = ~us_pmi_50,
                                                     color=~return_s_p_500,
                                                     type = "scatter",
                                                     text=as.character(as.Date(date_data_us_pmi_50_return_s_p_500)))

print(regimes_us_pmi_50_with_return_s_p_500_plotly)

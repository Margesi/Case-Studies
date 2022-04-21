# Project Tasks 

library(dplyr)
library(formattable)
library(readr)
library(stargazer)
library(fpp2)
library(xtable)
library(stats)
library(Metrics)

X2022_02 <- read_csv(file ="2022-02.csv")

X2022_02 <- X2022_02[3:254,c(1,2,61,121,160,244,35,145)]
colnames(X2022_02)
str(X2022_02)
#Growth Rates
project1data_diff <- X2022_02[,c(2,4:6)] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x))

colnames(project1data_diff) <- paste0("%",colnames(project1data[,c(2,4:6)]))

project1data <- cbind(X2022_02,project1data_diff)
project1data_final <- project1data[,c(1,3,7:12)]
head(project1data_final,10)

#Trends
plot_file<-ts(project1data_final,start=1959, frequency = 4)

autoplot(plot_file)+
  ggtitle("Number of new single-family houses sold") +
  xlab("Year") + ylab("Value")

autoplot(plot_file[,c("GDPC1","M1REAL",'S&P 500')])+
  ggtitle("GDP,Real M1 and S&P 500 value ", subtitle = "S&P’s Common Stock Price Index: Composite,
          Real Gross Domestic Product, 3 Decimal (Billions of Chained 2012 Dollars),
          Real M1 Money Stock (Billions of 1982-84 Dollars), deflated by CPI") +
  xlab("Year") + ylab("Value")

autoplot(plot_file[,"UNRATESTx"])+
  ggtitle("Unemployment rate throught the years") +
  xlab("Year") + ylab("Unemployment rate")

autoplot(plot_file[,"CPIAUCSL"])+
  ggtitle("Consumer Price Index for All Urban Consumers: All Items",subtitle = "Index is 1982-84=100)") +
  xlab("Year") + ylab("CPI")

autoplot(plot_file[,"CUMFNS"])+
  ggtitle("Capital Utilization", subtitle = "Capacity Utilization: Manufacturing (SIC) (Percent of Capacity)") +
  xlab("Year") + ylab("Percentage utilized")

autoplot(plot_file[,"FEDFUNDS"])+
  ggtitle("Effective Federal Funds Rate") +
  xlab("Year") + ylab("Funds Rate")

autoplot(plot_file[,c(9:12)])+
  ggtitle("Growth of GDP,Real M1, CPI and S&P 500")+
  xlab("Year") + ylab("Value Percentage")

autoplot(plot_file[,c(9,10,12)])+
  ggtitle("Growth of GDP, CPI and S&P 500 ", subtitle = "S&P’s Common Stock Price Index: Composite,
          Real Gross Domestic Product, 3 Decimal (Billions of Chained 2012 Dollars),
          Real M1 Money Stock (Billions of 1982-84 Dollars), deflated by CPI") +
  xlab("Year") + ylab("value Percentage")


#ACF (Check the paper that database coming from and others)

acf(GDP[-1], main = "ACF of the GDP growth")


#Task 2
library(xts)
project1data_gdp <- project1data_final[3:252,5]

project1data_model<-  ts(project1data_gdp,start=c(1959,3), freq = 4)

is.ts(project1data_model)
print(project1data_model)              

plot.ts(project1data_model, main="Growth in GDP", ylab="%GDPC1")

gdpc1_ar <- arima(project1data_model , order = c(1, 0, 0))
gdpc1_ar
residuals <- residuals(gdpc1_ar)
gdpc1_fitted <- project1data_model-residuals

ts.plot(project1data_model)
points(gdpc1_fitted, type = "l", col = 2, lty = 2)
#plot(decompose(project1data_model))

#AR Model

GDP <- xts(project1data_final$`%GDPC1`[-1], project1data_final$sasdate[-1], order.by=as.Date(project1data_final$sasdate[-1], format = "%m/%d/%Y"))

armod1 <- arima(GDP, c(1,0,0))
fitted(armod1)
ts.plot(project1data_final$`%GDPC1`[-c(1:2)])
points(fitted(armod1), type = "l", col = 2, lty = 2)

rmse(project1data_final$`%GDPC1`[-1],fitted(armod1))
summary(armod1)$sigma

# VAR(1) model

library(vars)
projectdata_var <- project1data_final[,c(1,3,4,6,9:12)]

GDPVAR <- xts( projectdata_var[-1,-1],projectdata_var$sasdate[-1],order.by=as.Date(projectdata_var$sasdate[-1], format = "%m/%d/%Y"))
varmod <- VAR(GDPVAR, p = 1, type = "const", season = NULL, exog = NULL) 
summary(varmod)
fitted(varmod)[,4]

ts.plot(GDPVAR$`%GDPC1`[-1])
points(fitted(varmod)[,4], type = "l", col = 2, lty = 2)
rmse(project1data_final$`%GDPC1`[-c(1:2)],fitted(varmod)[,4]) #??


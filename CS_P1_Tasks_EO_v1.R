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

#Task 1
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

acf(na.omit(project1data_final$GDPC1), main = "ACF of the GDP growth")


#Task 2
library(xts)
project1data_gdp <- project1data_final[2:252,5]

project1data_model<-  ts(project1data_gdp,start=c(1959,2), freq = 4)

print(project1data_model)              

plot.ts(project1data_model, main="Growth in GDP", ylab="%GDPC1")


#AR Model

arfc <- c()
for (i in 2:250) {
armod1 <- arima(project1data_model[1:i], c(1,0,0))
forecasts <- forecast(armod1, 1)
arfc <- append(arfc,forecasts$mean)
}

ts.plot(project1data_model[3:251])
points(arfc, type = "l", col = 2, lty = 2)

rmse(project1data_model[2:250],arfc)

# task c
# VAR(1) model

library(vars)

GDPVAR <- xts( project1data_final[-1,-1],project1data_final$sasdate[-1],order.by=as.Date(project1data_final$sasdate[-1], format = "%m/%d/%Y"))

varfc <- c()
for (i in 9:250) {
  varmod <- VAR(GDPVAR[1:i], p = 1, type = "const", season = NULL, exog = NULL) 
  forecasts <- predict(varmod, n.ahead=1)
  varfc <- append(varfc,forecasts$fcst$GDPC1[1])
}

ts.plot(project1data_model[9:251])
points(varfc, type = "l", col = 2, lty = 2)

rmse(project1data_model[9:250],varfc)

# task d
library(lmtest)
varmod_gr <- VAR(GDPVAR[1:251], p = 1, type = "const", season = NULL, exog = NULL)
grangertest(GDPVAR$GDPC1~GDPVAR$CUMFNS)
causality(varmod_gr, cause = "CUMFNS")

grangertest(GDPVAR$GDPC1~GDPVAR$UNRATESTx) #yes
causality(varmod_gr, cause = "UNRATESTx")


grangertest(GDPVAR$GDPC1~GDPVAR$FEDFUNDS)
causality(varmod_gr, cause = "FEDFUNDS")


grangertest(GDPVAR$GDPC1~GDPVAR$CPIAUCSL) #yes
causality(varmod_gr, cause = "CPIAUCSL")


grangertest(GDPVAR$GDPC1~GDPVAR$M1REAL) #yes
causality(varmod_gr, cause = "M1REAL")


grangertest(GDPVAR$GDPC1~GDPVAR$`S&P 500`) #yes
causality(varmod_gr, cause = "S.P.500")

#task e

VARselect(GDPVAR, type= "const", lag.max = 10)
varfc3 <- c()
for (i in 25:250) {
  varmod3 <- VAR(GDPVAR[1:i], p = 3, type = "const", season = NULL, exog = NULL) 
  forecasts3 <- predict(varmod3, n.ahead=1)
  varfc3 <- append(varfc3,forecasts3$fcst$GDPC1[1])
}

rmse(project1data_model[25:250],varfc3)

ts.plot(project1data_model[1:251])
points(arfc, type = "l", col = 2, lty = 2)
points(varfc, type = "l", col = 3, lty = 2)
points(varfc, type = "l", col = 4, lty = 2)




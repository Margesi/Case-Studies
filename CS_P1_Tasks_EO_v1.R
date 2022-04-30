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

data_forecast_eo <- data.frame(project1data_final$sasdate[3:251],project1data_final$GDPC1[3:251], arfc)

rmse(project1data_model[3:251],arfc)

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

data_var_forecast_eo <- data.frame(project1data_final$sasdate[10:251],project1data_final$GDPC1[10:251],varfc)

rmse(project1data_model[10:251],varfc)
sqrt(mean((project1data_model[10:251] - varfc)^2))
# task d
library(lmtest)
varmod_gr <- VAR(GDPVAR[1:251], p = 1, type = "const", season = NULL, exog = NULL)

coefs <- rbind(varmod_gr$varresult$UNRATESTx$coefficients,
varmod_gr$varresult$CUMFNS$coefficients,
varmod_gr$varresult$FEDFUNDS$coefficients,
varmod_gr$varresult$GDPC1$coefficients,
varmod_gr$varresult$CPIAUCSL$coefficients,
varmod_gr$varresult$M1REAL$coefficients,
varmod_gr$varresult$S.P.500$coefficients)

grangertest(GDPVAR$GDPC1~GDPVAR$CUMFNS)
causality(varmod_gr, cause = "CUMFNS")
coefs[c(4,2),c(4,2)]

grangertest(GDPVAR$GDPC1~GDPVAR$UNRATESTx) #yes
causality(varmod_gr, cause = "UNRATESTx")
coefs[c(4,1),c(4,1)]

x <- summary(varmod)

grangertest(GDPVAR$GDPC1~GDPVAR$FEDFUNDS)
causality(varmod_gr, cause = "FEDFUNDS")
coefs[c(4,3),c(4,3)]

grangertest(GDPVAR$GDPC1~GDPVAR$CPIAUCSL) #yes
causality(varmod_gr, cause = "CPIAUCSL")
coefs[c(4,5),c(4,5)]

grangertest(GDPVAR$GDPC1~GDPVAR$M1REAL) #yes
causality(varmod_gr, cause = "M1REAL")
coefs[c(4,6),c(4,6)]

grangertest(GDPVAR$GDPC1~GDPVAR$`S&P 500`) #yes
causality(varmod_gr, cause = "S.P.500")
coefs[c(4,7),c(4,7)]

#task e

VARselect(GDPVAR, type= "const", lag.max = 10)
varfc3 <- c()
for (i in 25:250) {
  varmod3 <- VAR(GDPVAR[1:i], p = 3, type = "const", season = NULL, exog = NULL) 
  forecasts3 <- predict(varmod3, n.ahead=1)
  varfc3 <- append(varfc3,forecasts3$fcst$GDPC1[1])
}

rmse(project1data_model[26:251],varfc3)

ts.plot(project1data_model[1:251])
points(arfc, type = "l", col = 2, lty = 2)
points(varfc, type = "l", col = 3, lty = 2)
points(varfc3, type = "l", col = 4, lty = 2)


data_varp_forecast_eo <- data.frame(project1data_final$sasdate[26:251],project1data_final$GDPC1[26:251],varfc3)
head(data_forecast_eo) #9/1/1959
head(data_var_forecast_eo) #6/1/1961 7 0s
head(data_varp_forecast_eo) #23 0s

colnames(data_forecast_eo) <- c("dates","gdp_real", "ar_forecast")
final_ar <- data.frame(data_forecast_eo)
var_missing <- data.frame(project1data_final$sasdate[3:9], 0, 0)
colnames(var_missing) <- c("dates", "gdp_real", "var_forecast")
colnames(data_var_forecast_eo) <- c("dates","gdp_real", "var_forecast")
final_var <- rbind(data_var_forecast_eo,var_missing )
final_var <- final_var%>%
  as.Date(final_var$dates, "%m/%d/%Y")%>%
  arrange(dates)
varp_missing <- data.frame(project1data_final$sasdate[3:25], 0,0)
colnames(varp_missing) <- c("dates", "gdp_real", "varp_forecast")
colnames(data_varp_forecast_eo) <- c("dates","gdp_real", "varp_forecast")
final_varp <- rbind(data_var_forecast_eo,var_missing )

final_plot <- data.frame(final_ar, final_var, final_varp)
final_plot$dates <- as.Date(final_plot$dates, "%m/%d/%Y")

varp_plot_total <- ggplot(final_plot, aes(dates)) +  
  geom_line(aes(y = gdp_real, colour = "actual GDP growth"))
  geom_line(aes(y = ar_forecast, colour = "AR(1) forecasted GDP growth")) +
  geom_line(aes(y = var_forecast, colour = "VAR(1) forecasted GDP growth")) +
  geom_line(aes(y = varp_forecast, colour = "VAR(3) forecasted GDP growth")) +
  ggtitle("Forecast of GDP Growth") + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Time") + ylab("actual GDP growth/ forecasted GDP growth in %") +
  theme(legend.position="bottom")

  plot(final_plot$gdp_real, type = "l")
  points(final_plot$ar_forecast, type = "l", col = 2, lty = 2)
  points(final_plot$var_forecast, type = "l", col = 3, lty = 2)
  points(final_plot$varp_forecast, type = "l", col = 4, lty = 2)


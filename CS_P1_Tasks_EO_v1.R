# Project Tasks 

library(dplyr)
library(formattable)
library(readr)
library(stargazer)
library(fpp2)
library(xtable)
library(stats)
library(Metrics)
library(lubridate)

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

#Task a
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


#Task b
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

arfc_ols <- c()
for (i in 2:250) {
  armod1_ols <- ar.ols(project1data_model[1:i], order = 1)
  forecasts_ols <- predict(armod1_ols, n.ahead = 1)
  arfc_ols <- append(arfc_ols,forecasts_ols$pred)
}

ts.plot(project1data_model[2:251])
points(arfc, type = "l", col = 2, lty = 2)
points(arfc_ols, type = "l", col = 3, lty = 2)


ar_data <- data.frame(project1data_final$sasdate[3:251],project1data_final$GDPC1[3:251], arfc)

rmse(project1data_model[3:251],arfc)


rmse(project1data_model[3:251],arfc_ols)

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

var_data <- data.frame(project1data_final$sasdate[10:251],project1data_final$GDPC1[10:251],varfc)

rmse(project1data_model[10:251],varfc)
# task d Granger

one_vec <- rep(1, (250))
z1 <- project1data_final$GDPC1[2:(251)]
z2 <- project1data_final$CUMFNS[2:(251)]
z3 <- project1data_final$UNRATESTx[2:(251)]
z4 <- project1data_final$CPIAUCSL[2:(251)]
z5 <- project1data_final$FEDFUNDS[2:(251)]
z6 <- project1data_final$M1REAL[2:(251)]
z7 <- project1data_final$`S&P 500`[2:(251)]
z <- cbind(one_vec,z1,z2,z3,z4,z5,z6,z7)
Z <- matrix(t(z), nrow = 8, ncol= 250 )
y1 <- project1data_final$GDPC1[3:(252)]
y2 <- project1data_final$CUMFNS[3:(252)]
y3 <- project1data_final$UNRATESTx[3:(252)]
y4 <- project1data_final$CPIAUCSL[3:(252)]
y5 <- project1data_final$FEDFUNDS[3:(252)]
y6 <- project1data_final$M1REAL[3:(252)]
y7 <- project1data_final$`S&P 500`[3:(252)]
y <- cbind(y1,y2,y3,y4,y5,y6,y7)
Y <- matrix(t(y), nrow = 7, ncol=250 )
A_hat <- Y %*% t(Z) %*% solve(Z %*% t(Z))
U <- Y - A_hat%*%Z
covar <- (U %*% t(U))*(1/242)
var <- kronecker(solve(Z %*% t(Z)),covar)
A_hat[1,2]/(sqrt(var[8,8])) #GDP
A_hat[1,3]/(sqrt(var[15,15])) #CUM
A_hat[1,4]/(sqrt(var[22,22])) #UNR
A_hat[1,5]/(sqrt(var[29,29])) #CPI
A_hat[1,6]/(sqrt(var[36,36])) #FED
A_hat[1,7]/(sqrt(var[43,43])) #M1
A_hat[1,8]/(sqrt(var[50,50])) #SP
qt(0.05/2, 241, lower.tail=FALSE)


#task e

VARselect(GDPVAR, type= "const", lag.max = 10)
varfc3 <- c()
for (i in 25:250) {
  varmod3 <- VAR(GDPVAR[1:i], p = 3, type = "const", season = NULL, exog = NULL) 
  forecasts3 <- predict(varmod3, n.ahead=1)
  varfc3 <- append(varfc3,forecasts3$fcst$GDPC1[1])
}

rmse(project1data_model[26:251],varfc3)

##Graph for task e

varp_data <- data.frame(project1data_final$sasdate[26:251],project1data_final$GDPC1[26:251],varfc3)
head(ar_data) #9/1/1959
head(var_data) #6/1/1961 7 0s
head(varp_data) #23 0s

colnames(ar_data) <- c("dates","gdp_real", "ar_forecast")
final_ar <- data.frame(ar_data)
var_missing <- data.frame(project1data_final$sasdate[3:9], 0, 0)
colnames(var_missing) <- c("dates", "gdp_real", "var_forecast")
colnames(var_data) <- c("dates","gdp_real", "var_forecast")
final_var <- rbind(var_data,var_missing )
final_var$dates <- as.Date(final_var$dates, "%m/%d/%Y")
final_var <- final_var%>%
  arrange(ymd(final_var$dates))

varp_missing <- data.frame(project1data_final$sasdate[3:25], 0,0)
colnames(varp_missing) <- c("dates", "gdp_real", "varp_forecast")
colnames(varp_data) <- c("dates","gdp_real", "varp_forecast")
final_varp <- rbind(varp_data,varp_missing )
final_varp$dates <- as.Date(final_varp$dates, "%m/%d/%Y")
final_varp <- final_varp%>%
  arrange(ymd(final_varp$dates))

final_plot <- data.frame(final_ar, final_var, final_varp)
final_plot$dates <- as.Date(final_plot$dates, "%m/%d/%Y")

plot(final_plot$gdp_real, type = "l")
points(final_plot$ar_forecast, type = "l", col = 2, lty = 2)
points(final_plot$var_forecast, type = "l", col = 3, lty = 2)
points(final_plot$varp_forecast, type = "l", col = 5, lty = 2)

varfc_2 <- c()
for (i in 9:251) {
  varmod_2 <- VAR(GDPVAR[1:i], p = 1, type = "const", season = NULL, exog = NULL) 
  forecasts <- predict(varmod, n.ahead=1)
  varfc <- append(varfc,forecasts$fcst$GDPC1[1])
}

summary(varmod_2)
covar


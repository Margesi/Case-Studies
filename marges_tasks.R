library(tidyverse)
library(writexl)
library(Metrics)
library(readr)

X2022_02 <- read_csv("2022-02.csv")
excel_file <- X2022_02[3:254,] %>%  dplyr::select(sasdate,GDPC1,CUMFNS,UNRATESTx,CPIAUCSL,FEDFUNDS,M1REAL,'S&P 500')
# lagging <- function(variable) {
#   return (variable - lag(variable))/variable
# }

growth_rates <- excel_file[,-1] %>% mutate_all( function(x) ((x-lag(x))/lag(x)*100 ))


colnames(growth_rates)<-c("GDPC1_gr","CUMFNS_gr","UNRATEST_gr", "CPIAUCSL_gr", "FEDFUNDS_gr","M1REAL_gr","S&P 500_gr"  )

alltogether<-cbind(excel_file,growth_rates)



#excel_file$sasdate<-NULL

#excel_file$sasdate<-as.Date(excel_file$sasdate,"%m/%d/%y",)
ts_alltogether<-ts(alltogether,start=1959, frequency = 4)
#excel_file %>% ggplot(aes(x=sasdate, y=c(GDPC1,CUMFNS)))+geom_line()



length(alltogether$GDPC1_gr[-1])


models <- list()

for (i in 3:251) {
  models[[i]]<- ar.ols(alltogether$GDPC1_gr[2:252][1:i] , 
                       order.max = 1)
  
}

predictions <-list()
prediction <- predict(model1,n.ahead = 1)                        


for (i in 2:251) {
  predictions[[i-1]] <- predict(models[[i]],n.ahead = 1)
  
}

predictions[[4]][[1]][1]

predicted<-vector()
for (i in 1:250) {
  predicted[i]<- predictions[[i]][[1]][1]
  
}


rmse(alltogether$GDPC1_gr[3:252], predicted)

alltogether$predictedgdpar_1[3:252]<-predicted

plot_file<-ts(plot_file,start=1959, frequency = 4)


autoplot(plot_file[,])+
  ggtitle("Capital Utilization", subtitle = "Capacity Utilization: Manufacturing (SIC) (Percent of Capacity)") +
  xlab("Year") + ylab("Percentage utilized")



# forecast <- c()
# for(i in 2:length(data$GDP_per)) {
#   GDP_ar<-arima(data$GDP_per[1:i],c(1,0,0))
#   GDP_ar_forecast <- predict(GDP_ar, n.ahead = 1)
#   new_value <- GDP_ar_forecast$pred           
#   forecast <- c(forecast, new_value)    
# }


ggp

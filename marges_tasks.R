library(tidyverse)
library(writexl)


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
  models[[i]]<- ar.ols(alltogether$GDPC1_gr[-1][1:i] , 
                       order.max = 1)
  
}

predictions <-list()
prediction <- predict(model1,n.ahead = 1)                        


for (i in 3:251) {
  predictions[[i]] <- predict(models[[i]],n.ahead = 1)
  
}

predictions[[4]][[1]][1]

predicted<-vector(length = 249)
for (i in 1:249) {
  predicted[i]<- predictions[[i+2]][[1]][1]
  
}

library(Metrics)
rmse(alltogether$GDPC1_gr[-c(1,2,3)], predicted)

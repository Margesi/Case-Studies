library(rpart)
library(tidyverse)
X2022_02 <- read_csv(file ="2022-02.csv")



project2data <- X2022_02[3:254,] %>%
  select(sasdate,GDPC1,CUMFNS,UNRATESTx,CPIAUCSL,FEDFUNDS,M1REAL,'S&P 500')

X2022_02 <- X2022_02[3:254,c(1,2,61,121,160,244,35,145)]

#Growth Rates
project2data_diff <- project2data[,-1] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x)) %>%
  select(colnames(project1data[,-c(1,3,4,6)]))

colnames(project2data_diff) <- paste0("%",colnames(project1data[,-c(1,3,4,6)]))

project2data <- cbind(X2022_02,project2data_diff)
project2data_final <- project2data[,c(1,3,7:12)]
lag(proje)

lags<- data_frame()
lags[]
for (j in 1:10) {
  example<- assign(paste0("lags",as.character(j), sep = ""),project2data_final[2:8])
  for (i in 2:8) {
    
    example[,i-1]<-lag(project2data_final[,i],n=j)
    
  }
  assign(paste0("lags",as.character(j), sep = ""),example)
   }
  

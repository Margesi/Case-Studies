library(dplyr)
library(formattable)
library(readr)
library(stargazer)
library(fpp2)
library(xtable)

X2022_02 <- read_csv(file ="Desktop/SS22/Case Studies/Project 1/2022-02.csv")


project1data <- X2022_02[3:254,] %>%
  select(sasdate,GDPC1,CUMFNS,UNRATESTx,CPIAUCSL,FEDFUNDS,M1REAL,'S&P 500')

str(project1data)
colSums(is.na(project1data))
#Growth Rates
project1data_diff <- project1data[,-1] %>% 
  mutate_all( function(x) 100*(x-lag(x))/lag(x)) %>%
  select(colnames(project1data[,-c(1,3,4,6)]))

colnames(project1data_diff) <- paste0("%",colnames(project1data[,-c(1,3,4,6)]))

project1data_final <- cbind(project1data,project1data_diff)


head(project1data_final,10)

#Descriptive Statistics
stargazer(project1data_final,type="text", digits = 2, median = TRUE)
##GDPC1
annotations1 <- data.frame(
  x = c(round(min(project1data_final$GDPC1), 2), 
        round(mean(project1data_final$GDPC1), 2), 
        round(max(project1data_final$GDPC1), 2), 
        round(median(project1data_final$GDPC1), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = GDPC1))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations1, 
            aes(x = c(3500,10500,19500,9000), y = c(23,12,8,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = 5100, y = -1,  label ="\U003BC - \u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 15000, y = -1,  label ="\U003BC + \u03C3" ), size = 2, fontface = "bold")+
  geom_vline(aes(xintercept = mean(GDPC1)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(GDPC1) + sd(GDPC1)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(GDPC1) - sd(GDPC1)), color = "#000000", size = 0.7, linetype = "dashed")+
labs(
    title = "Histogram of real GDP (billions of chained 2012 dollars)",
    x = "GPDC1",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##CUMFNS
stargazer(project1data_final[6], digits = 2, median = TRUE)
annotations2 <- data.frame(
  x = c(round(min(project1data_final$CUMFNS), 2), 
        round(mean(project1data_final$CUMFNS), 2), 
        round(max(project1data_final$CUMFNS), 2), 
        round(median(project1data_final$CUMFNS), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = CUMFNS))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations2, 
            aes(x = c(64,80,91,80), y = c(3,12,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = 69, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 90, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 64, y = 5,  label =as.character(project1data_final[which(round(project1data_final$CUMFNS,2)==63.76),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 91, y = 9.5,  label =as.character(project1data_final[which(round(project1data_final$CUMFNS,2)==91.57),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(CUMFNS)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(CUMFNS) + 2*sd(CUMFNS)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(CUMFNS) - 2*sd(CUMFNS)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Histogram of Capacity Utilization: Manufacturing (SIC)",
    subtitle ="(Percent of Capacity)",
    x = "CUMFNS",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))

## UNRATESTx
annotations3 <- data.frame(
  x = c(round(min(project1data_final$UNRATESTx), 2), 
        round(mean(project1data_final$UNRATESTx), 2), 
        round(max(project1data_final$UNRATESTx), 2), 
        round(median(project1data_final$UNRATESTx), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = UNRATESTx))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations3, 
            aes(x = c(2.7,4.75,12,4.75), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = 2.5, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 7.3, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 2.7, y = 21,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==2.81),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 12, y = 11,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==12.25),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(UNRATESTx)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(UNRATESTx) + 2*sd(UNRATESTx)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(UNRATESTx) - 2*sd(UNRATESTx)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Unemployment Rate less than 27 weeks (Percent)",
    x = "UNRATESTx",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))
##CPIAUCSL
annotations4 <- data.frame(
  x = c(round(min(project1data_final$CPIAUCSL), 2), 
        round(mean(project1data_final$CPIAUCSL), 2), 
        round(max(project1data_final$CPIAUCSL), 2), 
        round(median(project1data_final$CPIAUCSL), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = CPIAUCSL))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations4, 
            aes(x = c(28,131,278,131), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = 50, y = -1,  label ="\U003BC - \u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 220, y = -1,  label ="\U003BC + \u03C3" ), size = 2, fontface = "bold")+
  geom_vline(aes(xintercept = mean(CPIAUCSL)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(CPIAUCSL) + sd(CPIAUCSL)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(CPIAUCSL) - sd(CPIAUCSL)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Consumer Price Index for All Urban Consumers: All Items",
    subtitle = "(Index 1982-84=100)",
    x = "CPIAUCSL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"))


##FEDFUNDS
annotations5 <- data.frame(
  x = c(round(min(project1data_final$FEDFUNDS), 2), 
        round(mean(project1data_final$FEDFUNDS), 2), 
        round(max(project1data_final$FEDFUNDS), 2), 
        round(median(project1data_final$FEDFUNDS), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = FEDFUNDS))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations5, 
            aes(x = c(0,4.8,17.5,4.8), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -2.4, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 12, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  #geom_text(  aes(x = 0, y = 21,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==0.06),]$sasdate)), size = 2.5)+
  #geom_text(  aes(x = 17.5, y = 11,  label =as.character(project1data_final[which(round(project1data_final$UNRATESTx,2)==17.78),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(FEDFUNDS)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(FEDFUNDS) + 2*sd(FEDFUNDS)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(FEDFUNDS) - 2*sd(FEDFUNDS)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Effective Federal Funds Rate (Percent)",
    x = "FEDFUNDS",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

## M1REAL
annotations6 <- data.frame(
  x = c(round(min(project1data_final$M1REAL), 2), 
        round(mean(project1data_final$M1REAL), 2), 
        round(max(project1data_final$M1REAL), 2), 
        round(median(project1data_final$M1REAL), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = M1REAL))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations6, 
            aes(x = c(460,862,7278,642), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -200, y = -1,  label ="\U003BC - \u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 1900, y = -1,  label ="\U003BC + \u03C3" ), size = 2, fontface = "bold")+
  geom_vline(aes(xintercept = mean(M1REAL)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(M1REAL) + sd(M1REAL)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(M1REAL) - sd(M1REAL)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Real M1 Money Stock (Billions of 1982-84 Dollars)",
    x = "M1REAL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))
## S&P 500
annotations7 <- data.frame(
  x = c(round(min(project1data_final$'S&P 500'), 2), 
        round(mean(project1data_final$'S&P 500'), 2), 
        round(max(project1data_final$'S&P 500'), 2), 
        round(median(project1data_final$'S&P 500'), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final, aes(x = `S&P 500`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations7, 
            aes(x = c(55,782,4600,343), y = c(18,5,8,8),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -130, y = -1,  label ="\U003BC - \u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 1700, y = -1,  label ="\U003BC + \u03C3" ), size = 2, fontface = "bold")+
  geom_vline(aes(xintercept = mean(`S&P 500`)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(`S&P 500`) + sd(`S&P 500`)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(`S&P 500`) - sd(`S&P 500`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "S&P's Common Stock Price Index: Composite",
    x = "S&P 500",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthGDP
stargazer(project1data_final[3], digits = 2, median = TRUE)
annotations8 <- data.frame(
  x = c(round(min(project1data_final[-1,]$`%GDPC1`), 2), 
        round(mean(project1data_final[-1,]$`%GDPC1`), 2), 
        round(max(project1data_final[-1,]$`%GDPC1`), 2), 
        round(median(project1data_final[-1,]$`%GDPC1`), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final[-1,], aes(x = `%GDPC1`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations8, 
            aes(x = c(-9,0.7,7.5,0.7), y = c(23,12,8,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -1.48, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 3, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -9, y = 28,  label =as.character(project1data_final[which(round(project1data_final$`%GDPC1`,2)==-8.94),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 7.5, y = 13,  label =as.character(project1data_final[which(round(project1data_final$`%GDPC1`,2)==7.55),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(`%GDPC1`)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(`%GDPC1`) + 2*sd(`%GDPC1`)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(`%GDPC1`) - 2*sd(`%GDPC1`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Histogram of growth in GDP (percent)",
    x = "%GPDC1",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthCPIAUCSL
annotations9 <- data.frame(
  x = c(round(min(project1data_final[-1,]$`%CPIAUCSL`), 2), 
        round(mean(project1data_final[-1,]$`%CPIAUCSL`), 2), 
        round(max(project1data_final[-1,]$`%CPIAUCSL`), 2), 
        round(median(project1data_final[-1,]$`%CPIAUCSL`), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final[-1,], aes(x = `%CPIAUCSL`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations9, 
            aes(x = c(-2.3,0.9,4,0.8), y = c(5,12,5,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
   geom_text(  aes(x = -0.6, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
    geom_text(  aes(x = 2.4, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -2.3, y = 10,  label =as.character(project1data_final[which(round(project1data_final$`%CPIAUCSL`,2)==-2.29),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 4, y = 10,  label =as.character(project1data_final[which(round(project1data_final$`%CPIAUCSL`,2)==3.95),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(`%CPIAUCSL`)), color = "#000000", size = 0.5) +
   geom_vline(aes(xintercept = mean(`%CPIAUCSL`) + 2*sd(`%CPIAUCSL`)), color = "#000000", size = 0.7, linetype = "dashed") +
    geom_vline(aes(xintercept = mean(`%CPIAUCSL`) - 2*sd(`%CPIAUCSL`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Histogram of growth in CPI (percent)",
    x = "%CPIAUCSL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthM1Real
annotations10 <- data.frame(
  x = c(round(min(project1data_final[-1,]$`%M1REAL`), 2), 
        round(mean(project1data_final[-1,]$`%M1REAL`), 2), 
        round(max(project1data_final[-1,]$`%M1REAL`), 2), 
        round(median(project1data_final[-1,]$`%M1REAL`), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final[-1,], aes(x = `%M1REAL`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations10, 
            aes(x = c(-3.8,1.5,207.5,0.47), y = c(5,12,15,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -25, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 28, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
 # geom_text(  aes(x = -3.8, y = 18,  label =as.character(project1data_final[which(round(project1data_final$`%M1REAL`,2)==-3.82),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 207, y = 24,  label =as.character(project1data_final[which(round(project1data_final$`%M1REAL`,2)==207.56),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(`%M1REAL`)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(`%M1REAL`) + 2*sd(`%M1REAL`)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(`%M1REAL`) - 2*sd(`%M1REAL`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Histogram of growth in M1 (percent)",
    x = "%M1REAL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))

##GrowthM1Real2
annotations11 <- data.frame(
  x = c(round(min(project1data_final[-c(1,246),]$`%M1REAL`), 2), 
        round(mean(project1data_final[-c(1,246),]$`%M1REAL`), 2), 
        round(max(project1data_final[-c(1,246),]$`%M1REAL`), 2), 
        round(median(project1data_final[-c(1,246),]$`%M1REAL`), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final[-c(1,246),], aes(x = `%M1REAL`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations11, 
            aes(x = c(-3.82,0.67,33,0.47), y = c(7,12,5,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -4.5, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 6, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -3.8, y = 13,  label =as.character(project1data_final[which(round(project1data_final$`%M1REAL`,2)==-3.82),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 33, y = 10,  label =as.character(project1data_final[which(round(project1data_final$`%M1REAL`,2)==33.56),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(`%M1REAL`)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(`%M1REAL`) + 2*sd(`%M1REAL`)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(`%M1REAL`) - 2*sd(`%M1REAL`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Histogram of growth in M1 (percent)",
    x = "%M1REAL",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))
##GrowthS&P 500
annotations12 <- data.frame(
  x = c(round(min(project1data_final[-1,]$`%S&P 500`), 2), 
        round(mean(project1data_final[-1,]$`%S&P 500`), 2), 
        round(max(project1data_final[-1,]$`%S&P 500`), 2), 
        round(median(project1data_final[-1,]$`%S&P 500`), 2)),
  label = c("Min:", "Mean:", "Max:", "Median:")
) 

ggplot(project1data_final[-1,], aes(x = `%S&P 500`))+
  geom_histogram(bins=20,color = "white", fill = "gray")+
  geom_text(data = annotations12, 
            aes(x = c(-27.3,1.96,20.1,2), y = c(5,12,5,19),  label = paste(label, x)), size = 2.5, fontface = "bold")+
  geom_text(  aes(x = -10, y = -1,  label ="\U003BC - 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = 13.5, y = -1,  label ="\U003BC + 2\u03C3" ), size = 2, fontface = "bold")+
  geom_text(  aes(x = -27.3, y = 8,  label =as.character(project1data_final[which(round(project1data_final$`%S&P 500`,2)==-27.33),]$sasdate)), size = 2.5)+
  geom_text(  aes(x = 20, y = 8,  label =as.character(project1data_final[which(round(project1data_final$`%S&P 500`,2)==20.12),]$sasdate)), size = 2.5)+
  geom_vline(aes(xintercept = mean(`%S&P 500`)), color = "#000000", size = 0.5) +
  geom_vline(aes(xintercept = mean(`%S&P 500`) + 2*sd(`%S&P 500`)), color = "#000000", size = 0.7, linetype = "dashed") +
  geom_vline(aes(xintercept = mean(`%S&P 500`) - 2*sd(`%S&P 500`)), color = "#000000", size = 0.7, linetype = "dashed")+
  labs(
    title = "Histogram of growth in S&P 500 (percent)",
    x = "%S&P 500",
    y = "Count"
  )+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(
    plot.title = element_text(color = "black", size = 10, face = "bold"))
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

dGDPpercent <- diff(project1data_final[-1,]$`%GDPC1`, lag = 1, differences = 1)
acf(dGDPpercent, main = "ACF of the GDP growth")
acf(project1data_final[-1,]$`%GDPC1`, main = "ACF of the GDP growth")

dGDP <- diff(project1data_final$GDPC1, lag = 1, differences = 1)
acf(dGDP, main = "ACF of the GDP")

#Outliers/NAs
colSums(is.na(project1data_final))





# Description

X2022_02_1 <- X2022_02%>%
  select(sasdate,GDPC1,CUMFNS,UNRATESTx,CPIAUCSL,FEDFUNDS,M1REAL,'S&P 500')

variable_info <- as.data.frame(t(X2022_02_1[1:2,])) 
colnames(variable_info)<- (variable_info[1, ])
p <- variable_info%>%
  mutate(transformation = case_when(variable_info$transform ==1 ~ "no transformation",
                                   variable_info$transform ==2 ~ "delta x_t",
                                   variable_info$transform ==3 ~ "delta^2 x_t",
                                   variable_info$transform ==4 ~ "log(x_t)",
                                   variable_info$transform ==5 ~ "delta log(x_t)",
                                   variable_info$transform ==6 ~ "delta^2 log(x_t)",
                                   variable_info$transform ==7 ~ "delta (x_t/x_(t-1)-1.0)"),
         description = case_when(rownames(.) == "GDPC1" ~ "Real Gross Domestic Product",
                                  rownames(.) == "CUMFNS" ~ "Capacity Utilization: Manufacturing",
                                  rownames(.) == "UNRATESTx" ~ "Unemployment Rate less than 27 weeks",
                                  rownames(.) == "CPIAUCSL" ~ "Consumer Price Index for All Urban Consumers:",
                                  rownames(.) == "FEDFUNDS" ~ "Effective Federal Funds Rate",
                                  rownames(.) == "M1REAL" ~ "Real M1 Money Stock",
                                  rownames(.) == 'S&P 500' ~ "S&P's Common Stock Price Index:Composite"))%>%
  slice(-1) %>%
  select(transformation, description)

x <-formattable(p, align = c("c", "l","l"), type = "text")
xtable(x)

              
              
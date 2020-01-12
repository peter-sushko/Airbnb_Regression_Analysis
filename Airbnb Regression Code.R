##########################
rm(list = ls())
Dir <- "C:/Users/psush/Desktop/Econ 173/project"
setwd(Dir)
library(tidyverse)
library(stargazer)
###################
Data <- read.csv("FinalData.csv", header=TRUE, sep=",")

#pair1 = chicago, New Orleans
Data$season = ifelse(Data$Season == "Winter",1,0)
Data$treated = ifelse(Data$Latitude == "North",1,0)
Data$DiD = Data$treated * Data$season

Reg1Avg = lm(Avg_Price ~ treated + season + DiD, data = Data)
Reg1Median = lm(Median_Price ~ treated + season + DiD, data = Data)

#pair2 =  La, Seattle
Data$season = ifelse(Data$Season2 == "Winter",1,0)
Data$treated = ifelse(Data$Latitude2 == "North",1,0)
Data$DiD = Data$treated * Data$season


Reg2Avg = lm(Avg_Price2 ~ treated + season + DiD, data = Data)
Reg2Median = lm(Median_Price2 ~ treated + season + DiD, data = Data)

#pair3 = Minnesota, Austin
Data$season = ifelse(Data$Season3 == "Winter",1,0)
Data$treated = ifelse(Data$Latitude3 == "North",1,0)
Data$DiD = Data$treated * Data$season


Reg3Avg = lm(Avg_Price3 ~ treated + season + DiD, data = Data)
Reg3Median = lm(Median_Price3 ~ treated + season + DiD, data = Data)

#combined pairings
Data$season = ifelse(Data$Season_Total == "Winter",1,0)
Data$treated = ifelse(Data$Latitude_Total == "North",1,0)
Data$DiD = Data$treated * Data$season


RegAvgTotal = lm(Avg_Price_Total ~ treated + season + DiD, data = Data)
RegMedianTotal = lm(Median_Price_Total ~ treated + season + DiD, data = Data)

# table
  stargazer(Reg1Avg, Reg1Median, Reg2Avg, Reg2Median, Reg3Avg, Reg3Median, 
            title="Regression", type="text",
            column.labels=c("Chicago vs New Orleans", "Chicago vs New Orleans", "LA vs Seattle","LA vs Seattle","Minn. vs Austin","Minn. vs Austin"),
            df=FALSE, digits=2)
  
# table for combined pairs
  stargazer(RegAvgTotal, RegMedianTotal,
            title="Combined Pairs", type="text",
            column.labels=c("Total Average", "Total Median"),
            df=FALSE, digits=2)
  

############################# Chicago vs New Olreans
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(130,200))
  axis(1)
  axis(2)
  x = c(1,0)
  Chicago =  c(131, 161)
  NewOrleans = c(191,198)
  lines(x,Chicago, lwd=2, col="blue")
  lines(x,NewOrleans, lwd=2, col="green")
  title(ylab = "Average Price")
  title(xlab = "Season (0 = Summer, 1 = Winter)")
  title("Chicago (blue) vs New Orleans (green)")
  box()
############################ LA vs Seattle 
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(120,220))
  axis(1)
  axis(2)
  x = c(1,0)
  LA =  c(128, 213)
  Seattle = c(187,209)
  lines(x,LA, lwd=2, col="purple")
  lines(x,Seattle, lwd=2, col="red")
  title(ylab = "Average Price")
  title(xlab = "Season (0 = Summer, 1 = Winter)")
  title("Seattle (purple) vs LA (red) ")
  box()
#################### Minnesota vs Austin
  plot.new()
  plot.window(xlim=c(0,1), ylim=c(80,550))
  axis(1)
  axis(2)
  x = c(1,0)
  Minn =  c(433, 536)
  Austin = c(87,100)
  lines(x,Minn, lwd=2, col="yellow")
  lines(x,Austin, lwd=2, col="pink")
  title(ylab = "Average Price")
  title(xlab = "Season (0 = Summer, 1 = Winter)")
  title("Minneapolis (yellow) vs Austin (pink)")
  box()


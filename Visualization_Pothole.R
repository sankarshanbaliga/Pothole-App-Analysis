setwd("F:/Reap_Benefit/Data/Pothole App/")
library(ggplot2)
library(tidyverse)
library(geosphere)


#Read data
testData <- read.csv("teamData/SRB/20Jan2017005940.csv", header = TRUE, stringsAsFactors = FALSE)

testData <- testData[2:dim(testData)[1],]

#set names of variables
names(testData) <- c("accelx", "accely", "accelz","axisVar","isBump",
                      "Threshold.high", "Threshold.low", "car", 
                      "gyrx", "gyry", "gyrz", "lat", "Long",
                      "timeData", "accuracy")

#Make Cleaner for Timestamp
monthlist <- c("01", "02", "03", "04", "05","06",
            "07", "08", "09", "10", "11", "12")
names(monthlist) <- c("Jan", "Feb", "Mar", "Apr", "May","Jun",
                      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")



timeCleaner <- function(time) {
  timelist <- str_split(time, " ")
  timelist <- timelist[[1]]
  paste0(timelist[4], "-", monthlist[timelist[3]], "-", timelist[2],
         " ", timelist[5])
  }



#Additional Variables to be done

for (i in 1:dim(testData)[1]){
  testData$y_x_ratio[i] <- testData$accely[i] / testData$accelx[i]
  testData$y_z_ratio[i] <- testData$accely[i] / testData$accelz[i]
  testData$x_z_ratio[i] <- testData$accelx[i] / testData$accelz[i]
  testData$timeData_cln[i] <- timeCleaner(testData$timeData[i])
}

#cleaning up timeData
testData$timeData_cln <- ymd_hms(testData$timeData_cln, tz = "Asia/Kolkata")


  
#set overall index
testData$parentIndex <- 1:dim(testData)[1]

testData$distanceData <- 0
testData$timeDiff <- 0



# #Plot of accely
# zplot <- ggplot(testData, aes(x = parentIndex, y = accelz))+ geom_line()
# zplot
# 
# for(i in 2:dim(testData)[1]){
#   j = i-1
#   testData$distanceData[i] <- distVincentyEllipsoid(c(testData$lat[i], testData$Long[i]), 
#                                                     c(testData$lat[j], testData$Long[j]))
#   testData$timeDiff[i] <- testData$timeData_cln[i] -testData$timeData_cln[j]
# }
# 
# testData$spd <- testData$distanceData / testData$timeDiff
# summary(testData$timeDiff)
# summary(testData$distanceData)
# hist(log(testData$spd))

####POSITIVE#######
posdeviation <-  mean(testData$accelz) + 3 * sd(testData$accelz)
negdeviation <-  mean(testData$accelz) - 3 * sd(testData$accelz)

for (i in 1:dim(testData)[1]) {
  if(testData$accelz[i] > posdeviation ) {
    testData$bump[i] <- "Positive"
  } else if(testData$accelz[i] < negdeviation) {
    testData$bump[i] <- "Negative"
  } else {
    testData$bump[i] <- "Normal"
  }
}

#table(as.factor(testData$bump))

#check the data
zplot <- ggplot(testData, aes(x = parentIndex, y = accelz,
                              group = bump, colour = bump))+ geom_line()
zplot



#take out only positive idices
posData <- testData %>% filter(bump == "Positive")

#distance between bumps initial values
posData$dist = 0

#get distance between bumps in meters
for(i in 2:dim(posData)[1]){
  j <- i - 1
  ifelse(j > 0, j, 1)
  posData$dist[i] <- distVincentyEllipsoid(c(posData$lat[i], posData$Long[i]), 
                                   c(posData$lat[j], posData$Long[j]))
  }
#hist(posData$dist)

#Assume potholes will be at least ____n___ meters away from each other
n = 100 #meters
finalData <- posData %>% filter(dist > n)
write.csv(finalData, "PotholeMap.csv")

# ratioData <- testData %>% filter(y_x_ratio > 0)%>% filter(y_z_ratio > 0)%>% filter(bump == "Positive")
# 
# ratioData$y_x_ratio_log<- log(ratioData$y_x_ratio)
# ratioData$y_z_ratio_log<- log(ratioData$y_z_ratio)
# 
# ratioData$dist = 0
# 
# for(i in 2:dim(ratioData)[1]){
#   j <- i - 1
#   ifelse(j > 0, j, 1)
#   ratioData$dist[i] <- distVincentyEllipsoid(c(ratioData$lat[i], ratioData$Long[i]), 
#                                            c(ratioData$lat[j], ratioData$Long[j]))
# }
# 
# #Some interesting action going on
# zplot <- ggplot(ratioData, aes(x = y_x_ratio_log, y = y_z_ratio_log,
#                               group = bump, colour = bump))+ geom_point()
# zplot
# 
# finalData <- ratioData %>% filter(dist > 50)
# 
# summary(finalData$y_x_ratio_log)
# 
# write.csv(finalData, "PotholeMap.csv")

###Caroline Barrett
#######GEOG 331
##########Activity 3
################################

#set working directory
setwd("~/Desktop/geog_331")

#install packages
install.packages(c("lubridate"))

library(lubridate)

#read in data, skip first 3 rows
datW <- read.csv("bewkes_weather.csv",  na.strings=c("#N/A"), skip=3, header=FALSE)
sensorInfo <- read.csv("bewkes_weather.csv", na.strings=c("#N/A"), nrows=2)

#set colnames
colnames(datW) <- colnames(sensorInfo)

#change format of dates to m/d/y
dates <- mdy_hm(datW$timestamp, tz= "America/New_York")

#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)
#quick preview of new date calcualtions
datW[1,]

#check values with missing data
#air temp
length(which(is.na(datW$air.temperature)))

#wind speed
length(which(is.na(datW$wind.speed)))

#precipitation
length(which(is.na(datW$precipitation)))

#soil temperature
length(which(is.na(datW$soil.moisture)))

#plot doy and soil moisture
plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab = "Day of Year",
     ylab="Soil moisture (cm3 water per cm3 soil)")

#plot doy and air temp
plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab = "Day of Year",
     ylab="Air temperature (degrees C)")

#QAQC

#ifelse to invaluate data
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check values at extreme range of data and throughout percentiles
quantile(datW$air.tempQ1)

#days with really low air temp
datW[datW$air.tempQ1 < 8,]  

#days with really high air temp
datW[datW$air.tempQ1 > 33,]

#normalize lighling strikes to match precip
lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#plot precip and lightning activty (empty)
plot(datW$DD , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation & lightning",
     type="n")

#plot precip and lightning activty
points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15) 

#add points where there is only lighting
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#use assert function to check if same length
#create assert function
assert <- function(statement,err.message){
  if(statement == FALSE){
    print(err.message)}}
assert(length(datW$DD) == length(lightscale))


#filter out storms in wind and air temp measurements
#filer all values with lighning and greater rainfall than 2mm or over 5mm
#create new air temp column
datW$air.tempQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))


#remove suspect measurements from wind speed
#normalize lighling strikes to match wind speed
lightscale2 <- (max(datW$wind.speed)/max(datW$lightning.acvitivy)) * datW$lightning.acvitivy

#plot wind and lightning activty (empty)
plot(datW$DD , datW$wind.speed, xlab = "Day of Year", ylab = "Wind Speed & lightning",
     type="n")

#plot wind speed and lightning activty
points(datW$DD[datW$wind.speed > 0], datW$precipitation[datW$wind.speed > 0],
       col= rgb(95/255,158/255,160/255,.5), pch=15) 

#add points where there is only lighting
points(datW$DD[lightscale > 0], lightscale[lightscale > 0],
       col= "tomato3", pch=19)

#question 7

#filter out storms in wind and air temp measurements
#filer all values with lighning and greater wind speed than 0.5 or over 1.5
#create new air temp column

datW$wind.speedQ2 <- ifelse(datW$precipitation  >= 2 & datW$lightning.acvitivy >0, NA,
                          ifelse(datW$precipitation > 5, NA, datW$wind.speed))

#assert to verify 
assert(length(datW$wind.speedQ2) == length(datW$precipitation))

      
#plot windspeed 
#plot new wind and lightning activty (empty)
plot(datW$DD , datW$wind.speedQ2, xlab = "Day of Year", ylab = "Wind Speed & lightning",
     type="b")

#question 7
#test if soil temp and soil moisture are reliable in mid july
plot(datW$doy , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture",
     type="b")

plot(datW$doy , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Moisture",
     type="b")


#question 8

#find averages of variables
mean_air_temp <- round(mean(datW$air.temperature),digits = 1)
mean_wind_speed <- round(mean(datW$wind.speed),digits = 3)
mean_soil_moisture <- round(mean(datW$soil.moisture, na.rm = TRUE),digits = 5)
mean_soil_temp <- round(mean(datW$soil.temp, na.rm = TRUE),digits = 2)

# 2118 observations
# data collected from june 12-july 26

#total precip
total_precip <- round(sum(datW$precipitation))

#create new variable for table 
weather_averages <- c(mean_air_temp, mean_wind_speed, mean_soil_moisture, mean_soil_temp, total_precip)

#create  data.frame
data.frame(weather_averages)

#question 9

#create plot soil moisture over time

par(mfrow = c(2,2))
plot(datW$doy , datW$soil.moisture, xlab = "Day of Year", ylab = "Soil Moisture", col = "red",
     type="b")

plot(datW$doy , datW$soil.temp, xlab = "Day of Year", ylab = "Soil Temperature", col = "yellow",
     type="b")

plot(datW$doy , datW$air.temperature, xlab = "Day of Year", ylab = "Air Temperature", col = "orange",
     type="b")

plot(datW$doy , datW$precipitation, xlab = "Day of Year", ylab = "Precipitation", col = "blue",
     type="b")




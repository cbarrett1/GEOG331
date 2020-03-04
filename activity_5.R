###Caroline Barrett
#######GEOG 331
##########Activity 5
################################

#install packages
install.packages(c("lubridate"))

library(lubridate)

#read in data
datH <- read.csv("y:\\Data\\activities\\a05\\stream_flow_data.csv",
                 na.strings = c("Eqp"))
head(datH)  

#read in precipitation data
#hourly precipitation in mm
datP <- read.csv("y:\\Data\\activities\\a05\\2049867.csv")                          
head(datP)

#new data frame with only most reliable measurements
datD <- datH[datH$discharge.flag == "A",]



### define time for streamflow ###
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

### define time for precipitation ###   
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year 
datP$year <- year(dateP)

### get decimal formats ###
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year),datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))
#plot discharge
plot(datD$decYear, datD$discharge, type="l", xlab="Year", ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))

#basic formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy","dailySD")

#start new plot
dev.new(width=8,height=8)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2)

#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i")#remove gaps from axes  
#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)


#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       

axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle


#add legend
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       fill=c(NA,rgb(0.392, 0.584, 0.929,.2)),#fill boxes
       border=NA,#no border for both fill boxes (don't need a vector here since both are the same)
       bty="n")#no legend border

#make legend align with end of plot 
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy,aveF$dailyAve, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,90),
     xaxs="i", yaxs ="i",#remove gaps from axes
     axes=FALSE)#no axes
polygon(c(aveF$doy, rev(aveF$doy)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),#ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)       
axis(1, seq(0,360, by=40), #tick intervals
     lab=seq(0,360, by=40)) #tick labels
axis(2, seq(0,80, by=20),
     seq(0,80, by=20),
     las = 2)#show ticks at 90 degree angle
legend("topright", c("mean","1 standard deviation"), #legend items
       lwd=c(2,NA),#lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2)),#colors
       pch=c(NA,15),#symbols
       bty="n")#no legend border

#Question 5

#subset observations from 2017
#obs_year <- subset(datD, year == "2017")

#add line that shows observations for 2017

aveF <- aggregate(datD$discharge, by=list(datD$decDay), FUN="mean")
colnames(aveF) <- c("decDay","dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$decDay), FUN="sd")
colnames(sdF) <- c("decDay","dailySD")

#start new plot to show each month instead of doy
dev.new(width=8,height=8)

#increase margin size
par(mai=c(1,1,1,1))

#make plot
plot(aveF$decDay,aveF$dailyAve, 
     type="l", 
     xlab="Day of Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(-10,100),
     xaxs="i", yaxs ="i",  #remove gaps from axes
     axes=FALSE)   #no axes
polygon(c(aveF$decDay, rev(aveF$decDay)),#x coordinates
        c(aveF$dailyAve-sdF$dailySD,rev(aveF$dailyAve+sdF$dailySD)),   #ycoord
        col=rgb(0.392, 0.584, 0.929,.2), #color that is semi-transparent
        border=NA#no border
)     

#first days of month in year
first_dom <- c(1,32,60,91,121,152,182, 213, 244, 274, 305,335)

#change axis to be first day of month
axis(1, seq(0,360, by=31), #tick intervals every 31 days
     labels=(first_dom)) #tick labels
axis(2, seq(-80,80, by=20),
     seq(-80,80, by=20),
     las = 2)   #show ticks at 90 degree angle


#add 2017 discharge line

#subset for 2017

ave_year <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$doy[datD$year == 2017]), FUN="mean")
colnames(ave_year) <- c("doy","dailyAve")

sdF_year <- aggregate(datD$discharge[datD$year == 2017], by=list(datD$doy[datD$year == 2017]), FUN="sd")
colnames(sdF_year) <- c("doy","dailySD")

#add lines using subsets
lines(x=ave_year$doy, y=ave_year$dailyAve, col = "blue")

#create legend 
legend("topright", c("mean","1 standard deviation", "2017"),   #legend items
       lwd=c(2,NA, 2),   #add lines
       col=c("black",rgb(0.392, 0.584, 0.929,.2), "blue"),   #add colors
       pch=c(NA,15, NA),     #symbols
       bty="n")   #no legend border


#Question 7

#create dataframe that indicates what days will have full 24 hours of precip measurements

full_hours<- aggregate(datP, list(datP$doy, datP$year), length)
full_precip <-full_hours[full_hours$doy ==24,]

#start new plot
dev.new(width=8,height=8)

#increase margin size
par(mai=c(1,1,1,1))

#make plot
plot(datD$decYear, datD$discharge, 
     type="l", 
     xlab="Year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")),
     lwd=2,
     ylim=c(0,400),
     xaxs="i", yaxs ="i",  #remove gaps from axes
     axes=FALSE)   #no axes

names(full_precip)[1] <- "doy_good"
names(full_precip)[2] <- "year_good"

full_precip$decYear <- ifelse(leap_year(full_precip$year_good), full_precip$year_good + ((full_precip$doy_good -1)/366),
                              full_precip$year_good + ((full_precip$doy_good-1/365)))

points(x=full_precip$decYear,
       y = rep(325, length(full_precip$decYear),
               type="p",
               pch=7,
               col = "blue"))

axis(1, seq(2007, 2020, by=1), #tick intervals
     lab=seq(2007,2020, by=1)) #tick labels
axis(2, seq(0,400, by=50),
     seq(0,400, by=50),
     las=2) #show ticks at 90 degree angle

legend("topright", c("discharge","Days w/ 24 Hour Precip Measurements"),   #legend items
       lwd=c(2,NA),   #add lines
       col=c("black", "black"),   #add colors
       pch=c(NA,1),     #symbols
       bty="y")   #legend border

#######
#subsest discharge and precipitation within range of interest
hydroD <- datD[datD$doy >= 248 & datD$doy < 250 & datD$year == 2011,]
hydroP <- datP[datP$doy >= 248 & datP$doy < 250 & datP$year == 2011,]

#scale precip values
min(hydroD$discharge)

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl <- floor(min(hydroD$discharge))-1
#celing rounds up to the integer
yh <- ceiling(max(hydroD$discharge))+1
#minimum and maximum range of precipitation to plot
pl <- 0
pm <-  ceiling(max(hydroP$HPCP))+.5
#scale precipitation to fit on the 
hydroP$pscale <- (((yh-yl)/(pm-pl)) * hydroP$HPCP) + yl

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD$decDay,
     hydroD$discharge, 
     type="l", 
     ylim=c(yl,yh), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP)){
        polygon(c(hydroP$decDay[i]-0.017,hydroP$decDay[i]-0.017,
                  hydroP$decDay[i]+0.017,hydroP$decDay[i]+0.017),
                c(yl,hydroP$pscale[i],hydroP$pscale[i],yl),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#Question 8

#subsest discharge and precipitation within range of interest ()
hydroD2 <- datD[datD$doy >= 330 & datD$doy < 332 & datD$year == 2011,]
hydroP2 <- datP[datP$doy >= 330 & datP$doy < 332 & datP$year == 2011,]

#get minimum and maximum range of discharge to plot
#go outside of the range so that it's easy to see high/low values
#floor rounds down the integer
yl2 <- floor(min(hydroD2$discharge))-1
#celing rounds up to the integer
yh2 <- ceiling(max(hydroD2$discharge))+1
#minimum and maximum range of precipitation to plot
pl2 <- 0
pm2 <-  ceiling(max(hydroP2$HPCP))+.5
#scale precipitation to fit on the 
hydroP2$pscale <- (((yh2-yl2)/(pm2-pl2)) * hydroP2$HPCP) + yl2

par(mai=c(1,1,1,1))
#make plot of discharge
plot(hydroD2$decDay,
     hydroD2$discharge, 
     type="l", 
     ylim=c(yl2,yh2), 
     lwd=2,
     xlab="Day of year", 
     ylab=expression(paste("Discharge ft"^"3 ","sec"^"-1")))
#add bars to indicate precipitation 
for(i in 1:nrow(hydroP2)){
        polygon(c(hydroP2$decDay[i]-0.017,hydroP2$decDay[i]-0.017,
                  hydroP2$decDay[i]+0.017,hydroP2$decDay[i]+0.017),
                c(yl2,hydroP2$pscale[i],hydroP2$pscale[i],yl2),
                col=rgb(0.392, 0.584, 0.929,.2), border=NA)
}


#################
#box plots and violin plots

install.packages(c("lubridate"))
library(ggplot2)
#specify year as a factor
datD$yearPlot <- as.factor(datD$year)
#make a boxplot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_boxplot()

#make a violin plot
ggplot(data= datD, aes(yearPlot,discharge)) + 
        geom_violin()

#Question 9

#make factor variable for streamflow by season for 2016 and 2017
#meterorilogial seasons: Spring(March, April,May) Summer(June, July, Aug)....
# Winter days-- 
#spring days-- 6

#subset data for 2016 
datD_16 <-subset(datD, year == 2016)

#create function 

#winter = dec, jan, feb
#spring = march, april, may
#summer = june, july, august
#fall = sept, oct, nov

#create function for seasons 2016
datD_16_func <- function(doy) {
        if(doy < 59 | doy>= 334){
                seasonname = "winter"
        } else if (doy >= 60 & doy < 152){
                seasonname = "spring"
        } else if(doy >= 153 & doy <245) {
                seasonname = "summer"
        } else if (doy >= 246 & doy < 333){
                seasonname = "fall"
        }
        return(seasonname)
}


datD_16$season <- lapply(datD_16$doy, datD_16_func)
datD_16$season <- unlist(datD_16$season)
datD_16$season <- as.factor(datD_16$season)
is.factor(datD_16$season)

install.packages("ggplot2")
library(ggplot2)

#make a violin plot
ggplot(data= datD_16, aes(season,discharge)) + 
        geom_violin() + ggtitle("2016 Seasonal Discharge")




#subset data for 2017 
datD_17 <-subset(datD, year == 2017)

#create function 

#winter = dec, jan, feb
#spring = march, april, may
#summer = june, july, august
#fall = sept, oct, nov

#create function for seasons 2017
datD_17_func <- function(doy) {
        if(doy < 59 | doy>= 334){
                seasonname = "winter"
        } else if (doy >= 60 & doy < 152){
                seasonname = "spring"
        } else if(doy >= 153 & doy <245) {
                seasonname = "summer"
        } else if (doy >= 246 & doy < 333){
                seasonname = "fall"
        }
        return(seasonname)
}



datD_17$season <- lapply(datD_17$doy, datD_17_func)
datD_17$season <- unlist(datD_17$season)
datD_17$season <- as.factor(datD_17$season)
is.factor(datD_17$season)

#violin plot for 2017
ggplot(data= datD_17, aes(season,discharge)) + 
        geom_violin() + ggtitle("2017 Seasonal Discharge")


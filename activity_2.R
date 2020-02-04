###Caroline Barrett Activity 2###

#make a vector of tree heights
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
#look at first tree height
heights[1]
#2nd and 3rd
heights[2:3]

#matrix with 2 colums
#first argument is vector of numbers to fill in matrix
Mat <- matrix(c(1,2,3,4,5,6), ncol = 2, byrow = TRUE)
#subset matrix to look at row1 col2
Mat.bycol[1,2]
#look at all values in row 1
Mat.bycol[1,]
#look at all values in col 2
Mat.bycol[,2]

####Question 2####

#create vector with 5 character objects
charac <- c("dog", "mouse", "cat", "lion", "tyler")

#numeric vector
numer <- c(1.2, 3.4, 5.6, 7.8, 9.8)

#integer vector
integ <- as.integer(c(1,2,3,4,5))

#factor data vector
fdata<- factor(integ)

###Question 4###

#read in weather data
datW <- read.csv("Y:\\Students\\cbarrett1\\Data\\activities\\a02\\2011124.csv")

#for working on mac with file
#convert to csv file
#write.csv(datW, "datW_file.csv", row.names = FALSE)

#read in data with new file
#datW <- read.csv("datW_file.csv")

#more info about dataframe
str(datW)

#change date format from a factor to proper date format
#percent signs indicate data format
datW$dateF<-as.Date(datW$DATE, "%Y-%m-%d")

#create date column for only year as numeric
datW$year <- as.numeric(format(datW$dateF, "%Y"))

#mean max temp for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm = TRUE)

#average daily temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#calculate means across all sites
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)

#change col names
colnames(averageTemp) <- c("NAME","MAAT")

#change NAME values to numeric 
datW$siteN <- as.numeric(datW$NAME)

#add all histograms to same window
par(mfrow=c(2,2))

#create histogram of avg temp in site 1
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add line for mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation below mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation above mean
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#create histogram of avg temp in site 2
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add line for mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation below mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation above mean
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)


#create histogram of avg temp in site 3
hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add line for mean
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation below mean
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation above mean
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#create histogram of avg temp in site 4
hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add line for mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)

#add line for standard deviation below mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

#add line for standard deviation above mean
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)



#make histogram for first site in levels
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(levels(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")

#create seq of numbers to plot normal across range of temp
x.plot <- seq(-10,30, length.out = 100)

#dnorm function to find probabilitiy density based on mean and stand dev
y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#scale density to plot by changing range
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#add points to graph
points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)



#calculate probability of below freezing temperatures at site 1
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#calc all probability below 5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#probability temp between 0-5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))- pnorm(0,
                                                        mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                                                        sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#prob of area above 20 (subtract from 1)
1 - pnorm(20,
          mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
          sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#probability above 95%
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


###Question 6###

#probability above 95% with 4 degree increase

#mean site 1 + 4 degrees
mean1 <- (mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) + 4

qnorm(0.95,
      mean1,
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


###Question 7###

#histogram for daily precip in Aberdeen
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily precipitation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")


#calculate sum precip across all sites per site per year
sumPrecip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum",na.rm=TRUE)

#change col names
colnames(sumPrecip) <- c("site","year", "sum_precip")

#histogram for daily precip in Aberdeen
hist(sumPrecip$sum_precip[sumPrecip$"site" == "ABERDEEN, WA US"],
     freq=FALSE, 
     main = "ABERDEEN, WA US",
     xlab = "Annual precipitation (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#mean annual precipitation for all sites
avgPrecip <- aggregate(sumPrecip$sum_precip, by=list(sumPrecip$site), FUN="mean",na.rm=TRUE)

#change col names
colnames(avgPrecip) <- c("site", "avg_precip")



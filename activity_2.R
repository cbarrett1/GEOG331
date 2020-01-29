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





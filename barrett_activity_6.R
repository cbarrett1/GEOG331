###Caroline Barrett
#######GEOG 331
##########Activity 6
################################

#set working directory
#set working directory
setwd("~/Desktop/geog_331/a06")

#install packages
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
#readOGR in rgdal does this
g1966 <- readOGR("GNPglaciers/GNPglaciers_1966.shp")
g1998 <- readOGR("GNPglaciers/GNPglaciers_1998.shp")
g2005 <- readOGR("GNPglaciers/GNPglaciers_2005.shp")
g2015 <- readOGR("GNPglaciers/GNPglaciers_2015.shp")

#Question 1
#projection and datum for g1966
g1966@proj4string

#map glaciers by name
spplot(g1966, "GLACNAME")

#check glacier names for 1966 and 2015
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))


#read in rgb imagery from landsat
redL <- raster("glacier_09_05_14/l08_red.tif")
greenL <- raster("glacier_09_05_14/l08_green.tif")
blueL <- raster("glacier_09_05_14/l08_blue.tif")

#check coordinate system
redL@crs

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#use ext to zoom in on shapefiles
plotRGB(rgbL, ext=c(289995,310000,5371253,5400000), stretch="lin")
plot(g1966, col="palegreen2", border=NA, add=TRUE)
plot(g1998, col="royalblue3", add=TRUE, border=NA)
plot(g2005, col="darkgoldenrod4", add=TRUE, border=NA)
plot(g2015, col="tomato3", add=TRUE, border=NA)

#set up years to read in
ndviYear <- seq(2003,2016)

#read all NDVI files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("NDVI/NDVI_",ndviYear[i],".tif"))
  
}

#look at single raster
str(NDVIraster[[1]])

#get projection
NDVIraster[[1]]@crs

plot(NDVIraster[[1]])


#Question 3

#try to plot NDVI with 1966 glacier data
plot(NDVIraster[[1]])
plot(g1966, col="tan3", border=NA, add=TRUE)

#plot 2003 NDVI data next to 1966 glacier
par(mai=c(1,1,1,1))
#plot 2 maps next to each other
par(mfrow = c(1,2))

#plot NDVI
plot(NDVIraster[[1]], main = "NDVI")

#plot 1966 Glaciers
plotRGB(rgbL, stretch="lin", axes=TRUE, main = "1966 Glaciers")
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)


#Question 4

#reproject the glaciers
#use the NDVI projection
#spTransform(file to project, new coordinate system)
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)


#map max NDVI and glaciers in 2015 with reprojected glaciers

#plot NDVI for 2015
plot(NDVIraster[[13]], axes = FALSE)
#plot 2015 Glaciers
plotRGB(rgbL, stretch="lin", add =TRUE)
#add polygons to plot
plot(g2015p, col=NA, border= "black", add=TRUE)


#Question 5

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#join data together
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")

#plot the area for each glacier
plot(c(1966,1998,2005,2015), 
     c(gAll$a1966m.sq[1],gAll$a1998m.sq[1], gAll$a2005m.sq[1],gAll$a2015m.sq[1]),
     type="b", 
     pch=19, col=rgb(0.5,0.5,0.5,0.5), xlim= c(1965,2016),
     ylim=c(0,2000000),
     ylab="Area of glacier (meters squared)",
     xlab="Year")

for(i in 2:39){
  points(c(1966,1998,2005,2015), 
         c(gAll$a1966m.sq[i],gAll$a1998m.sq[i], gAll$a2005m.sq[i],gAll$a2015m.sq[i]),
         type="b", 
         pch=19, col=rgb(0.5,0.5,0.5,0.5))
  
}  



#calculate percent change in area of glaciers between 1966 and 2015
# sum1966-sum2015/sum1966
#sum 2015 = 14130990
#sum 1966 = 21568260
#> 21568260-14130990
#[1] 7437270
#> 7437270/21568260
#[1] 0.3448248
# %change = -34%

#create variable for %change
gAll$percent_change <- (gAll$a1966m.sq - gAll$a2015m.sq)/(gAll$a1966m.sq) * 100

#apply percent change to glaciers in 2015
g2015p@data$percent_change <- gAll$percent_change

#plot of glaciers in 2015 showing %change of each
spplot(g2015p,"percent_change",col="transparent")


#Question 6


#polygon that shows difference in glaciers from 1966-2015
diffPoly <- gDifference(g1966p, g2015p)
plot(diffPoly)

#plot glacier retreat with NDVI
plot(NDVIraster[[13]], axes=FALSE, box=FALSE)
plot(diffPoly,col="black", border=NA,add=TRUE)

#find glacier with largest %loss
gAll_max <- subset(gAll,percent_change==max(percent_change),select=c(GLACNAME))
#GLACNAME = Boulder

#create subsets for glacial extent for Boulder glacier all years
g1966Boulder <- subset(g1966, GLACNAME==gAll_max$GLACNAME)
g1998Boulder <- subset(g1998, GLACNAME==gAll_max$GLACNAME)
g2005Boulder <- subset(g2005, GLACNAME==gAll_max$GLACNAME)
g2015Boulder <- subset(g2015, GLACNAME==gAll_max$GLACNAME)

#plot each year in Boulder on map
par(mfrow=c(1,1))
plotRGB(rgbL,ext=c(272000,275600,5426000,5428500), stretch="lin",main="% Loss Boulder Glacier")
plot(g1966Boulder, col="blue", border=NA, add=TRUE)
plot(g1998Boulder, col="green", add=TRUE, border=NA)
plot(g2005Boulder, col="red", add=TRUE, border=NA)
plot(g2015Boulder, col="yellow", add=TRUE, border=NA)




#Question 7


#extract NDVI values from areas that experienced glacial retreat and find average
NDVIdiff <- list()
meanDiff <- numeric(0)
#loop through all NDVI years
for(i in 1:length(ndviYear)){
  #get raster values in the difference polygon
  NDVIdiff[[i]] <- extract(NDVIraster[[i]],diffPoly)[[1]]
  #calculate the mean of the NDVI values
  meanDiff[i] <- mean(NDVIdiff[[i]], na.rm=TRUE)
}

#plot NDVI and mean difference
plot(ndviYear, meanDiff, type="b",
     xlab= "Year",
     ylab="Average NDVI (unitless)",
     pch=19)


#designate that NDVIraster list is a stack
NDVIstack <- stack(NDVIraster)
#set up lm function to apply to every cell
#where x is the value of a cell
#need to first skip NA values (like lakes)
#if NA is missing in first raster, it is missing in all
#so we can tell R to assign an NA rather than fitting the function
timeT <- ndviYear
fun <- function(x) {
  if(is.na(x[1])){
    NA}else{
      #fit a regression and extract a slope
      lm(x ~ timeT)$coefficients[2] }}
#apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)
#plot the change in NDVI
plot(NDVIfit, axes=FALSE)


#Question 8

#buffer glaciers
glacier500m <- gBuffer(g1966p,#data to buffer
                       byid=TRUE,#keeps original shape id 
                       width=500)#width in coordinate system units

#convert to a raster
buffRaster <- rasterize(glacier500m,#vector to convert to raster
                        NDVIraster[[1]], #raster to match cells and extent
                        field=glacier500m@data$GLACNAME, #field to convert to raster data
                        background=0)#background value for missing data
plot(buffRaster)

#rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
#remove buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)


# Question 9 

meanChange <- zonal(NDVIfit, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
head(meanChange)

#add mean change in NDVI per year for glacZones
g2015p@data$meanChange <- meanChange[2:40]

#plot mean change NDVI for 2015 within each glacier polygon
spplot(g2015p,"meanChange", col = "transparent")




# Question 11

#mean NDVI across all years 
g2015p@data$NDVImean  



#dataframe of all NDVI values
meanMax <- zonal(NDVIstack, #NDVI function to summarize
                    glacZones,#raster with zones
                    "mean")#function to apply
#mean of each year
mean1 <- for(i in 1:length(meanMax)){
  NDVIraster[[i]] <- mean(ndviYear[i])}

#plot recent glacial extent and max NDVI range and raster of avg NDVI 
g2015p@data$NDVIcol <- ifelse(g2015p@data$NDVImean<0.4,"blue","red")
plot(g2015p, add=TRUE, col=paste(g2015p@data$NDVIcol),border=FALSE)


str(NDVIraster[[1]])





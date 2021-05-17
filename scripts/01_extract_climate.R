install.packages("ncdf4")
install.packages("raster")
library(ncdf4)
library(raster)

# load temp data
temp <- nc_open("C:\\Users\\Xingli Giam\\Downloads\\air.mon.mean.v501.nc")
tempbrick <- brick("C:\\Users\\Xingli Giam\\Downloads\\air.mon.mean.v501.nc")

## extract layers from 1959
tempbrick1<-tempbrick[[709:1416]]
tempbrick1.rotated <- rotate(tempbrick1)

# powdermill years 1959-2017
	# Lat 40 deg 10 min N, and Long 79 deg 16 min W

# convert latitude for powdermill
lat.powder = 40 + (10*60+0)/(60*60)
lon.powder = -(79 + (16*60+0)/(60*60))

# for palomarin
lat.palo = 37 + (56*60+0)/(60*60)
lon.palo = -(122 + (45*60+0)/(60*60))

# for teton
lat.teton = 43 + (40*60+20)/(60*60)
lon.teton = -(110 + (35*60+52)/(60*60))

# for waterfall
lat.waterfall = 41 + (41*60+55)/(60*60)
lon.waterfall = -(87 + (59*60+2)/(60*60))

# for panama
lat.panama = 9 + (9*60+9)/(60*60)
lon.panama = -(79 + (44*60+36)/(60*60))

# for brazil
lat.brazil = -(2 + (29*60+22)/(60*60))
lon.brazil = -(60 + (1*60+41)/(60*60)) 

# for guanica
lat.guanica = 17 + (58*60+32)/(60*60)
lon.guanica = -(66 + (52*60+5)/(60*60))

# for puerto rico
lat.pr = 18 + (2*60+4)/(60*60)
lon.pr = -(66 + (7*60+5)/(60*60))

# assemble lat long dataframe
latlon.df <-data.frame(Site=c("Powdermill","Palomarin","Teton","Waterfall","Panama","Brazil","Guanica","Puerto Rico"),
						Lat=c(lat.powder, lat.palo, lat.teton, lat.waterfall, lat.panama, lat.brazil, lat.guanica, lat.pr),
						Lon=c(lon.powder, lon.palo, lon.teton, lon.waterfall, lon.panama, lon.brazil, lon.guanica, lon.pr))

# establish rough extent of study sites						
ROI <- extent(-130,-50,-10,50)

# plot temperature grids over this extent for the year 1959
plot(crop(tempbrick1.rotated[[1]],ROI))		

# add points of sites
points(latlon.df$Lon, latlon.df$Lat, col="darkblue", pch=19)	## looks right!

# now extract the points from first year of dataset 1959
temp.df <- extract(tempbrick1.rotated, latlon.df[,c("Lon","Lat")], method="simple")	# note NAs for palo and guanica

## explore palo
ROI <- extent(-124,-121,35,39)	
plot(crop(tempbrick1.rotated[[1]],ROI))						
points(latlon.df$Lon, latlon.df$Lat, col="darkblue", pch=19)	## point is out of grid

## explore guanica
ROI <- extent(-67,-65,17,20)	
plot(crop(tempbrick1.rotated[[1]],ROI))						
points(latlon.df$Lon, latlon.df$Lat, col="darkblue", pch=19)	## point is out of grid
						
##	# now extract the points from first year of dataset 1959 - through simple extraction in all sites except palo and guanica
temp.df1 <- extract(tempbrick1.rotated, latlon.df[-c(2,7),c("Lon","Lat")], method="simple")			

####################################
####### manually move point up to overlap with nearest (eyeballed) cell (tentative method)
# palomarin
lat.palo1 = lat.palo + 0.25
#lon.palo = -(122 + (45*60+0)/(60*60)) # use original value

# guanica
lat.guanica1 = lat.guanica + 0.25
#lon.guanica = -(66 + (52*60+5)/(60*60)) # use original value

latlon_paloguanica.df <-data.frame(Site=c("Palomarin","Guanica"),
						Lat=c(lat.palo1, lat.guanica1),
						Lon=c(lon.palo, lon.guanica))
						
## explore palo (manually moved point)
ROI <- extent(-124,-121,35,39)	
plot(crop(tempbrick1.rotated[[1]],ROI))						
points(latlon_paloguanica.df$Lon, latlon_paloguanica.df$Lat, col="darkblue", pch=19)	## point is out of grid

## explore guanica (manually moved point)
ROI <- extent(-67,-65,17,20)	
plot(crop(tempbrick1.rotated[[1]],ROI))						
points(latlon_paloguanica.df$Lon, latlon_paloguanica.df$Lat, col="darkblue", pch=19)	## point is out of grid
					

##  # now extract the points through simple extraction for palo and guanica (moved points)
temp.df2 <- extract(tempbrick1.rotated, latlon_paloguanica.df[,c("Lon","Lat")], method="simple")			

### combine the two data frames
temp.final<-rbind(temp.df1, temp.df2)	
rownames(temp.final)<-c("Powdermill","Teton","Waterfall","Panama","Brazil","Puerto Rico","Palomarin","Guanica")

write.csv(temp.final, "E:\\Dropbox\\Bird_body_size-analysis\\temp.final.csv")		




############################################################# precipitation ## office computer


#precip <- nc_open("C:\\Users\\xgiam\\Downloads\\air.mon.mean.v501.nc")
precipbrick <- brick("C:\\Users\\xgiam\\Downloads\\precip.mon.total.v501.nc")



## extract layers from 1959
precipbrick1<-precipbrick[[709:1416]]
precipbrick1.rotated <- rotate(precipbrick)

# powdermill years 1959-2017
	# Lat 40 deg 10 min N, and Long 79 deg 16 min W

lat.powder = 40 + (10*60+0)/(60*60)
lon.powder = -(79 + (16*60+0)/(60*60))

# palomarin
lat.palo = 37 + (56*60+0)/(60*60)
lon.palo = -(122 + (45*60+0)/(60*60))

# teton
lat.teton = 43 + (40*60+20)/(60*60)
lon.teton = -(110 + (35*60+52)/(60*60))

# waterfall
lat.waterfall = 41 + (41*60+55)/(60*60)
lon.waterfall = -(87 + (59*60+2)/(60*60))

# panama
lat.panama = 9 + (9*60+9)/(60*60)
lon.panama = -(79 + (44*60+36)/(60*60))

# brazil
lat.brazil = -(2 + (29*60+22)/(60*60))
lon.brazil = -(60 + (1*60+41)/(60*60)) 

# guanica
lat.guanica = 17 + (58*60+32)/(60*60)
lon.guanica = -(66 + (52*60+5)/(60*60))

# puerto rico
lat.pr = 18 + (2*60+4)/(60*60)
lon.pr = -(66 + (7*60+5)/(60*60))

latlon.df <-data.frame(Site=c("Powdermill","Palomarin","Teton","Waterfall","Panama","Brazil","Guanica","Puerto Rico"),
						Lat=c(lat.powder, lat.palo, lat.teton, lat.waterfall, lat.panama, lat.brazil, lat.guanica, lat.pr),
						Lon=c(lon.powder, lon.palo, lon.teton, lon.waterfall, lon.panama, lon.brazil, lon.guanica, lon.pr))

# establish rough extent of study sites						
ROI <- extent(-130,-50,-10,50)
# plot precip grids over this extent for the year 1959
plot(crop(precipbrick1.rotated[[1]],ROI))						
# add points of sites
points(latlon.df$Lon, latlon.df$Lat, col="darkblue", pch=19)	## looks right!

# now extract the points from first year of dataset 1959
precip.df <- extract(precipbrick1.rotated, latlon.df[,c("Lon","Lat")], method="simple")	# note NAs for palo and guanica

## explore palo
ROI <- extent(-124,-121,35,39)	
plot(crop(precipbrick1.rotated[[1]],ROI))						
points(latlon.df$Lon, latlon.df$Lat, col="darkblue", pch=19)	## point is out of grid

## explore guanica
ROI <- extent(-67,-65,17,20)	
plot(crop(precipbrick1.rotated[[1]],ROI))						
points(latlon.df$Lon, latlon.df$Lat, col="darkblue", pch=19)	## point is out of grid
						

##	# now extract the points from first year of dataset 1959 - through simple extraction in all sites except palo and guanica
precip.df1 <- extract(precipbrick1.rotated, latlon.df[-c(2,7),c("Lon","Lat")], method="simple")			

####################################
####### manually move point up to overlap with nearest (eyeballed) cell (tentative method)
# palomarin
lat.palo1 = lat.palo + 0.25
#lon.palo = -(122 + (45*60+0)/(60*60)) # use original value

# guanica
lat.guanica1 = lat.guanica + 0.25
#lon.guanica = -(66 + (52*60+5)/(60*60)) # use original value

latlon_paloguanica.df <-data.frame(Site=c("Palomarin","Guanica"),
						Lat=c(lat.palo1, lat.guanica1),
						Lon=c(lon.palo, lon.guanica))
						
## explore palo (manually moved point)
ROI <- extent(-124,-121,35,39)	
plot(crop(precipbrick1.rotated[[1]],ROI))						
points(latlon_paloguanica.df$Lon, latlon_paloguanica.df$Lat, col="darkblue", pch=19)	## point is out of grid

## explore guanica (manually moved point)
ROI <- extent(-67,-65,17,20)	
plot(crop(precipbrick1.rotated[[1]],ROI))						
points(latlon_paloguanica.df$Lon, latlon_paloguanica.df$Lat, col="darkblue", pch=19)	## point is out of grid
					

##  # now extract the points through simple extraction for palo and guanica (moved points)
precip.df2 <- extract(precipbrick1.rotated, latlon_paloguanica.df[,c("Lon","Lat")], method="simple")			

### combine the two data frames
precip.final<-rbind(precip.df1, precip.df2)	
rownames(precip.final)<-c("Powdermill","Teton","Waterfall","Panama","Brazil","Puerto Rico","Palomarin","Guanica")

write.csv(precip.final, "D:\\Dropbox\\Bird_body_size-analysis\\precip.final.csv")		
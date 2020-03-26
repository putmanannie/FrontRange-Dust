#crop NLCD for use in the land cover part
#need to have NLCD raster dataset available for download.
library(rgdal)
library(mapdata)
library(dplyr)
library(broom)
library(raster)
library(sp)
library(sf)

xmn <- -110
xmx <- -101.5
ymn <- 37
ymx <- 42
xres <- 0.01
yres <- xres
transNLCD <- F
landcovershp <- file.path(basedir, 'path/to/shp/')

#change to landcover raster
nlcd <- raster(file.path(landcovershp, "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10.img"))
#make cropping extent
rx <- as(extent(xmn, xmx, ymn, ymx), 'SpatialPolygons')
#transform the cropping coords to those of nlcd raster
crs(rx)<-"+proj=longlat"
rx_utm <- spTransform(rx, crs(nlcd))
#crop the nlcd raster
nlcd_crop <- crop(nlcd, rx_utm)
writeRaster(nlcd_crop, file.path(landcovershp, "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10_crop.grd"), overwrite = T)

#I dont think that this actually works, but leaving it here in case we want to develop it...
if(transNLCD){
  #transform the cropped nlcd raster
  nlcd_trans <- projectRaster(nlcd_crop, crs = "+proj=longlat +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,-0,-0,-0,0 +units=m +no_defs")
  #write the new raster to a file so we just have to load in the future
  writeRaster(nlcd_trans, file.path(landcovershp, "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10_crop_trans.grd"), overwrite = T)
  nlcd_stilt <-nlcd_trans
}else{
  nlcd_stilt <-nlcd_crop
}

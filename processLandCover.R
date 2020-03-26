#Get the data to make the summary figure.

library(rgdal)
library(ggplot2)
library(dplyr)
library(broom)
library(raster)
library(sp)
library(sf)

season = 'summer' # can pick from summer, winter, fall, spring - case sensitive
continue = TRUE
basedir = 'Where/Files/Live'
stiltdir = 'stiltdir'
dataout = 'Path/to/outfile'
landcovershp <- file.path(basedir, 'Where/shpfiles/live')
nlcd_stilt <- raster(file.path(landcovershp, "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10_crop.grd"))

classdict <- as.data.frame(attributes(attributes(nlcd_stilt)$data)$attributes, row.names = NULL)[,c('ID', 'NLCD.2011.Land.Cover.Class')]
colnames(classdict)[2] <- 'NLCDcls'
classdict <- classdict[which(classdict$NLCDcls != ''),]
rownames(classdict) <- classdict$ID

#make a grouped dictionary with the first digit of the ID
#this is to combine grassland with shrub
classdict$ID[classdict$ID == 71] = 51
gclassdict <- as.data.frame(unique(floor(classdict$ID/10)))
colnames(gclassdict) <- c('Grouped')
rownames(gclassdict) <- gclassdict$Grouped
gclassdict['color'] <- c('black', 'cadetblue3', 'palevioletred2', 'azure3', 'palegreen4', 'olivedrab2', 'goldenrod1', 'burlywood3')
gclassdict['Name'] <- c('Unclassified', 'Water', 'Developed', 'Barren land', 'Forest', 'Shrub/Grassland', 'Agricultural', 'Wetland')

#need to select among these to make outfile
if (season == 'summer'){
  project <- c('SEEC_summer', 'BET_summer', 'GG_summer', 'C1_summer', 'SAD_summer')
  outfilename <- 'summer_LandTypeCts.rds'
}else if (season == 'winter'){
  project <- c('SEEC_winter', 'BET_winter', "GG_winter")
  outfilename <- 'winter_LandTypeCts.rds'
}else if (season == 'fall'){
  project <- c("BET_fall", "GG_fall")
  outfilename <- 'fall_LandTypeCts.rds'
}else if (season == 'spring'){
  project <- c('SEEC_spring', 'BET_spring', 'GG_spring')
  outfilename <- 'spring_LandTypeCts.rds'
}else{
  print('not a valid entry')
  continue = FALSE #this prevents the rest of the script from running
}
  
if (continue){
  #get the subset of particles that are below 0.5 m -- can change this threshold
  hthresh = 0.5
  projorder = 0
  initlandsubcts = T
  for (proj in project){
    projorder = projorder+1
    initparsub = T
    i = 1
    
    stilt_wd <- file.path(basedir, stiltdir, proj)
    output_wd <- file.path(stilt_wd, 'out', 'by-id')
    
    dirlist = list.dirs(path = file.path(output_wd))
    initdate = array(0, dim = c(length(dirlist)))
    
    setwd(file.path(output_wd))
    
    #get some information so we can extract the site code
    nend=as.numeric(gregexpr(pattern = '_', proj))-1
    #extract site ID from project name
    site_ID = substring(proj, 0, nend)
    #get the season from the project name
    season = substring(proj, nend+2, nchar(proj))
    
    for (direlement in dirlist){
      if (nchar(direlement) > nchar(output_wd)){
        f = substring(direlement, nchar(output_wd)+2, nchar(direlement))
        fpath = file.path(output_wd, f, paste0(f, '_traj.rds'))
        initdate[i] = as.Date(substring(f, 0, 8), format = "%Y%m%d")
        i = i+1
        #check if the file exists
        if (file.exists(fpath)){
          dat <- readRDS(fpath)
          #initialize the tibble with the contents of the first file
          if (initparsub){
            #grab the particle part of the tibble (dat$particle) and filter by height threshold
            particlesub = dat$particle[dat$particle[['zagl']] < hthresh, ]
  
            #number of particles
            npar = dim(dat$particle[dat$particle[['zagl']] < hthresh, ])[1]
  
            #add site ID and season to tibble as columns using mutate
  
            initparsub = F
            
          }else{
            #get some information so we can extract the site code
            nend=as.numeric(gregexpr(pattern = '_', proj))-1
            #number of particles
            npar = dim(dat$particle[dat$particle[['zagl']] < hthresh, ])[1]
  
            particlesub = bind_rows(particlesub, dat$particle[dat$particle[['zagl']] < hthresh, ])
          }
        }else{
          print(paste0('No existe ', fpath))
        }
      }
    }
    xy <- particlesub[, c('long', 'lati')]
    spdf <- SpatialPointsDataFrame(coords = xy, data = particlesub, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    spdf_utm <- spTransform(spdf, crs(nlcd_stilt))
    vals <- extract(nlcd_stilt, spdf_utm, sp = T)
    
    #rename so it's easier to deal with
    names(vals)[length(names(vals))] <- "nlcd_2011"
    vals_df <-as.data.frame(vals)
    #this is to combine grassland with shrub
    vals_df$nlcd_2011[vals_df$nlcd_2011 == 71] = 51
    vals_df$nlcd_2011[is.na(vals_df$nlcd_2011)] = 0
    vals_df['Grouped'] <- floor(vals_df$nlcd_2011/10)
    if(initlandsubcts){
      IDs <- names(summary(as.factor(vals_df$Grouped)))
      landtypects <- tibble('ID' = as.factor(names(summary(as.factor(vals_df$Grouped)))), 
                            'Values' = summary(as.factor(vals_df$Grouped))/sum(summary(as.factor(vals_df$Grouped))), 
                            'Season' = season, 'Site' = site_ID, 'Order' = projorder)   
      initlandsubcts = F
      
    }else{
      landtypects <- bind_rows(landtypects, tibble('ID' = as.factor(names(summary(as.factor(vals_df$Grouped)))), 
                                                'Values' = summary(as.factor(vals_df$Grouped))/sum(summary(as.factor(vals_df$Grouped))), 
                                                   'Season' = season, 'Site' = site_ID, 'Order' = projorder))
    }
  
  }
  
  saveRDS(landtypects, file.path(basedir, dataout, outfilename))
}

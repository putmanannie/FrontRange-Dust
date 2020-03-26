#process data to make the horizontal stacked bar plot of direction and distance.
library(rgdal)
library(dplyr)
library(broom)
library(sp)
library(sf)
library(geosphere)

basedir = 'Where/Files/Live'
stiltdir = 'stiltdir'
dataout = 'Path/to/outfile'
landcovershp <- file.path(basedir, 'Where/shpfiles/live')
sitename = 'SEEC' #options are SEEC, BET, GG
continue = TRUE

if (sitename == 'SEEC'){
  project <- c('SEEC_winter', 'SEEC_spring', 'SEEC_summer')
}else if (sitename == 'GG'){
  project <- c('GG_winter', 'GG_spring', 'GG_summer', 'GG_fall')
}else if (sitename == 'BET'){
  project <- c('BET_winter', 'BET_spring', 'BET_summer', 'BET_fall')
}else{
  print('not a valid site name')
  continue = FALSE
}

if (continue) {
  seasons <- c('winter', 'spring', 'summer', 'fall')
  outfilename <- paste(sitename, '_distancecounts.rds', sep = '')
  distthresh = c(1, 10, 100, 1000) #kilometer distance thresholds 
  
  #get the subset of particles that are below 0.5 m
  hthresh = 0.5
  starti = 1
  projorder = 0
  initparsub = T
  
  for (proj in project){
    projorder = projorder+1
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
          #get the site coords so we know if we are east or west and can calc distance  
          sitepoint <- data.frame(lat = c(dat$receptor$lati), long = c(dat$receptor$long)) 
          
          #initialize the tibble with the contents of the first file
          if (initparsub){
            #grab the particle part of the tibble (dat$particle) and filter by height threshold
            particlesub = dat$particle[dat$particle[['zagl']] < hthresh, ]                  
            initparsub = F
            
          }else{        
            particlesub = bind_rows(particlesub, dat$particle[dat$particle[['zagl']] < hthresh, ])
          }
        }else{
          print(paste0('No existe ', fpath))
        }
      }
    }
    endi = dim(particlesub)[1]
    particlesub[starti:endi, 'WE'] = ifelse(particlesub[starti:endi,'long'] < sitepoint[,'long'], -1, 1)
    particlesub[starti:endi,'dist'] = distm(particlesub[starti:endi, c('long', 'lati')], sitepoint[,c('long', 'lat')], fun = distGeo)/1000 # this is in km
    particlesub[starti:endi,'Season'] = rep(season, (endi-starti+1))
    particlesub[starti:endi,'Order'] = rep(projorder, (endi-starti+1))
    starti = endi+1
  }
  particlesub['dist_dir'] = particlesub[,'dist']*particlesub[,'WE']
  particlesub['catdist'] = 10^(floor(log10(particlesub[,'dist'])))
  particlesub$catdist[particlesub$catdist < 1] = 1
  particlesub$catdist[particlesub$catdist > 10000] = 10000
  particlesub$catdist = particlesub$catdist*particlesub$WE
  
  #categorize by threshold
  initdcts = T
  
  for (seas in unique(particlesub$Season)){
    subset = particlesub[which(particlesub$Season == seas),]
    if (initdcts){
      dcounts = tibble('Threshold' = as.numeric(names(summary(as.factor(subset$catdist)))), 
                       'Values'= summary(as.factor(subset$catdist))/sum(summary(as.factor(subset$catdist))),
                       'Season' = subset$Season[1], 'Order' = 1)
      initdcts = F
    }else{
      dcounts = bind_rows(dcounts, tibble('Threshold' = as.numeric(names(summary(as.factor(subset$catdist)))), 
                                          'Values'= summary(as.factor(subset$catdist))/sum(summary(as.factor(subset$catdist))),
                                          'Season' = subset$Season[1], 'Order' = 1))
    }
  }
  for (seas in seasons){
    if (!any(seas==unique(dcounts[,'Season']))) {
      dcounts[(dim(dcounts)[1]+1), ] = c(-1000, 0.0000000001, seas, 1)
      dcounts[(dim(dcounts)[1]+1), ] = c(-100, 0.0000000001, seas, 1)
      dcounts[(dim(dcounts)[1]+1), ] = c(-10, 0.0000000001, seas, 1)
      dcounts[(dim(dcounts)[1]+1), ] = c(-1, 0.0000000001, seas, 1)
      dcounts[(dim(dcounts)[1]+1), ] = c(1, 0.0000000001, seas, 1)
      dcounts[(dim(dcounts)[1]+1), ] = c(10, 0.0000000001, seas, 1)
      dcounts[(dim(dcounts)[1]+1), ] = c(100, 0.0000000001, seas, 1)
    }
  }
  
  saveRDS(dcounts, file.path(basedir, dataout, outfilename))
}  
  

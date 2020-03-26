#run this for each season to generate each plot by season


library(ggplot2)
library(dplyr)
library(broom)
library(RColorBrewer)
library(raster)

basedir = 'Where/Files/Live'
figfile = file.path(basedir, "path/to/Figures")
dataout = 'Path/to/outfile'
landcovershp <- file.path(basedir, 'Where/shpfiles/live')
nlcd_stilt <- raster(file.path(landcovershp, "nlcd_2011_landcover_2011_edition_2014_10_10/nlcd_2011_landcover_2011_edition_2014_10_10_crop.grd"))
season = 'summer' #options are summer, fall, winter, spring
continue = TRUE
makelegend = TRUE

#name of file out
outfilename <- paste(season, '_LandTypeCts.rds', sep='')

#figure name
fname<-paste(season, '_LandTypeCts.eps', sep='')
if (season == 'summer'){
  title = 'Jul, Aug, Sep'
}else if (season == 'fall'){
  title = 'Nov, Dec, Jan'
}else if (season == 'winter'){
  title = 'Feb Mar, Apr'
}else if (season == spring){
  title = 'May, Jun'
}else{
  print('Season input is invalid')
  continue = FALSE
}

if (continue){
  #read the rds file
  landtypects <- readRDS(file.path(basedir, dataout, outfilename))
  #need to rename sites
  
  namereplace <- c('SW', 'BT', 'SD')
  nameoriginal <- c('SEEC', 'BET', 'SAD')
  i = 1
  for (sitename in nameoriginal){
    inds <- which(landtypects[,'Site']==sitename)
    landtypects[inds,'Site']<-namereplace[i]
    i=i+1
  }
  
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
  
  setwd(figfile)
  
  ggplot(landtypects, aes(fill=ID, y=Values, x=reorder(Site, Order)))+ 
    geom_bar( stat="identity")+
    scale_color_manual(values = gclassdict$color, aesthetics = c('color', 'fill'))+
    labs(title = title, x = 'Site', y = '% interactions with land use category')+theme_bw(base_family = "Helvetica", base_size=12)
  ggsave(fname, units = "mm", width = 75, dpi = 500)
  if (makelegend){
    #make a plot just for the legend
    ggplot(landtypects, aes(fill=ID, y=Values, x=reorder(Site, Order)))+ 
      geom_bar( stat="identity")+
      scale_color_manual(values = gclassdict$color, aesthetics = c('color', 'fill'), name = 'Land type', labels = gclassdict$Name)+
      labs(title = "May, Jun", x = 'Site', y = '% interactions with land use category')+theme_bw(base_family = "Helvetica", base_size=12)
    ggsave('getLegend_landtypects.eps', units = "mm", width = 100, dpi = 500)
  }
}
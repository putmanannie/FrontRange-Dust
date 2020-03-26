#make a horizontal stacked bar plot with different distance classes and division between W and E. Normalize to number of interactions
library(rgdal)
library(ggplot2)
library(dplyr)
library(broom)
library(sp)
library(sf)


basedir = 'Where/Files/Live'
figfile = file.path(basedir, "To/Figures")
dataout = 'Path/to/outfile'
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

if (continue){
  outfilename <-paste(sitename, '_distancecounts', sep='')
  figname <-paste("TransportDirectionDistance_", sitename, ".png", sep='')
  dcounts <- readRDS(file.path(basedir, dataout, outfilename))
  
  mymin <- -1.0
  mymax <- 1.0
  
  palpos <- c('#f6e8c3','#dfc27d', '#bf812d')
  palneg <- c('#01665e', '#35978f', '#80cdc1', '#c7eae5')
  
  dcountsw <- dcounts[which(dcounts$Threshold < 0), ]
  dcountsw['color'] <- rep(palneg, projorder)
  dcountse <- dcounts[which(dcounts$Threshold > 0), ]
  dcountse['color'] <- rep(palpos, projorder)
  labs = c('1000+ km W', '100-1000 km W', '10-100 km W', '0-10 km W', '0-10 km E','10-100 km E', '100-1000 km E')
  
  #reorder(Season, Order)
  setwd(figfile)
  #based on http://rnotr.com/likert/ggplot/barometer/likert-plots/
  ggplot() + 
    geom_bar(data = dcountsw, aes(y=-Values*100, x=reorder(Season, Order), fill = color), stat="identity", position = "stack")+
    geom_bar(data = dcountse, aes(y=Values*100, x=reorder(Season, Order), fill = color), stat="identity", position = "stack")+
    geom_hline(yintercept = 0, color =c("white"))+
    scale_x_discrete(labels = c('summer' = "Jul, Aug,\nSep    ", 'fall' = 'Nov, Dec,\nJan     ', 
                                'winter' = 'Feb, Mar,\nApr      ', 'spring' = 'May, Jun'))+
    coord_flip()+
    labs(title=paste('Seasonality of air transport at', sitename), y="% interactions",x="Season") +  
    scale_fill_identity("direction and \ndistance ", labels = labs, breaks=c(palneg, palpos), guide = "legend")
  ggsave(figname, units = "in", width = 5, dpi = 500)
}
# ------------------------------------------------------------------------------------------------------------
# This code estimates the population reached within a buffer from a green area; the analysis is carried out
# as a comparison of the travel time-based accessibility analysis. (c) Henna Fabritius & Joel Jalkanen
# This version uses the same publicly available Helsinki green area polygons as the travel time analysis.
# ------------------------------------------------------------------------------------------------------------
library(raster) # raster
library(rgdal)  # readOGR
library(sp) # gridTopology
library(fields) # rdist
library(rgeos) # gBuffer, for creating buffers around polygons

ETRS_TM35FIN_projection<-"+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs" # projection of the habitat layer & travel grid
setwd() # set working folder of the analysis
threshold_distance<-500 # enter here distance in metres within which people benefit from a nearby green area

# Single-cell buffer analysis
# ------------------------------------------------------------------------------------------------------------
# The travel time grid that will be used for calculating distances to greenery:
travelgrid<-readOGR(dsn=paste0(getwd(),"/MetropAccess_YKR_grid_2018"),layer="MetropAccess_YKR_grid_EurefFIN",stringsAsFactors=F) # travelgrid has 13231 cells out of which 8509 are populated
proj4string(travelgrid) # this layer is already in the ETRS-TM35FIN projection
travelraster<-raster(ext=extent(travelgrid),crs=ETRS_TM35FIN_projection,resolution=250,vals=1) # A raster layer with the extent and resolution of the travel grid for creating output rasters

# Read population data for the travel grid and cut to the same extent
population<-readOGR(dsn=paste0(getwd(),"/Helsinki_Population_250m_2016"),layer="Helsinki_Population2016_250m",stringsAsFactors=F)
population$population<-as.numeric(population$population); population$YKR_ID<-as.integer(population$YKR_ID)
population<-population[(population$YKR_ID %in% travelgrid$YKR_ID),] # include only cells within the subset travelgrid
population_raster<-rasterize(population,travelraster,field=population$population)

# Read in the Helsinki greenery raster & extract to the travelgrid (NOTE: the code for creating this layer is below the analysis script)
Hki_CORINE2018_green_raster<-raster("Hki_greenery_CORINE.tif") # upload modified & simplified CORINE 2018 greenery raster for the Hki region
Hki_green_raster<-raster("Hki_greenery_raster.tif")

# Convert the Helsinki green area raster to polygons: Summed population densities within a buffer for all CORINE cells
population_raster<-focal(population_raster,ifelse(focalWeight(population_raster,threshold_distance,type="circle")>0,1,0),na.rm=T,fun=sum) # create a moving window surface that sums population densities within buffer distance
population_raster<-resample(population_raster,Hki_CORINE2018_green_raster)
green_areas<-population_raster; green_areas[is.na(Hki_CORINE2018_green_raster[])]<-NA # extract summed population counts within the buffer into the cells overlaid by CORINE greenery cells
writeRaster(green_areas,paste0("Buffer_analysis/population_in_",threshold_distance,"_m_buffer.tif"),overwrite=T)

plot(green_areas,col=colorRampPalette(c("green","yellow","orange","red","purple","blue","black"))(max(green_areas[],na.rm=T)+1))
plot(green_areas,col=colorRampPalette(c("lightgray","darkgray","black","purple","red"))(max(green_areas[],na.rm=T)+1))

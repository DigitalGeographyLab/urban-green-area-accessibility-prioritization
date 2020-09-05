# ------------------------------------------------------------------------------------------------------------
# This code creates green area accessibility rasters. (c) Henna Fabritius & Joel Jalkanen
# The analysis uses publicly available Helsinki green area spatial datasets:
# - a 20x20 m2 greenery map based on 2018 CORINE data (same extent as accessibility data)
# - polygons of large >10 km2 forest areas (from CORINE 2018 data)
# - Helsinki travel time & population data
# Incorporate travel times from each populated YKR grid cell to the greenery cells
# Convert to travel shares using the fitted travel share distance decay functions
# ------------------------------------------------------------------------------------------------------------
packages<-c("raster",      # raster
            "rgdal",       # readOGR
            "sp",          # gridTopology
            "matrixStats", # weightedMedian
            "rgeos",       # gCentroid
            "drc")         # predict.drc
install<-packages[!(packages %in% installed.packages()[,"Package"])]; if(length(install)>0) install.packages(as.character(install)) # install if packages missing
for(i in 1:length(packages)) require(packages[i],character.only=T) # load needed packages specified above

# set constants and variables
ETRS_TM35FIN_projection<-"+proj=utm +zone=35 +ellps=GRS80 +units=m +no_defs" # projection of the habitat layer & travel grid
setwd() # set working folder of the analysis
travel_type<-"walking" # walking, biking, public_transport: set the travel type here
median_type<-"min" # min, normal, weighted: for calculating travel time medians or minimum travel times per subregion
greenery_data<-"CORINE" # 250grid, CORINE: used for naming rasters according to analysis type
max_walking_time<-1000; max_biking_time<-1000; max_pt_time<-1000 # max travel times after which accessibility is not considered, we don't want to limit max_pt_time

# Read in the data layers used
# ------------------------------------------------------------------------------------------------------------
# The travel time grid that will be used for calculating times to greenery:
travelgrid<-readOGR(dsn=paste0(getwd(),"/MetropAccess_YKR_grid_2018"),layer="MetropAccess_YKR_grid_EurefFIN",stringsAsFactors=F) # travelgrid has 13231 cells out of which 8509 are populated
proj4string(travelgrid) # this layer is already in the ETRS-TM35FIN projection

# Read population data for the travel grid and cut to the same extent
population<-readOGR(dsn=paste0(getwd(),"/Helsinki_Population_250m_2016"),layer="Helsinki_Population2016_250m",stringsAsFactors=F)
population$population<-as.numeric(population$population); population$YKR_ID<-as.integer(population$YKR_ID)
population<-population[(population$YKR_ID %in% travelgrid$YKR_ID),] # include only cells within the subset travelgrid

# Read in the Helsinki greenery raster & extract to the travelgrid (NOTE: the code for creating this layer is below the analysis script)
if(travel_type=="walking") Hki_CORINE2018_green_raster<-raster("Hki_greenery_CORINE.tif") # upload modified & simplified CORINE 2018 greenery raster for the Hki region
if(travel_type=="biking") Hki_CORINE2018_green_raster<-raster("Hki_greenery_CORINE.tif") # upload modified & simplified CORINE 2018 greenery raster for the Hki region
if(travel_type=="public_transport") Hki_CORINE2018_green_raster<-raster("HMA_Acc_GIS/Corine_2018_forests_over10km2.tif") # upload raster of about >10km2 forests (from CORINE 2018)

# Download shapefile of postal codes (or another grid) to calculate regional pop-weighted medians
subregions<-readOGR(dsn=paste0(getwd(),"/HMA_Acc_GIS"),layer="cityDistricts2019",stringsAsFactors=F,use_iconv=T,encoding="UTF-8")
subregions<-spTransform(subregions,crs(travelgrid)) # transform to the same projection
subregions<-subregions[!is.na(over(subregions,gCentroid(population[population$population>0,],byid=T))),] # remove unpopulated subregions
subregions$code<-paste0(subregions$kunta,"_",subregions$kauposanro,"_",gsub("Ö","O",gsub("Ä","A",subregions$nimi))) # create individual & recognizable codes for raster naming
subregions$population<-NA

# Limit the analysis to grid cells that reside within the Greater Helsinki Region
travelgrid<-travelgrid[!is.na(over(gCentroid(travelgrid,byid=T),subregions)[,1]),]
population<-population[!is.na(over(gCentroid(population,byid=T),subregions)[,1]),]

# Plot to see data
plot(Hki_CORINE2018_green_raster,col="green")
plot(travelgrid,add=T)
plot(population[population$population>0,],col=rgb(0,0,1,alpha=0.5),add=T) # transparent colour to population maps
plot(subregions,add=T)

# Prepare raster creation (set travel grid cells to loop, the target folder etc.)
travelraster<-raster(ext=extent(Hki_CORINE2018_green_raster),crs=ETRS_TM35FIN_projection,resolution=20,vals=1) # Create a raster layer with the extent and resolution of the travel grid for creating output rasters

# set target folder (NOTE: make sure the target folder exists)
if(travel_type=="walking") target_folder<-paste0(getwd(),"/Generated_traveltime_rasters_walk_time_subregions_",greenery_data,ifelse(median_type=="weighted","_weighted","_min")) # where the created rasters are stored
if(travel_type=="biking") target_folder<-paste0(getwd(),"/Generated_traveltime_rasters_bike_slow_time_subregions_",greenery_data,ifelse(median_type=="weighted","_weighted","_min")) # where the created rasters are stored
if(travel_type=="public_transport") target_folder<-paste0(getwd(),"/Generated_traveltime_rasters_pt_midday_totaltime_subregions_",greenery_data,ifelse(median_type=="weighted","_weighted","_min"),"_forests") # where the created rasters are stored
ifelse(!dir.exists(target_folder),dir.create(target_folder),FALSE) # create the target folder if it doesn't exist

# Create a travel time raster layer for all populated cells: read the travel time matrix per grid cell, merge into the travel grid & rasterize travel times to all green areas
# ------------------------------------------------------------------------------------------------------------
positive_rasters<-c() # keep a list of rasters that contain accessible greeneries (i.e. are not on an island)
for(s in 1:nrow(subregions)){ # For each subregion, create a raster of pop-weighted median times

  # NOTE: We remove those populated cells from the analysis that contain greenery  
  subregions$population[s]<-sum(population$population[!is.na(over(gCentroid(population,byid=T),subregions[s,])[,1])])
  populated_IDs_sub<-population$YKR_ID[(population$population>0 & !is.na(over(gCentroid(population,byid=T),subregions[s,])[,1]))] # Get IDs of cells within subregion & with a population count > 0 (raster layers are made for each of these)
  travel_times<-matrix(NA,nrow=nrow(travelgrid),ncol=length(populated_IDs_sub)) # save travel times from each populated cell of the subregion to a matrix

  for(i in 1:length(populated_IDs_sub)){ # For each populated cell within the subregion,
  
    # Read and merge the travel times of the targeted populated grid cell to the travelgrid, then rasterize
    traveltime<-read.table(paste0(getwd(),"/HelsinkiRegion_TravelTimeMatrix_2018/",substr(populated_IDs_sub[i],1,4),"xxx/travel_times_to_ ",populated_IDs_sub[i],".txt"),header=T,sep=";") # Folder path to the travel time matrices based on the populated grid cell ID
    travelgrid_with_ID<-travelgrid # Make a copy of the travel time grid for joining travel times from a specific grid cell to the grid & then rasterizing
    travelgrid_with_ID<-sp::merge(travelgrid_with_ID,traveltime,by.x="YKR_ID",by.y="from_id") # join travel times to the grid
    if(travel_type=="walking") travel_times[,i]<-travelgrid_with_ID$walk_t # rasterize the travel times from the grid
    if(travel_type=="biking") travel_times[,i]<-travelgrid_with_ID$bike_s_t # rasterize the travel times from the grid
    if(travel_type=="public_transport") travel_times[,i]<-travelgrid_with_ID$pt_m_tt # rasterize the travel times from the grid
  }

  # remove unaccessible locations & skip raster creation altogether if all greenery is unaccessible from the subregion s
  travel_times[travel_times==-1]<-NA  # exclude the "unaccessible" locations, e.g. lakes, accessible but far away
  if(all(is.na(travel_times))==FALSE){ # remove the subregion if there are no accessible greeneries
    
  # calculate the population weighted median
  travelgrid_with_ID$travel_time<-NA # add the median travel times to the last grid
  if(median_type=="min") for(i in 1:nrow(travel_times)) if(min(travel_times[i,],na.rm=T)!=Inf){ travelgrid_with_ID$travel_time[i]<-min(travel_times[i,],na.rm=T)}
  if(median_type=="normal") for(i in 1:nrow(travel_times)) travelgrid_with_ID$travel_time[i]<-median(travel_times[i,],na.rm=T)
  if(median_type=="weighted") for(i in 1:nrow(travel_times)) travelgrid_with_ID$travel_time[i]<-weightedMedian(travel_times[i,],population$population[(population$YKR_ID %in% populated_IDs_sub)],na.rm=T)
  travelraster_with_ID<-rasterize(travelgrid_with_ID,travelraster,field=travelgrid_with_ID$travel_time) # add the pop-weighted median travel times to the subregion raster
  if(TRUE){ # Remove cells with overly long travel times for the travel mode
  if(travel_type=="walking") travelraster_with_ID[travelraster_with_ID[]>max_walking_time]<-NA
  if(travel_type=="biking") travelraster_with_ID[travelraster_with_ID[]>max_biking_time]<-NA
  if(travel_type=="public_transport") travelraster_with_ID[travelraster_with_ID[]>max_pt_time]<-NA
  }
  travelraster_with_ID[is.na(Hki_CORINE2018_green_raster[])]<-NA # remove non-greenery cells based on CORINE

  # CONVERT TRAVEL TIMES TO SHARES FROM TRAVELS
  # ------------------------------------------------------------------------------------------------------------------------------------
  # Load model that converts travel times to shares of travel
  if(travel_type=="walking") travel_share_model<-readRDS("Travel_time_distributions/model_walking_recreation.rds")
  if(travel_type=="biking") travel_share_model<-readRDS("Travel_time_distributions/model_biking_recreation.rds")
  if(travel_type=="public_transport") travel_share_model<-readRDS("Travel_time_distributions/model_pt_recreation.rds")
  
  # Predict to the raster
  travelraster_with_ID[]<-predict(travel_share_model,newdata=data.frame(time_class=travelraster_with_ID[]),interval="none")
  travelraster_with_ID[travelraster_with_ID[]>1]<-1 # set prediction cap to 1 for cells that have greenery in the same cell
  # ------------------------------------------------------------------------------------------------------------------------------------
  
  if(travel_type=="walking") writeRaster(travelraster_with_ID,paste0(target_folder,"/walk_t_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[s],".tif"),overwrite=T) # name maps based on the grid cell ID
  if(travel_type=="biking") writeRaster(travelraster_with_ID,paste0(target_folder,"/bike_s_t_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[s],".tif"),overwrite=T) # name maps based on the grid cell ID
  if(travel_type=="public_transport") writeRaster(travelraster_with_ID,paste0(target_folder,"/pt_m_tt_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[s],"_forests.tif"),overwrite=T) # name maps based on the grid cell ID
  positive_rasters<-c(positive_rasters,s) # add to the list of rasters that will be used for the Zonation analysis (the empty ones will be skipped)
}}

# Store a summary file with positive grid cell population weights and respective raster file paths
if(travel_type=="walking") write.table(data.frame(population=subregions$population[positive_rasters],Z_params=matrix(1,ncol=4,nrow=length(positive_rasters)),path=paste0(target_folder,"/walk_t_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[positive_rasters],".tif")),paste0(target_folder,"/summary_zonation_weights.txt"),sep="\t",row.names=F,col.names=F,fileEncoding="UTF-8")
if(travel_type=="biking") write.table(data.frame(population=subregions$population[positive_rasters],Z_params=matrix(1,ncol=4,nrow=length(positive_rasters)),path=paste0(target_folder,"/bike_s_t_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[positive_rasters],".tif")),paste0(target_folder,"/summary_zonation_weights.txt"),sep="\t",row.names=F,col.names=F,fileEncoding="UTF-8")
if(travel_type=="public_transport") write.table(data.frame(population=subregions$population[positive_rasters],Z_params=matrix(1,ncol=4,nrow=length(positive_rasters)),path=paste0(target_folder,"/pt_m_tt_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[positive_rasters],"_forests.tif")),paste0(target_folder,"/summary_zonation_weights.txt"),sep="\t",row.names=F,col.names=F,fileEncoding="UTF-8")

if(travel_type=="walking") write.table(data.frame(Z_params=matrix(1,ncol=5,nrow=length(positive_rasters)),path=paste0(target_folder,"/walk_t_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[positive_rasters],".tif")),paste0(target_folder,"/summary_zonation.txt"),sep="\t",row.names=F,col.names=F,fileEncoding="UTF-8")
if(travel_type=="biking")  write.table(data.frame(Z_params=matrix(1,ncol=5,nrow=length(positive_rasters)),path=paste0(target_folder,"/bike_s_t_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[positive_rasters],".tif")),paste0(target_folder,"/summary_zonation.txt"),sep="\t",row.names=F,col.names=F,fileEncoding="UTF-8")
if(travel_type=="public_transport") write.table(data.frame(Z_params=matrix(1,ncol=5,nrow=length(positive_rasters)),path=paste0(target_folder,"/pt_m_tt_raster_subregions_",greenery_data,"_",ifelse(median_type=="weighted","weighted_","_min_"),subregions$code[positive_rasters],"_forests.tif")),paste0(target_folder,"/summary_zonation.txt"),sep="\t",row.names=F,col.names=F,fileEncoding="UTF-8")

# ------------------------------------------------------------------------------------------------------------------
# Code for creating the Helsinki greenery raster where cells with >=50 % cover are counted as greenery cells
if(FALSE){ # if the travelgrid raster doesn't exist, read in greenery layer and rasterize cover per grid cell
  Hki_CORINE2018_green_raster<-raster("./../clc2018_fi20m/Clc2018_FI20m.tif")
  Hki_CORINE2018_green_raster<-crop(Hki_CORINE2018_green_raster,extent(travelgrid),snap="out")
  travelraster_CORINEres<-rasterize(travelgrid,Hki_CORINE2018_green_raster) # for removing corine values outside of travelgrid, create a raster with CORINE's resolution
  Hki_CORINE2018_green_raster[is.na(travelraster_CORINEres)]<-NA # set those CORINE cells NA that don't overlay with the travelgrid
  Hki_CORINE2018_green_raster[Hki_CORINE2018_green_raster[] %in% c(1:11,16,47:49)]<-NA # set those CORINE cells NA that are not greenery
  Hki_CORINE2018_green_raster[!is.na(Hki_CORINE2018_green_raster[])]<-1 # set those CORINE cells NA that are not greenery
  writeRaster(Hki_CORINE2018_green_raster,"Hki_greenery_CORINE",format="GTiff",overwrite=T)
}

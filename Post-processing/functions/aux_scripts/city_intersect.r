
# function to check for particles hitting the city
# accepts the "output" object from simulation_step.r
# this should be run with adopted maxagl heights

# using columns up to 3000m beccause that's where most of the action is
# using 300 particles that are pressure and AK weighted

city_intersect<-function(part, city_name){
  library(raster);library(dplyr)
  
  # read in trajs
  part<-readRDS(part)
  

  ###### get intersection with clustered city
  
  # list of urban core info
  urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
  # cluster code for this city 


  cluster_code<-urban_core$ID[urban_core$city == city_name]
  # all clusters - subset to this cluster
  cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
  cluster<-cluster[cluster$cluster_id == cluster_code, ]
  
  # Read in pop dataset - changed to density
  pop <- raster::brick("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif")    
  pop<-subset(pop, 1)
  

  pop<-crop(pop, extent(min(cluster$long)-0.15, max(cluster$long)+0.15, min(cluster$lat)-0.15, max(cluster$lat)+0.15))
  pop[is.na(pop[])]<-0 # this keeps all our coords
  
  coords<-rasterToPoints(pop)
  coords<-as.data.frame(coords)
  city_points <- SpatialPointsDataFrame(coords = cluster[,1:2], data = cluster[,1:2],
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # get raster cells with a city grid point in them
  city<- extract(pop, city_points, cellnumbers=TRUE)[,"cells"]
  all<-1:(dim(pop)[1]*dim(pop)[2])
  not_city<-setdiff(all, city)
  
  # set places not in clity cluster equal to 0
  # Don't want a tiny pocket of super high density near but not in the city to be flagged as the densest urban core
  pop[not_city] <- 0
  ### pop should now be a raster with 0 everywhere but for our clustered city
  
  
  # get particle information
  part<-part$particle
  hours<-seq(-1, min(part$time)/60, -1)
  
  # particle level footprints
  part<-part[part$foot > 0, ]
  city_flag<-extract(pop, part[, c(4,3)])
  part$city_flag<-city_flag
  part$city_flag[is.na(part$city_flag)]<-0
  
  # normalize foot
  part$foot<-part$foot/sum(part$foot)
  
  part$city<-part$foot*part$city_flag
  
  vect<-c()
  hours<-hours*60
  for(i in hours){
    val<-sum(part$city[part$time < i +60 & part$time > i])
    vect<-c(vect, val)
  }
  
  ### Get intersection with all demographic data
  pop <- raster::brick("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif")    
  pop<-subset(pop, 1)
  city_lon<-as.numeric(urban_core$lon[urban_core$city == city_name])
  city_lat<-as.numeric(urban_core$lat[urban_core$city == city_name])
  
  pop<-crop(pop, extent(city_lon-10, city_lon+10, city_lat-10, city_lat+10))
  pop[is.na(pop[])]<-0 # this keeps all our coords
  city_flag<-extract(pop, part[, c(4,3)])
  part$city_flag<-city_flag
  part$city_flag[is.na(part$city_flag)]<-0
  
  part$city<-part$foot*part$city_flag
  
  total_vect<-c()
  for(i in hours){
    val<-sum(part$city[part$time < i +60 & part$time > i])
    total_vect<-c(total_vect, val)
  }
  
  ### get intersection with city center (0.3x0.3 box for forward plume)
  pop <- raster::brick("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif")    
  pop<-subset(pop, 1)
  
  pop<-crop(pop, extent(urban_core$lon[urban_core$city== city_name]-0.15, urban_core$lon[urban_core$city== city_name]+0.15, urban_core$lat[urban_core$city== city_name]-0.15, urban_core$lat[urban_core$city== city_name]+0.15))
  pop[is.na(pop[])]<-0 # this keeps all our coords
  city_flag<-extract(pop, part[, c(4,3)])
  part$city_flag<-city_flag
  part$city_flag[is.na(part$city_flag)]<-0
  
  part$city<-part$foot*part$city_flag
  
  urban_vect<-c()
  for(i in hours){
    val<-sum(part$city[part$time < i +60 & part$time > i])
    urban_vect<-c(urban_vect, val)
  }
  ### combine

  df<-data.frame(urban = urban_vect, city = vect, all = total_vect)
  return(df)
  
}

# this uses the shape file to get the urban extent and not the population data

library(sf)
library(dplyr)


get_urban_extent <- function(city_name) {
   print("Getting urban extent for ODIAC city...")
     print(city_name)
    if (city_name == "NewYork") {
      city_name <- "New York"
    }
    if (city_name == "SanDiego") {
      city_name <- "San Diego, CA"
    }
    if (city_name == "Austin") {
      city_name <- "Austin, TX"
    }
    if (city_name == "Atlanta") {
      city_name <- "Atlanta, GA"
    }
    if (city_name == "Portland") {
      city_name <- "Portland, OR"
    }
    if (city_name == "Philadelphia") {
      city_name <- "Philadelphia, PA"
    }
    if (city_name == "SaltLakeCity") {
      city_name <- "Salt Lake City"
    }
    if (city_name == "Baltimore") {
      city_name <- "Baltimore, MD"
    }

     if (city_name == "Washington") {
      city_name <- "Washington--Arlington"
    }
  
     if (city_name == "Dallas_FortWorth") {
      city_name <- "Dallas--Fort Worth--Arlington, TX"
    }
     if (city_name == "LasVegas") {
      city_name <- "Las Vegas--Henderson--Paradise, NV"
    }

     if (city_name == "LosAngeles") {
      city_name <- "Los Angeles"
    }
     if (city_name == "SanAntonio") {
      city_name <- "San Antonio, TX"
    }
     if (city_name == "Boston") {
      city_name <- "Boston, MA--NH"
    }
    if (city_name == "Miami") {
      city_name <- "Miami--Fort Lauderdale, FL"
    }
    if (city_name == "Indianapolis") {
      city_name <- "Indianapolis, IN"
    }
    if (city_name == "Columbus") {
      city_name <- "Columbus, OH"
    }
    if (city_name == "Cleveland") {
      city_name <- "Cleveland, OH"
    }
    # Load the urban area US Census shapefile 
      shapefile_path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/data/Urban_Shapefiles/tl_2024_us_uac20.shp"
      gdf <- st_read(shapefile_path)
  

    # Find the matching city in the NAME20 attribute
    print("here")
    print(city_name)
    grepl_result <- grepl(city_name, gdf$NAME20, ignore.case = TRUE)  
    gdf_city <- gdf[grepl_result, ]
    print(gdf_city)
    # print(paste("Used:", gdf_city[1, ]$NAME20))
    # print("All Options:")
 
  
    if (nrow(gdf_city) == 0) {
      stop(paste("City not found in the shapefile:", city_name))
    }
  
    # If multiple matches, take the first one (you can modify this logic as needed)
    bbox <- st_bbox(gdf_city[1, ])

    # Return the bounding box in the required format
    return(list(
      lat = c(bbox$ymin, bbox$ymax),
      lon = c(bbox$xmin, bbox$xmax)
    ))
  }

shp_city_convolution<- function(foot,  city_name, urban_extent){
  library(ncdf4); library(raster); library(lubridate)

  print(paste0("footprint: ", foot))
  cat("shape file city convolution function\n")
  
    #
    
  foot_byid<-dirname(foot)
  print(foot_byid)
  print(city_name)
  # read in ODIAC data
  # year<-substr(basename(foot), 1,4)
  year<-"2023"

  
  month<-substr(basename(foot), 5,6)
  timestr=substr(basename(foot), 1,6)
  print(paste("Processing:", timestr))

  ## get ODIAC files
  emiss.path=file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/ODIAC/",year)
  
    # if timestr is after 2024, use the 2023 emissions
  if(timestr < '2024000000') {
    emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
                      recursive = T, full.names = T)
  }else{
    timestr = "2023123100"
    emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
                      recursive = T, full.names = T)
  }
  # emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
  #                     recursive = T, full.names = T)
  print(emiss_file)

  emissions<-raster(emiss_file)

 
  # urban_extent <- get_urban_extent(city_name)

  print(urban_extent)
  # coords<-rasterToPoints(emissions)
  # print(coords)
  # coords<-as.data.frame(coords)
  # coords <- as.matrix(expand.grid(urban_extent$lon, urban_extent$lat))
  # city_points <- SpatialPointsDataFrame(coords = coords, 
  #                                       data = data.frame(coords),
  #                                       proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
  #   # get raster emission cells with a city grid point in them
  # city<- extract(emissions, city_points, cellnumbers=TRUE)[,"cells"]
  # all<-1:(dim(emissions)[1]*dim(emissions)[2])
  # not_city<-setdiff(all, city)

  # emissions_city=emissions  
  # #   # set places not in city cluster equal to 0
  # #   # Don't want a tiny pocket of super high density near but not in the city to be flagged as the densest urban core
  # emissions_city[not_city] <- 0
  
  ### emission conversion function from tonnes C/ grid cell / month to umol/m2/s
  emission_conversion<-function(emissions){
    # units conversions for emissions (starts in tonnes C/ grid cell / month)
    emissions<-emissions*1000000 # gC/grid cell/month
    emissions<-emissions/12.011*1000000 #umol/grid cell/month
    area.raster <- raster::area(emissions) * 1E6    # convert km2 to m2
    emissions<-emissions/area.raster #umol/m2/month
    number_of_days<-as.numeric(lubridate::days_in_month(paste0(year,"-",month,"-01")))
    emissions<-emissions/(number_of_days*24*60*60) #umol/m2/s

    return(emissions)
  }

 
 
  # footprint stuff
 
  # deal with footprints that do not exist
  foot_exist<-file.exists(foot)
  print(foot)  
  
  if(foot_exist == T){
  # think the paste0() in here will solve broken symlinks for moved files
    footprint_file<-nc_open(foot)
    #save relevant foot information - lat/lon needed for referencing to GPW grid
    lons<-ncvar_get(footprint_file, varid="lon")
    lats<-ncvar_get(footprint_file, varid="lat")
    footprint<-ncvar_get(footprint_file, varid="foot")
    
    
    # footprint <- raster(foot)
    
    if (length(dim(footprint)) == 3) {
      footprint_raster <- brick(footprint, xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    } else {
      footprint_raster <- raster(footprint, xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    }
    # Crop each layer of the brick to the urban extent and combine into a new brick
    city_footprint <- brick(lapply(1:nlayers(footprint_raster), function(i) {
      crop(footprint_raster[[i]], extent(urban_extent$lon[1], urban_extent$lon[2], urban_extent$lat[1], urban_extent$lat[2]))
    }))
    # city_footprint <- crop(footprint_raster, extent(urban_extent$lon[1], urban_extent$lon[2], urban_extent$lat[1], urban_extent$lat[2]))
  
 
    
    emissions_urban <- crop(emissions, extent(min(lons)-0.0002, max(lons)+0.0002, min(lats)-0.0002,max(lats)+0.0002))
    # emissions_urban <-emissions
   
    emissions_city <- crop(emissions, extent(urban_extent$lon[1], urban_extent$lon[2], urban_extent$lat[1], urban_extent$lat[2]))
   
    
    #blank<-raster(nrow = length(lats), ncol = length(lons), xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats))
    #emissions2<-resample(emissions, blank, method = "ngb")
    emissions_city<-emission_conversion(emissions_city)
    emissions_urban<-emission_conversion(emissions_urban)
    
   
    # Resample emissions_city to match the resolution and extent of city_footprint
    emissions_city <- resample(emissions_city, city_footprint, method = "bilinear")
    # Resample emissions_urban to match the resolution and extent of footprint_raster (urban footprint)
    emissions_urban <- resample(emissions_urban, footprint_raster, method = "bilinear")
    
  
    
  
    if (!is.null(footprint) && length(dim(footprint)) >= 3) {
        hours <- 1:dim(footprint)[3]
    } else {
        hours <- 1
    }
  
    # hours<-1:dim(footprint)[3]
    influence_urban<-c()
    influence_city<-c()
    emissions_city<-raster::as.matrix(emissions_city)
    emissions_urban<-raster::as.matrix(emissions_urban)

    
    for(i in hours){
     
      val_urban<-sum(as.matrix(footprint_raster[[i]])*emissions_urban)
      # val_urban<-sum(footprint_raster[,,i]*emissions_urban)
      influence_urban<-c(influence_urban, val_urban)
      
 
      val_city <- sum(as.matrix(city_footprint[[i]])* emissions_city, na.rm = TRUE)
      # val_city<-sum(city_footprint[,,i]*emissions_city)
      influence_city<-c(influence_city, val_city)
    }
    df_urban<-data.frame(hour = hours, conv = influence_urban)
    df_city<-data.frame(hour = hours, conv = influence_city)

    fn_urban_influence<-paste0(foot_byid,"/odiac_urban_influence.txt")
    fn_city_influence<-paste0(foot_byid,"/odiac_city_influence.txt")
    
    write.table(df_urban, file=fn_urban_influence, quote=F, row.names=F)
    write.table(df_city, file=fn_city_influence, quote=F, row.names=F)
    
    print(paste("Saved urban influence to to:", fn_urban_influence))
    print(paste("Saved city influence to to:", fn_city_influence))

    nc_close(footprint_file)
  }
  gc()
  
  
}

# city_convolution_nhrs<- function(foot, foot_byid, nhrs, city,shp_file){
#   library(ncdf4); library(raster); library(lubridate)

#   for (i in 1:length(foot)){
#     foot=foot[i]
#     foot_byid=foot_byid[i]
#     nhrs=nhrs[i]
#     city_name=city[i]
#     if(nhrs=="-Inf") {
#       nhrs=24
#     } 
#     print(paste0("footprint: ", foot, " i: ", i))
#     # foot_byid<-dirname(foot)
#     print(foot_byid)
#     # print(city_name)
#   # read in ODIAC data
#   # year<-substr(basename(foot), 1,4)
#     year<-"2023"
   
    
#     month<-substr(basename(foot), 5,6)
#     timestr=substr(basename(foot), 1,6)
#     print(paste("Processing:", timestr))

#     ## get ODIAC files
#     emiss.path=file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/ODIAC_2021/",year)

  
#     emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
#                         recursive = T, full.names = T)
#     print(emiss_file)

#     emissions<-raster(emiss_file)
    
#     # crop emissions to city cluster


#     urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
#     # cluster code for this city 
#     cluster_code<-urban_core$ID[urban_core$city == city_name]
    
#     # all clusters - subset to this cluster
#     cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
#     cluster<-cluster[cluster$cluster_id == cluster_code, ]

    
    
#     emissions<-crop(emissions, extent(min(cluster$long)-11, max(cluster$long)+11, min(cluster$lat)-11, max(cluster$lat)+11))
#     emissions[is.na(emissions[])]<-0 # this keeps all our coords
    

#     coords<-rasterToPoints(emissions)
#     coords<-as.data.frame(coords)
#     city_points <- SpatialPointsDataFrame(coords = cluster[,1:2], data = cluster[,1:2],
#                                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
#     # get raster emission cells with a city grid point in them
#     city<- extract(emissions, city_points, cellnumbers=TRUE)[,"cells"]
#     all<-1:(dim(emissions)[1]*dim(emissions)[2])
#     not_city<-setdiff(all, city)
    
#     # set places not in city cluster equal to 0
#     # Don't want a tiny pocket of super high density near but not in the city to be flagged as the densest urban core
#     emissions[not_city] <- 0
    
#     ### emission conversion function from tonnes C/ grid cell / month to umol/m2/s
#     emission_conversion<-function(emissions){
#       # units conversions for emissions (starts in tonnes C/ grid cell / month)
#       emissions<-emissions*1000000 # gC/grid cell/month
#       emissions<-emissions/12.011*1000000 #umol/grid cell/month
#       area.raster <- raster::area(emissions) * 1E6    # convert km2 to m2
#       emissions<-emissions/area.raster #umol/m2/month
#       number_of_days<-as.numeric(lubridate::days_in_month(paste0(year,"-",month,"-01")))
#       emissions<-emissions/(number_of_days*24*60*60) #umol/m2/s
#       return(emissions)
#     }

#     emissions_city<-emission_conversion(emissions)
    
#     # footprint stuff
  
    
#     # deal with footprints that do not exist
#     foot_exist<-file.exists(foot)
#     print(foot)  
#     if(foot_exist == T){
#     # think the paste0() in here will solve broken symlinks for moved files
#       footprint_file<-nc_open(foot)
#       # save relevant foot information - lat/lon needed for referencing to GPW grid
#       lons<-ncvar_get(footprint_file, varid="lon")
#       lats<-ncvar_get(footprint_file, varid="lat")
#       footprint<-ncvar_get(footprint_file, varid="foot")
    
#       # slight shift in coords going on 10 **-10 but messing this up
#       ##get the city influence on by considering on the city corrdinates
#       emissions_city <- crop(emissions_city, extent(min(lons)-0.0002, max(lons)+0.0002, min(lats)-0.0002,max(lats)+0.0002))
#       #blank<-raster(nrow = length(lats), ncol = length(lons), xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats))
#       #emissions2<-resample(emissions, blank, method = "ngb")
      
#       emissions_city<-raster::as.matrix(emissions_city)
#       emissions_city[is.na(emissions_city)]<-0
      
#       # deal with this raster rotation
#       emissions_city<-t(emissions_city)[,dim(emissions_city)[1]:1] 

#       emissions_city_raster <- raster(emissions_city, xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
#       emissions_city <- resample(emissions_city_raster, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")


    
#       if (!is.null(footprint) && length(dim(footprint)) >= 3) {
#           hours <- 1:dim(footprint)[3]
#       } else {
#           hours <- 1
#       }
#       influence<-c()
#       emissions_city<-raster::as.matrix(emissions_city)

      
#       for(i in hours){
    
#         val<-sum(footprint[,,i]*emissions_city)
#         influence<-c(influence, val)
#       }
#       df<-data.frame(hour = hours, conv = influence)
#       fn_city_influence<-paste0(foot_byid,"/city_influence.txt")

#       write.table(df, file=fn_city_influence, quote=F, row.names=F)
#       print(paste("Saved city influence to to:", fn_city_influence))
    
#       ### get emissions influence of the entire footprint i.e city+suburban
#       emissions<-raster(emiss_file)

#       print(dim(emissions))

#         # slight shift in coords going on 10 **-10 but messing this up
#       emissions <- crop(emissions, extent(min(lons)-0.0002, max(lons)+0.0002, min(lats)-0.0002,max(lats)+0.0002))
#       #blank<-raster(nrow = length(lats), ncol = length(lons), xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats))
#       #emissions2<-resample(emissions, blank, method = "ngb")
#       emissions_urban<-emission_conversion(emissions)
      
    
#       emissions_urban<-raster::as.matrix(emissions_urban)
#       emissions_urban[is.na(emissions_urban)]<-0
      
#       # deal with this raster rotation
#       emissions_urban<-t(emissions_urban)[,dim(emissions_urban)[1]:1]
#       emissions_urban_raster <- raster(emissions_urban, xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
#       emissions_urban <- resample(emissions_urban_raster, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
    
    
#       if (!is.null(footprint) && length(dim(footprint)) >= 3) {
#           hours <- 1:dim(footprint)[3]
#       } else {
#           hours <- 1
#       }
    
#       # hours<-1:dim(footprint)[3]
#       influence<-c()
#       emissions_urban<-raster::as.matrix(emissions_urban)
#       for(i in hours){
#         val<-sum(footprint[,,i]*emissions_urban)
#         influence<-c(influence, val)
#       }
#       df<-data.frame(hour = hours, conv = influence)

#       fn_urban_influence<-paste0(foot_byid,"/urban_influence.txt")
#       write.table(df, file=fn_urban_influence, quote=F, row.names=F)
#       print(paste("Saved urban influence to to:", fn_urban_influence))
#       nc_close(footprint_file)
#     }
#     gc()
#   }
# }

# city_convolution_nhrs<-Vectorize(city_convolution_nhrs)


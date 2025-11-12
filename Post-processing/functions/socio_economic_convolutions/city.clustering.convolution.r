# fixed conversion issue 10/17/2022


cc_city_convolution<- function(foot,  city_name){
  library(ncdf4); library(raster); library(lubridate)
   cat("city file city convolution function\n")
  
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

  print(substr(timestr, 3, 6))
  print(emiss_file)

  emissions<-raster(emiss_file)
  
  # crop emissions to city cluster


  urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt", header=T, stringsAsFactors = F)
  # cluster code for this city 
 
  cluster_code<-urban_core$ID[urban_core$city == city_name]
  print(paste("cluster code:", cluster_code))
  # all clusters - subset to this cluster
  cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
  cluster<-cluster[cluster$cluster_id == cluster_code, ]

  
  
  emissions<-crop(emissions, extent(min(cluster$long)-11, max(cluster$long)+11, min(cluster$lat)-11, max(cluster$lat)+11))
  emissions[is.na(emissions[])]<-0 # this keeps all our coords
  
  print(dim(emissions))

  coords<-rasterToPoints(emissions)
  coords<-as.data.frame(coords)
  city_points <- SpatialPointsDataFrame(coords = cluster[,1:2], data = cluster[,1:2],
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  # get raster emission cells with a city grid point in them
  city<- extract(emissions, city_points, cellnumbers=TRUE)[,"cells"]
  all<-1:(dim(emissions)[1]*dim(emissions)[2])
  not_city<-setdiff(all, city)
  
  # set places not in city cluster equal to 0
  # Don't want a tiny pocket of super high density near but not in the city to be flagged as the densest urban core
  emissions[not_city] <- 0
  
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

  emissions_city<-emission_conversion(emissions)
  
  # footprint stuff
 
  
  # deal with footprints that do not exist
  foot_exist<-file.exists(foot)
  print(foot)  
  if(foot_exist == T){
  # think the paste0() in here will solve broken symlinks for moved files
    footprint_file<-nc_open(foot)
    # save relevant foot information - lat/lon needed for referencing to GPW grid
    lons<-ncvar_get(footprint_file, varid="lon")
    lats<-ncvar_get(footprint_file, varid="lat")
    footprint<-ncvar_get(footprint_file, varid="foot")
   
    # slight shift in coords going on 10 **-10 but messing this up
    ##get the city influence on by considering on the city corrdinates
    emissions_city <- crop(emissions_city, extent(min(lons)-0.0002, max(lons)+0.0002, min(lats)-0.0002,max(lats)+0.0002))
    #blank<-raster(nrow = length(lats), ncol = length(lons), xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats))
    #emissions2<-resample(emissions, blank, method = "ngb")
    
    emissions_city<-raster::as.matrix(emissions_city)
    emissions_city[is.na(emissions_city)]<-0
    
    # deal with this raster rotation
    emissions_city<-t(emissions_city)[,dim(emissions_city)[1]:1] 

    emissions_city_raster <- raster(emissions_city, xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    emissions_city <- resample(emissions_city_raster, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")


  
    if (!is.null(footprint) && length(dim(footprint)) >= 3) {
        hours <- 1:dim(footprint)[3]
    } else {
        hours <- 1
    }
    influence<-c()
    emissions_city<-raster::as.matrix(emissions_city)

    
    for(i in hours){
  
      val<-sum(footprint[,,i]*emissions_city)
      influence<-c(influence, val)
    }
    df<-data.frame(hour = hours, conv = influence)
    fn_city_influence<-paste0(foot_byid,"/city_influence.txt")

    write.table(df, file=fn_city_influence, quote=F, row.names=F)
    print(paste("Saved city influence to to:", fn_city_influence))
  
    ### get emissions influence of the entire footprint i.e city+suburban
    emissions<-raster(emiss_file)

    print(dim(emissions))

      # slight shift in coords going on 10 **-10 but messing this up
    emissions <- crop(emissions, extent(min(lons)-0.0002, max(lons)+0.0002, min(lats)-0.0002,max(lats)+0.0002))
    #blank<-raster(nrow = length(lats), ncol = length(lons), xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats))
    #emissions2<-resample(emissions, blank, method = "ngb")
    emissions_urban<-emission_conversion(emissions)
    
  
    emissions_urban<-raster::as.matrix(emissions_urban)
    emissions_urban[is.na(emissions_urban)]<-0
    
    # deal with this raster rotation
    emissions_urban<-t(emissions_urban)[,dim(emissions_urban)[1]:1]
    emissions_urban_raster <- raster(emissions_urban, xmn = min(lons), xmx = max(lons), ymn = min(lats), ymx = max(lats), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    emissions_urban <- resample(emissions_urban_raster, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
  
  
    if (!is.null(footprint) && length(dim(footprint)) >= 3) {
        hours <- 1:dim(footprint)[3]
    } else {
        hours <- 1
    }
  
    # hours<-1:dim(footprint)[3]
    influence<-c()
    emissions_urban<-raster::as.matrix(emissions_urban)
    for(i in hours){
      val<-sum(footprint[,,i]*emissions_urban)
      influence<-c(influence, val)
    }
    df<-data.frame(hour = hours, conv = influence)

    fn_urban_influence<-paste0(foot_byid,"/urban_influence.txt")
    write.table(df, file=fn_urban_influence, quote=F, row.names=F)
    print(paste("Saved urban influence to to:", fn_urban_influence))
    print("done in convolution")
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
#     emiss.path=file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/ODIAC/",year)

#       # if timestr is after 2024, use the 2023 emissions
#     if(timestr < '2024000000') {
#       emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
#                         recursive = T, full.names = T)
#     }else{
#       timestr = "2023123100"
#       emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
#                         recursive = T, full.names = T)
#     }
#     # emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
#     #                     recursive = T, full.names = T)
#     print(emiss_file)

#     emissions<-raster(emiss_file)
    
#     # crop emissions to city cluster


#     urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt", header=T, stringsAsFactors = F)
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
#        if (shp_file){
#             fname1="/city_influence_shp.txt"
#             fname2="/urban_influence_shp.txt"
#           }else{
#             fname1="/city_influence.txt"
#             fname2="/urban_influence.txt"
#           }
#       fn_city_influence<-paste0(foot_byid,fname1)

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

#       fn_urban_influence<-paste0(foot_byid,fname2)
#       write.table(df, file=fn_urban_influence, quote=F, row.names=F)
#       print(paste("Saved urban influence to to:", fn_urban_influence))
#       nc_close(footprint_file)
#     }
#     gc()
#   }
# }

# city_convolution_nhrs<-Vectorize(city_convolution_nhrs)
cc_city_convolution<-Vectorize(cc_city_convolution)


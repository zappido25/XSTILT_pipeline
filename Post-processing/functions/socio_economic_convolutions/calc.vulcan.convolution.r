# fixed conversion issue 10/17/2022


calc.vulcan.convolution<- function(foot_ff, foot_byid_ff, nhrs_ff, city,shp_file){
  library(ncdf4); library(raster); library(lubridate)
   cat("vulcan-city convolution function\n")
    
    city=city[1]

    if (!shp_file) {
      print("using city clustering method")
      urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
      # cluster code for this city 
      cluster_code<-urban_core$ID[urban_core$city == city]
      
      # all clusters - subset to this cluster
      cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
      cluster<-cluster[cluster$cluster_id == cluster_code, ]
      print(paste0("cluster code: ", cluster_code))
      
    } else {
        print("using us census")
        urban_extent <- get_urban_extent(city)
    }
        ## get Vulcan files
    emiss.path=file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/Vulcan//VULCAN_WGS84/")

    emission_conversion<-function(emissions){
      # units conversions for emissions (starts in tonnes CO2/ grid cell / year)
      emissions = emissions* (12.011/44.009) # convert tonnes CO2/ grid cell / year to tonnes C/ grid cell / year
      emissions<-emissions*1000000 / 12 # tonnes CO2/grid cell/year -> gCO2/grid cell/month
      emissions<-emissions/12.011*1000000 #umol/grid cell/month
      area.raster <- raster::area(emissions) * 1E6    # convert km2 to m2
      emissions<-emissions/area.raster #umol/m2/month
      number_of_days<-as.numeric(lubridate::days_in_month(paste0(year,"-",month,"-01")))
      emissions<-emissions/(number_of_days*24*60*60) #umol/m2/s
      return(emissions)
    }

    month<-substr(basename(foot_ff[1]), 5,6)
    year<-substr(basename(foot_ff[1]), 1,4)
    timestr=substr(basename(foot_ff[1]), 1,6)
    print(paste("Processing:", timestr))
    print(paste("year:", year))



  # if timestr is after 2022, use the 2022 emissions
    if(as.numeric(year) > 2022) {
      year_emiss=2022
      pattern <- paste0("tot.*", year_emiss, ".*\\.tif$")
      emiss_file = list.files(path = emiss.path, pattern =  pattern,
                        recursive = T, full.names = T)
      # print(emiss_file)                   
    }else{
      # emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
      #                   recursive = T, full.names = T)
    year_emiss=year
      pattern <- paste0("tot.*", year_emiss, ".*\\.tif$")
    emiss_file = list.files(path = emiss.path, pattern =   pattern, 
                        recursive = T, full.names = T)
                        # print(emiss_file)
    }

    emissions<-raster(emiss_file)

    if (!shp_file) {
      emissions<-crop(emissions, extent(min(cluster$long)-15, max(cluster$long)+15, min(cluster$lat)-15, max(cluster$lat)+15))
      emissions[is.na(emissions[])]<-0 # this keeps all our coords
      

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

      emissions_city<-emission_conversion(emissions)
      
      print(emissions_city)

      print(substr(timestr, 3, 6))
      print(emiss_file)
      
    } else{
      print("using us census")
      print(urban_extent)
      emissions_city <- crop(emissions, extent(urban_extent$lon[1], urban_extent$lon[2], urban_extent$lat[1], urban_extent$lat[2]))
      emissions_city<-emission_conversion(emissions)
    }

  for (i in seq_along(foot_ff)) {
     
    foot=foot_ff[i]
    foot_byid=foot_byid_ff[i]
    nhrs=nhrs_ff[i]
    print(paste0("nhrs: ",nhrs))
  
  #

    emissions_city_i=emissions_city
    
 
    if (is.na(foot) || is.na(foot_byid)) {
      message("Skipping i=", i, " due to NA path")
      next
    }
    if(is.na(foot)){return(NA)}
    print(paste0("footprint: ", foot, " i: ", i))
    foot_exist<-file.exists(foot)
    foot <- gsub("[\r\n]", "", foot)
    # select only timesteps for 2:nhrs. 1 is thereceptor timestamp
    foot_timestr<-substr(basename(foot), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
    
    print(foot_exist)
   
   
    if(foot_exist == T){

      footprint_file<-try(nc_open(foot), silent =T)
      
      if(length(footprint_file) > 1){
        
        foot_lon<-ncvar_get(footprint_file, varid="lon")
        foot_lat<-ncvar_get(footprint_file, varid="lat")
        footprint<-ncvar_get(footprint_file, varid="foot")
        
        foot_hours<-ncvar_get(footprint_file, varid="time")
        foot_hours<-as.POSIXct(foot_hours, tz="UTC", origin = "1970-01-01 00:00:00 UTC")
        foot_hours<-foot_hours - as.numeric(substr(foot_hours, 18,19))
        
        new_flag<-foot_hours %in% foot_timestrs
        if(TRUE %in% new_flag){
          new_flag<-(1:length(new_flag))[new_flag == TRUE]

          footprint<-footprint[,,new_flag]
          footprint<-apply(footprint, c(1,2), sum, na.rm = TRUE)
          footprint_raster <- raster(as.matrix(footprint))

          extent(footprint_raster) <- extent(min(foot_lon), max(foot_lon),
                                   min(foot_lat), max(foot_lat))
          crs(footprint_raster) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
   
          emissions_city_crop<-crop(emissions_city_i, extent(footprint_raster))
          
          # print(emissions_city)
          # emissions_city_crop[is.na(emissions_city_crop)]<-0
           # resample emissions to footprint raster
          emissions_city_resampled <- resample(emissions_city_crop, footprint_raster, method = "bilinear")

          emissions_city_mat <- as.matrix(emissions_city_resampled)
          footprint_mat <- as.matrix(footprint_raster)
          emissions_city_mat[is.na(emissions_city_mat)]<-0
          print(sum(footprint_mat))
          print(sum(emissions_city_mat,na.rm = TRUE))
            # print(dim(footprint_mat))
            # print(dim(emissions_city_mat))
          vulcan_conv <- sum(footprint_mat * emissions_city_mat, na.rm = TRUE)
          print(paste0("vulcan conv:: ", vulcan_conv))
          
          if (shp_file){
              fname="/vulcan_conv_shp.txt"
            }else{
              fname="/vulcan_conv.txt"
            }
      
            print(paste0(foot_byid,fname))
        
          write.table(vulcan_conv, file=paste0(foot_byid,fname), quote=F, row.names=F)

          nc_close(footprint_file)
    
      
        }
      gc()
      } 
    }
  }  # emissions_city_raster <- raster(emissions_city, xmn = min(foot_lon), xmx = max(foot_lon), ymn = min(foot_lat), 
          #     ymx = max(foot_lat), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
}

# cc_city_convolution<-Vectorize(cc_city_convolution)


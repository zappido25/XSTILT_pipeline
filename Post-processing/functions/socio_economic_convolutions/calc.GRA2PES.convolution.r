# fixed conversion issue 10/17/2022


calc.grapes.convolution<- function(foot_ff, foot_byid_ff, nhrs_ff, city,shp_file){
  library(ncdf4); library(raster); library(lubridate)
  cat("Gra2pes-city convolution function\n")
  sector_names_backup = c("AG","AVIATION", "COMM", "COOKING",
    "EGU", "FUG", "INDF", "INDP", "INTERNATIONAL", "OFFROAD", "OG", "ONROAD_DSL",
    "ONROAD_GAS", "RAIL", "RES", "SHIPPING", "VCP", "WASTE", "total")
  
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
    fname="/GRA2PES_conv.txt"
  } else {
      print("using us census")
      urban_extent <- get_urban_extent(city)
      fname="/GRA2PES_conv_shp.txt"
  }
      ## get Vulcan files
  emiss.path=file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GRA2PES/slices_overpass/")

  emission_conversion_grapes<-function(emissions){
    # units conversions for emissions (starts in mol km-2 h-1)
    emissions = emissions/ 3600# convert to mol km-2 s-1
    emissions =emissions *(1e6/1e6) # converts mol to umol and km2 to m2 -> umol m-2 s-1
    return(emissions)
  }
  
  month<-substr(basename(foot_ff[1]), 5,6)
  year<-substr(basename(foot_ff[1]), 1,4)
  timestr=substr(basename(foot_ff[1]), 1,10)
  print(paste("Processing:", timestr))
  print(paste("year:", year))

  #if timestr is between 2021 and 2023
  if(as.numeric(year) > 2020 && as.numeric(year)< 2024) {
    
    file_pattern <- paste0("GRAPES_", city, "_", timestr, "\\.nc$")

    emiss_file <- list.files(
      path = emiss.path,
      pattern = file_pattern,
      recursive = TRUE,
      full.names = TRUE
    )

    print(paste0("processing emission file: ",emiss_file))
    # emissions<-raster::brick(emiss_file)
    nc <- nc_open(emiss_file)
    # print(names(nc$var))
    # Sector names
    sector_names <- strsplit(ncatt_get(nc, 0, "sector_names")$value, " ")[[1]]
    print(sector_names)
    # print(sector_names)
    nlat <- nc$dim$lat$len
    nlon <- nc$dim$lon$len
    nsect <- length(nc$dim$sector_index$vals)

    # Get lat/lon coordinate arrays
    grapes_lat <- ncvar_get(nc, "lat")
    grapes_lon <- ncvar_get(nc, "lon")
  
  # extract ffco2 from the nc file
    ffco2_val <- ncvar_get(
      nc,
      varid = "CO2",
      start = c(1, 1, 1, 1),
      count = c(nlat, nlon, 1, nsect)
    )

    for (i in seq_along(foot_ff)) {
      
      foot=foot_ff[i]
      foot_byid=foot_byid_ff[i]
      nhrs=nhrs_ff[i]
      print(paste0("nhrs: ",nhrs))
  
      sector_conv<-NULL
 
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

            for (ii in seq_along(sector_names)){
              sector_index <- match(sector_names[ii], sector_names)
            
              print(paste0("Loaded emissions for sector: ", sector_names[ii], " ",sector_names[sector_index], " ", sector_index))
              co2_sect <- ffco2_val[,,sector_index]        # slice lat×lon for that sector
              
              # Convert mol km^-2 h^-1  →  µmol m^-2 s^-1
              co2_sector_umol <- emission_conversion_grapes(co2_sect)
              #
              print(dim(co2_sector_umol))
              # co2_sector_umol: matrix [lat, lon]

              r_sector <- raster(
                t(co2_sector_umol),         # transpose to lon × lat
                xmn = min(grapes_lon), xmx = max(grapes_lon),
                ymn = min(grapes_lat), ymx = max(grapes_lat),
                crs = CRS("+proj=longlat +datum=WGS84 +no_defs")
              )
              
              emissions_city_crop<-crop(r_sector, extent(footprint_raster))
              emissions_city_resampled <- resample(emissions_city_crop, footprint_raster, method = "bilinear")

              
              emissions_city_mat <- as.matrix(emissions_city_resampled)
              footprint_mat <- as.matrix(footprint_raster)  
              convolution <- sum(footprint_mat * emissions_city_mat, na.rm = TRUE)
              sector_conv<-rbind(sector_conv, c(sector_names[ii], convolution))
             }  
            sector_conv<-data.frame( sect = sector_conv[,1], ppm = sector_conv[,2])
            print("sector conv")
            print(sector_conv)  

            write.table(sector_conv, file=paste0(foot_byid,fname), quote=F, row.names=F)
            nc_close(footprint_file)
          }
        gc()
        } 
      }
    }
  }else{
    print(paste0("not between 2021 and 2023: ", year))
    
    for (i in seq_along(foot_ff)) {
      sector_conv=NULL
      foot=foot_ff[i]
      foot_byid=foot_byid_ff[i]
      nhrs=nhrs_ff[i]
      for (ii in seq_along(sector_names_backup)){
          sector_conv<-rbind(sector_conv, c(sector_names_backup[ii],  value = NA_real_))
      }
      sector_conv<-data.frame( sect = sector_conv[,1], ppm = sector_conv[,2])
      # print("sector conv")
      # print(sector_conv)
      print(foot_byid)
      print(i)
      write.table(sector_conv, file=paste0(foot_byid,fname), quote=F, row.names=F)
      
    }
  }
}

# cc_city_convolution<-Vectorize(cc_city_convolution)


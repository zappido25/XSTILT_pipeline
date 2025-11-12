library(dplyr)
library(terra)
library(raster)
# function to calculate receptor level effective population density 
# requires population density dataset and X-STILT footprint at the appropriate resolution
# could add GDP dataset or other data we are interested in here

# altered to run in parallel and subscribe to nhrs calcs
footprint_pop_density_nhrs<-function(foot, foot_byid, nhrs, city,shp_file){
  library(ncdf4); library(raster)
  
  # deal with footprints that do not exist
  for (i in seq_along(foot)){
    foot_i=foot[i]
    foot_byid_i=foot_byid[i]
    nhrs_i=nhrs[i]
   
    print(paste0("footprint: ", foot_i, " i: ", i))
  # check if file exist
    foot_exist<-file.exists(foot_i)

    # select only timesteps for 2:nhrs. 1 is thereceptor timestamp
    foot_timestr<-substr(basename(foot_i), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_timestrs<-seq(from = foot_timestr - (nhrs_i)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
  
    if(foot_exist == T){
      # think the paste0() in here will solve broken symlinks for moved files
      footprint_file<-try(nc_open(foot_i), silent =T)

      if(length(footprint_file) > 1){
        # save relevant foot information - lat/lon needed for referencing to GPW grid
        lons<-ncvar_get(footprint_file, varid="lon")
        lats<-ncvar_get(footprint_file, varid="lat")
        footprint<-ncvar_get(footprint_file, varid="foot")
        
        foot_hours<-ncvar_get(footprint_file, varid="time")
        foot_hours<-as.POSIXct(foot_hours, tz="UTC", origin = "1970-01-01 00:00:00 UTC")
        foot_hours<-foot_hours - as.numeric(substr(foot_hours, 18,19))
        
        new_flag<-foot_hours %in% foot_timestrs
        if(TRUE %in% new_flag){
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          # subset based on nhrs
          # but reverse time means we need the bottom layers
          
          footprint<-footprint[,,new_flag]
          footprint<-apply(footprint, c(1,2), sum)
          
          # raster footrpint cols and rows
          nx <- length(lons)
          ny <- length(lats)

          dx <- mean(diff(lons))
          dy <- mean(diff(lats))

          # Extent from cell centers
          xmn <- min(lons) - dx/2; xmx <- max(lons) + dx/2
          ymn <- min(lats) - dy/2; ymx <- max(lats) + dy/2

          # Create RasterLayer
          fp_r <- raster(nrows = ny, ncols = nx, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
                        crs = CRS("+proj=longlat +datum=WGS84"))

          # Map matrix to raster (flip so rows go north→south)
          # fp_mat <- t(foot)[ny:1, ]
          # values(fp_r) <- as.vector(fp_mat)
          # normalized foot for areal weighting
          #footprint<-footprint/sum(footprint)   # turned off 11/14/2022
          
          # read in GPW data
          pop_dat<-"/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
      
          gpw<-brick(pop_dat)
          gpw<-subset(gpw, 1) # don't want layers even though it only has 1 layer
          # subset raster to match footprint
          
          # gpw <- crop(gpw, extent(min(lons)-0.002,max(lons)+0.002,min(lats)-0.002,max(lats)+0.002))
          # gpw <- resample(gpw, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
          gpw <- crop(gpw, extent(fp_r))
          gpw <- resample(gpw, fp_r, method = "bilinear")
          gpw<-as.matrix(gpw)
          gpw[is.na(gpw)]<-0
          
          # annoying matrix turning issue
          gpw<-t(gpw)[,dim(gpw)[1]:1]

          # print(dim(fp_r))
          # print(dim(gpw))
          
          # calc pps P_conv
          pps<-sum(footprint*gpw)
          
          foot_sum<-sum(footprint)
          
          # total population  count
          tot_dat<-"/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_count_rev11_2020_30_sec.tif"
          tot<-brick(tot_dat)
          tot<-subset(tot, 1) # don't want layers even though it only has 1 layer
          
          # tot <- crop(tot, extent(min(lons)-0.002,max(lons)+0.002,min(lats)-0.002,max(lats)+0.002))
          # tot <- resample(tot, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
          tot <- crop(tot, extent(fp_r))
          tot <- resample(tot, fp_r, method = "bilinear")
          tot<-as.matrix(tot)
          tot[is.na(tot)]<-0
         
          # annoying matrix turning issue
          tot<-t(tot)[,dim(tot)[1]:1]
          
          footprint[footprint > 0 ]<-1
          total_pop<-sum(tot*footprint) # get total populattttion in footprint area
          
          if (shp_file){
            fname="/eff_pop_density_shp.txt"
          }else{
            fname="/eff_pop_density.txt"
          }
          # write output
          write.table(c(pps, foot_sum, total_pop), file=paste0(foot_byid_i,fname), quote=F, row.names=F)
          
          nc_close(footprint_file)
          
        }
      }
    }
  gc()
  }
}

# PPS_FUNC<-Vectorize(PPS_func)
# footprint_pop_density_nhrs<-Vectorize(footprint_pop_density_nhrs)

footprint_pop_density_nhrs_yearly<-function(foot, foot_byid, nhrs, city,shp_file){
  library(ncdf4); library(raster)
  
  # deal with footprints that do not exist
  for (i in 1:length(foot)){
    foot=foot[i]
    foot_byid=foot_byid[i]
    nhrs=nhrs[i]
    # if(nhrs=="-Inf") {
    #   nhrs=24
    # } 
    print(paste0("footprint: ", foot, " i: ", i))
  # check if file exist
    foot_exist<-file.exists(foot)
    
    # select only timesteps for 2:nhrs. 1 is thereceptor timestamp
    foot_timestr<-substr(basename(foot), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_year <- format(foot_timestr, "%Y")
    foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
    path="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
                
    if (foot_year < 2021) {
      pop_dat<-paste0(path,"gpw_v4_population_density_rev11_2020_30_sec.tif")
    } else if (foot_year >= 2021) {
      # foot_year <- 2020
      pop_dat<-paste0(path,"usa_pop_",foot_year,"_CN_1km_R2025A_UA_v1.tif")
    }
    gpw<-brick(pop_dat)
    gpw<-subset(gpw, 1) # don't want layers even though it only has 1 layer
    cat("foot_year:", foot_year, "\n")
    if(foot_exist == T){
      # think the paste0() in here will solve broken symlinks for moved files
      footprint_file<-try(nc_open(foot), silent =T)
      
      if(length(footprint_file) > 1){
        # save relevant foot information - lat/lon needed for referencing to GPW grid
        lons<-ncvar_get(footprint_file, varid="lon")
        lats<-ncvar_get(footprint_file, varid="lat")
        footprint<-ncvar_get(footprint_file, varid="foot")
        
        foot_hours<-ncvar_get(footprint_file, varid="time")
        foot_hours<-as.POSIXct(foot_hours, tz="UTC", origin = "1970-01-01 00:00:00 UTC")
        foot_hours<-foot_hours - as.numeric(substr(foot_hours, 18,19))
        
        new_flag<-foot_hours %in% foot_timestrs
        if(TRUE %in% new_flag){
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          # subset based on nhrs
          # but reverse time means we need the bottom layers
          
          footprint<-footprint[,,new_flag]
          footprint<-apply(footprint, c(1,2), sum)
          
          # raster footrpint cols and rows
          nx <- length(lons)
          ny <- length(lats)

          dx <- mean(diff(lons))
          dy <- mean(diff(lats))

          # Extent from cell centers
          xmn <- min(lons) - dx/2; xmx <- max(lons) + dx/2
          ymn <- min(lats) - dy/2; ymx <- max(lats) + dy/2

          # Create RasterLayer
          fp_r <- raster(nrows = ny, ncols = nx, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
                        crs = CRS("+proj=longlat +datum=WGS84"))

          # Map matrix to raster (flip so rows go north→south)
          # fp_mat <- t(foot)[ny:1, ]
          # values(fp_r) <- as.vector(fp_mat)
          # normalized foot for areal weighting
          #footprint<-footprint/sum(footprint)   # turned off 11/14/2022
          
    
          gpw <- crop(gpw, extent(fp_r))
          gpw <- resample(gpw, fp_r, method = "bilinear")
          gpw<-as.matrix(gpw)
          gpw[is.na(gpw)]<-0
          
          # annoying matrix turning issue
          gpw<-t(gpw)[,dim(gpw)[1]:1]

          # print(dim(fp_r))
          # print(dim(gpw))
          
          # calc pps P_conv
          pps<-sum(footprint*gpw)
          
          foot_sum<-sum(footprint)
          
          # total population  count
          tot_dat<-"/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_count_rev11_2020_30_sec.tif"
          tot<-brick(tot_dat)
          tot<-subset(tot, 1) # don't want layers even though it only has 1 layer
          
          # tot <- crop(tot, extent(min(lons)-0.002,max(lons)+0.002,min(lats)-0.002,max(lats)+0.002))
          # tot <- resample(tot, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
          tot <- crop(tot, extent(fp_r))
          tot <- resample(tot, fp_r, method = "bilinear")
          tot<-as.matrix(tot)
          tot[is.na(tot)]<-0
         
          # annoying matrix turning issue
          tot<-t(tot)[,dim(tot)[1]:1]
          
          footprint[footprint > 0 ]<-1
          total_pop<-sum(tot*footprint) # get total populattttion in footprint area
          
          if(foot_year < 2021) {
            if (shp_file){
              fname="/eff_pop_density_shp.txt"
            }else{
              fname="/eff_pop_density.txt"
            }
          } else if (foot_year >= 2021) {
            if (shp_file){
              fname="/eff_pop_density_shp_yearly.txt"
            }else{
              fname="/eff_pop_density_yearly.txt"
            }
          }
          
          # write output
          write.table(c(pps, foot_sum, total_pop), file=paste0(foot_byid,fname), quote=F, row.names=F)
          
          nc_close(footprint_file)
          
        }
      }
    }
  gc()
  }
}

# PPS_FUNC<-Vectorize(PPS_func)
footprint_pop_density_nhrs_yearly<-Vectorize(footprint_pop_density_nhrs_yearly)
# function to calculate consumption based emissions per capita for overpass using normalized footprint
# uses 2013 based consumption data at 250m resolution from Moran et al. 2018 (weird, in 1km res despite saying in 250m)

#cf<-brick("/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/consumption/GGMCFPerCap_v1.0.tif")
#cf<-projectRaster(cf, crs = "+proj=longlat +datum=WGS84 +no_defs", res = 1/120, extent = extent(-180, 180, -72,72))

#writeRaster(cf, file = "CF.nc")

consump_func<-function(foot){
  library(raster); library(ncdf4)
  
  # read in footprint
  footprint<-nc_open(foot)
  # save relevant foot information - lat/lon needed for referencing to GPW grid
  lons<-ncvar_get(footprint, varid="lon")
  lats<-ncvar_get(footprint, varid="lat")
  footprint<-ncvar_get(footprint, varid="foot")
  
  # sum it across the third dimension of time
  if (length(dim(footprint)) == 3) {
    footprint <- apply(footprint, c(1, 2), sum, na.rm = TRUE)  # Sum across the third dimension
  }
  # normalized foot for areal weighting
  footprint<-footprint/sum(footprint)
  # hopefully the resampling went well here
  # read in carbon footprint information

  base_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/Consumption/"
  consumption_file=file.path(base_dir, "CF.nc")
  
  cf<-brick(consumption_file)
  
  cf<-crop(cf, extent(min(lons),max(lons),min(lats),max(lats)))
  cf <- resample(cf, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
  
  # fix regridding issue - introduces some error
  # extent(cf) <- extent(footprint)
  
  # convert from Gg to t
  cf<-cf*1000
  
  # convolve
  # should be the effective carbon footprint/capita of the receptor in tCO2/yr
  # foot<-as.matrix(foot)
  cf<-as.matrix(cf)
  
  conv<-sum(footprint*cf)
  
  return(conv)
}


# function to calculate consumption based emissions per capita for overpass using normalized footprint
# uses 2013 based consumption data at 250m resolution from Moran et al. 2018 (weird, in 1km res despite saying in 250m)

#cf<-brick("/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/consumption/GGMCFPerCap_v1.0.tif")
#cf<-projectRaster(cf, crs = "+proj=longlat +datum=WGS84 +no_defs", res = 1/120, extent = extent(-180, 180, -72,72))

#writeRaster(cf, file = "CF.nc")

eff_consump_func_nhrs<-function(foot, foot_byid, nhrs, city,shp_file){
  library(raster); library(ncdf4)

    for (i in 1:length(foot)){
    this_foot <- foot[i]
    this_foot_byid <- foot_byid[i]
    this_nhrs <- nhrs[i]
    # if(this_nhrs=="-Inf") {
    #   this_nhrs=24
    # } 
    print(paste0("footprint: ", this_foot, " i: ", i))
  # check if file exist
    foot_exist<-file.exists(foot)
    
    # select only timesteps for 2:nhrs. 1 is thereceptor timestamp
    foot_timestr<-substr(basename(foot), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
    
    if(foot_exist == T){
      
    # read in footprint
        foot<-try(brick(foot),silent = T) # broken foots issues
        
        if(is.null(dim(foot)) == FALSE){
          foot_hours<-names(foot)
          foot_hours<-gsub("X","", foot_hours)
          foot_hours<-substr(foot_hours, 1,16)
          foot_hours<-as.POSIXct(foot_hours, format = "%Y.%m.%d.%H.%M", tz = "UTC")
          
          new_flag<-foot_hours %in% foot_timestrs
        
        if(TRUE %in% new_flag){
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          foot<-subset(foot, new_flag) # this should deal with mssing hours in foots
        
        # sum across layers
          foot<-calc(foot, sum)
        
        # normalize the footprint
          foot<-foot/cellStats(foot, sum)
        
            # read in carbon footprint information
          cf<-brick("/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/consumption/CF.nc")

          x1<-extent(foot)[1]
          x2<-extent(foot)[2]

          #crop cf to the footprint
          cf<-crop(cf, extent(x1, x2, extent(foot)[3], extent(foot)[4]))
          
          # reasample cf to footprint using the bilinear interpolation method
          blank<-raster(nrow = dim(foot)[1], ncol = dim(foot)[2], xmn = extent(foot)[1], xmx = extent(foot)[2], ymn = extent(foot)[3], ymx = extent(foot)[4])
          cf <- resample(cf, blank, method = "bilinear")
              
          # convert from Gg to t
          cf<-cf*1000
          
          # convolve
          # should be the effective carbon footprint/capita of the receptor in tCO2/yr
          cf<-as.matrix(cf)
          foot<-as.matrix(foot)
                
              
          conv<-sum(foot*cf)
          if (shp_file){
            fname="/consumption_shp.txt"
          }else{
            fname="/consumption.txt"
          }
          # save output and remove garbage
          write.table(conv, file=paste0(foot_byid,fname), quote=F, row.names=F)
        }
        gc()
      }
    }
  }
}

eff_consump_func_nhrs=Vectorize(eff_consump_func_nhrs)

# convolution of Scope 3 consumption and footprint
consump_conv_func_nhrs<-function(foot, foot_byid, nhrs, city,shp_file){
  library(raster); library(ncdf4)
  
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
    foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
    
    if(foot_exist == T){
      
    # read in footprint
      footprint_file<-try(nc_open(foot), silent =T)
      # read in foot
      # foot<-try(brick(foot),silent = T) # broken foots issues

      if(is.null(dim(foot)) == FALSE){
          foot_hours<-names(foot)
          foot_hours<-gsub("X","", foot_hours)
          foot_hours<-substr(foot_hours, 1,16)
          foot_hours<-as.POSIXct(foot_hours, format = "%Y.%m.%d.%H.%M", tz = "UTC")
          
          new_flag<-foot_hours %in% foot_timestrs
        
        if(TRUE %in% new_flag){
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          foot<-subset(foot, new_flag) # this should deal with mssing hours in foots
        
        # sum across layers
          foot<-calc(foot, sum)
        
        # normalize the footprint
          # foot<-foot/cellStats(foot, sum)
        
            # read in carbon footprint information
          cf<-brick("/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/consumption/CF.nc")

          x1<-extent(foot)[1]
          x2<-extent(foot)[2]

          #crop cf to the footprint
          cf<-crop(cf, extent(x1, x2, extent(foot)[3], extent(foot)[4]))
          
          # reasample cf to footprint using the bilinear interpolation method
          blank<-raster(nrow = dim(foot)[1], ncol = dim(foot)[2], xmn = extent(foot)[1], xmx = extent(foot)[2], ymn = extent(foot)[3], ymx = extent(foot)[4])
          cf <- resample(cf, blank, method = "bilinear")
          
              
          # convert from Gg to t
          cf<-cf*1000
          
          # convolve
          # should be the effective carbon footprint/capita of the receptor in tCO2/yr
          cf<-as.matrix(cf)
          foot<-as.matrix(foot)
                
              
          conv<-sum(foot*cf)
          if (shp_file){
            fname="/consumption_conv_shp.txt"
          }else{
            fname="/consumption_conv.txt"
          }
          # save output and remove garbage
          # print(paste0("fname:",foot_byid,fname))
          write.table(conv, file=paste0(foot_byid,fname), quote=F, row.names=F)
        }
        gc()
      }
    }
  }
}

consump_conv_func_nhrs=Vectorize(consump_conv_func_nhrs)
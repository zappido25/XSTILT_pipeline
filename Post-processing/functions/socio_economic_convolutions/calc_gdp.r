# uses 2022 data for all simulations
# two functions here
# calc_gdp for generate_post_processing and calc_gdp_nhrs for convolution
#https://www.nature.com/articles/s41597-025-04487-x for citation
calc_gdp<-function(foot){
  library(raster); library(ncdf4)
    
  # load gdp data
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

  base_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GDP"
  gdp_file=file.path(base_dir, "rast_adm1_gdp_perCapita_1990_2022.tif") 

  gdp <- stack(gdp_file) 
  
   # get just 2022 data
  gdp <- subset(gdp, which(names(gdp) == "gdp_2022"))
 

  
  # subset gdp data
  gdp<-crop(gdp, extent(min(lons),max(lons),min(lats),max(lats)))
  gdp <- resample(gdp, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
  
  
  # adjust resolution of GDP data - values unchanged
  # convert raster resolution
  # blank<-raster(nrow = 2400, ncol = 2400, xmn = extent(gdp)[1], xmx = extent(gdp)[2], ymn = extent(gdp)[3], ymx = extent(gdp)[4])
  # gdp<-resample(gdp, blank, method = "ngb")
  
  
  # convolve
  # footprint<-as.matrix(footprint)
  gdp<-as.matrix(gdp)
  
  conv<-sum(footprint*gdp, na.rm=T)
  
  print(paste0("GDP: ", conv))
  return(conv)
}

## calc_gdp_nhrs for calc_gdp_nhrs
calc_eff_gdp_nhrs<-function(foot, foot_byid, nhrs, city,shp_file){
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
    # load gdp data
      base_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GDP"
      gdp_file=file.path(base_dir, "GDP_per_capita_PPP_1990_2015_v2.nc")
      
      gdp<-brick(gdp_file)

      #  get just 2015 data
      gdp<-subset(gdp, 26) 
      
      # read in foot
      foot<-try(brick(foot),silent = T) # broken foots issues
      
      if(is.null(dim(foot)) == FALSE){
          foot_hours<-names(foot)
          foot_hours<-gsub("X","", foot_hours)
          foot_hours<-substr(foot_hours, 1,16)
          foot_hours<-as.POSIXct(foot_hours, format = "%Y.%m.%d.%H.%M", tz = "UTC")
          
          # check if foot_hours is a a subset of foot_timestrs
          new_flag<-foot_hours %in% foot_timestrs

        if(TRUE %in% new_flag){ # only if foot_hours
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          foot<-subset(foot, new_flag) # this should deal with mssing hours in foots
          
          # sum across layers
          foot<-calc(foot, sum)
          
          # normalize the footprint
          foot<-foot/cellStats(foot, sum)

        # subset gdp data
        
        # Auckland fix
          x1<-extent(foot)[1]
          x2<-extent(foot)[2]

          
          gdp<-crop(gdp, extent(x1, x2, extent(foot)[3], extent(foot)[4]))
          # foot<-crop(foot, extent(gdp))
        

          blank<-raster(nrow = dim(foot)[1], ncol = dim(foot)[2], xmn = extent(foot)[1], xmx = extent(foot)[2], ymn = extent(foot)[3], ymx = extent(foot)[4])
          gdp<-resample(gdp, blank, method = "ngb")
          gdp<-as.matrix(gdp)

          #}
          
          # convolve
          foot<-as.matrix(foot)
          
          # calc convolution
          conv<-sum(foot*gdp, na.rm=T)
          
          if (shp_file){
            fname="/gdp_shp.txt"
          }else{
            fname="/gdp.txt"
          }
          # save output and remove garbage
          write.table(conv, file=paste0(foot_byid,fname), quote=F, row.names=F)
          # remove gdp and foot  
          gc()
        } #inner flag loop
      } #dim(foot) loop
      } #dim(foot) loop
    } # iteration loop
} # function end
calc_eff_gdp_nhrs<-Vectorize(calc_eff_gdp_nhrs)


# this convolves entire footprint with the GDP data. used for Egdp
calc_gdp_conv_nhrs<-function(foot, foot_byid, nhrs, city,shp_file){
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
    # load gdp data
      base_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GDP"
      gdp_file=file.path(base_dir, "GDP_per_capita_PPP_1990_2015_v2.nc")
      
      gdp<-brick(gdp_file)

      #  get just 2015 data
      gdp<-subset(gdp, 26) 
      footprint_file<-try(nc_open(foot), silent =T)
      # read in foot
      # foot<-try(brick(foot),silent = T) # broken foots issues
     
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
          # check if foot_hours is a a subset of foot_timestrs
          
        # subset gdp data
        
          gdp<-crop(gdp, extent(min(lons)-0.002,max(lons)+0.002,min(lats)-0.002,max(lats)+0.002))
          gdp <- resample(gdp, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
          gdp<-as.matrix(gdp)

          #}
          
        
          # calc convolution
          conv<-sum(footprint*gdp, na.rm=T)

          if (shp_file){
            fname="/GDP_conv_shp.txt"
          }else{
            fname="/GDP_conv.txt"
          }
          # save output and remove garbage
          
          write.table(conv, file=paste0(foot_byid,fname), quote=F, row.names=F)
          # remove gdp and foot  
          gc()
        } #inner flag loop
      } #dim(foot) loop
      } #dim(foot) loop
    } # iteration loop
} # function end
calc_gdp_conv_nhrs<-Vectorize(calc_gdp_conv_nhrs)
library(dplyr)
# function to calculate receptor level nighttime lights as a proxy for energy consumption

# altered to run in parallel and subscribe to nhrs calcs
eff_lights_func_nhrs<-function(foot_ff, foot_byid_ff, nhrs_ff, city,shp_file){
  library(ncdf4); library(raster)
  
  print(length(foot_ff))
  # deal with footprints that do not exist
  for (i in seq_along(foot_ff)){
    foot=foot_ff[i]
    foot_byid=foot_byid_ff[i]
    nhrs=nhrs_ff[i]
    city_name=city[i]
    # if(nhrs=="-Inf") {
    #   nhrs=24
    # } 
    print(paste0("footprint: ", foot, " i: ", i))
    print(paste0("nhrs: ", nhrs, " i: ", i))

  # check if file exist
    foot_exist<-file.exists(foot)
    
    # select only timesteps for 2:nhrs. 1 is thereceptor timestamp
    foot_timestr<-substr(basename(foot), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
    
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
          
          # normalized foot for areal weighting
          footprint<-footprint/sum(footprint)   
          
          # read in nighttime lights data
          
          year_t<-substr(foot_hours[1],1,4)
          # if(year_t == "2022"){year_t<-"2021"}
          # if(year_t == "2023"){year_t<-"2021"}
          # if(year_t == "2024"){year_t<-"2021"}

          # if (city_name == "SanDiego"){
          #   city_name<-"SanDiego_Tiujana"
          # }
          
          
          # lights_dat<-dir("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/BlackMarble/out", pattern = paste0(city_name,"-", year_t), full.names=T)
          lights_dat<-dir("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/BlackMarble/CONUS_output", pattern = paste0(city_name,"-", year_t), full.names=T)
          print(lights_dat)
          lights<-brick(lights_dat)
          lights<-subset(lights, 1) # don't want layers even though it only has 1 layer
          # subset raster to match footprint
          
          
          lights <- crop(lights, extent(min(lons)-0.002,max(lons)+0.002,min(lats)-0.002,max(lats)+0.002))

          lights <- resample(lights, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
          
          lights<-as.matrix(lights)
          lights[is.na(lights)]<-0
        
          # annoying matrix turning issue
          lights<-t(lights)[,dim(lights)[1]:1]
          
          # calc pps
          rad<-sum(footprint*lights) # plots line up as expected - raster package still weird
          
          if (shp_file){
            fname="/lights_shp.txt"
          }else{
            fname="/lights.txt"
          }
          # write output
          write.table(rad, file=paste0(foot_byid_ff,fname), quote=F, row.names=F)
          
          nc_close(footprint_file)
          
        }
      }
    }
  }
    gc()
}

eff_lights_func_nhrs<-Vectorize(eff_lights_func_nhrs)
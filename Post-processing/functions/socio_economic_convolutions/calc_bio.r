library(dplyr)
# function to convolve footprints with SMUrF to get bio contibutions
# for the 2014-2018 period and extrapolated to the 2019-2022 period
# this is a bit of a hack, but it works for now.
# try https://earth.gov/ghgcenter/data-catalog/micasa-carbonflux-grid-v1 next
# but the data is coarse resolution
bio_func<-function(foot,city){
  library(ncdf4);library(chron);library(abind)
  
  
  if(is.na(foot)){return(NA)}
  # convolve with each year (2014-2018) - take SD and mean
  # also keep the value for the appropriate year if possible
  # cropping will be critical
  # Melbourne and Auckland need special care
  
  # TO DO:
    # Melbourne, Auckland cropping/merging
    # address small SMUrF domains for areas with AGB issues
  
  # get city files
  # city<-basename(dirname(dirname(dirname(dirname(foot)))))
  # print(city)
  SMUrF_home<-dir("/uufs/chpc.utah.edu/common/home/lin-group16/KaiW/SMUrF", full.names=T)
  SMUrF_home<-SMUrF_home[grepl(city, SMUrF_home)==TRUE]
  
  # get footprint ready
  footprint<-nc_open(foot)
  lons<-ncvar_get(footprint, varid="lon")
  lats<-ncvar_get(footprint, varid="lat")
  times<-ncvar_get(footprint, varid="time")
  
  times<-times/86400
  times<-chron(times)
  footprint<-ncvar_get(footprint, varid="foot")
  
  # sum it across the third dimension of time
  if (length(dim(footprint)) == 3) {
    footprint <- apply(footprint, c(1, 2), sum, na.rm = TRUE)  # Sum across the third dimension
  }
  
  times_save<-times
  
  # putting below in a loop for each year
  years<-c("14","15","16","17","18")
  bios<-c()
  for(y in years){
  times<-chron(dates.=c(paste0(substr(times_save,2,7),y)), times.=c(substr(times_save, 11,18)))
  # read in appropriate monthly SMUrF file
  # will change reionally which is tough
  timestr<-unique(paste0("20",y, substr(times, 2,3))) # trying to account for overpasses at start/end of month
  
  print(timestr)
  if(length(timestr) == 1){
  SMUrF<-SMUrF_home[grepl(timestr, SMUrF_home) == TRUE]
  }
  if(length(timestr) == 2){
    SMUrF1<-SMUrF_home[grepl(timestr[1], SMUrF_home) == TRUE]
    SMUrF2<-SMUrF_home[grepl(timestr[2], SMUrF_home) == TRUE]
    SMUrF<-c(SMUrF1, SMUrF2)
  }
   
  smurf_obj<-NULL
  smurf_obj_time<-c()
  for(sm in SMUrF){
    SMUrF_sm<-nc_open(sm)
    S_time_sm<-ncvar_get(SMUrF_sm, varid="time")
    smurf_sm<-ncvar_get(SMUrF_sm, varid="NEE_mean")
    
    smurf_obj<-abind(smurf_obj, smurf_sm, along=3)
    smurf_obj_time<-c(smurf_obj_time, S_time_sm)
  }
  
  S_lon<-ncvar_get(SMUrF_sm, varid="lon")
  S_lat<-ncvar_get(SMUrF_sm, varid="lat")
  
  S_time<-smurf_obj_time
  S_time<-chron(S_time/86400)
  smurf<-smurf_obj
  
  # this needs to go both ways for a few sites because of AGB issues
  # print(dim(smurf))  
  # subset SMUrF to match footprint, change digits to 1, which is a loose criteria
  # this is a bit of a hack, but it works for now
  valid_lon <- round(S_lon, digits=1) %in% round(lons, digits=1)
  valid_lat <- round(S_lat, digits=1) %in% round(lats, digits=1)
  
  if (any(valid_lon) && any(valid_lat)) {
    smurf <- smurf[valid_lon, valid_lat, ]
  } else {
    stop("Error: No matching longitude or latitude indices found for subsetting.")
  }
  
  # print(dim(smurf))  
  
  # subset the other way to fix places like Cape Town
  # only do for first iteration of loop
  # if(y == "14"){
  # footprint<-footprint[round(lons, digits=1) %in% round(S_lon, digits=1), round(lats, digits=1) %in% round(S_lat, digits=1),]
  # }
  
  # get times surrounding footprint_times
  S_time_min<-S_time[abs(S_time - min(times)) < 1/24 & S_time < min(times)]
  S_time_max<-S_time[abs(S_time - max(times)) < 1/24 & S_time > max(times)]
  
  smurf<-smurf[,, S_time <= S_time_max & S_time >= S_time_min]
  S_time<-S_time[S_time <= S_time_max & S_time >= S_time_min]
  
  # create interpolated SMUrF values to match footprint times
  smurf_interp<-NULL
  for(i in 1:length(times)){
    sub_smurf<-smurf[,,abs(S_time - times[i]) < 1/24]
    sub_smurf[is.na(sub_smurf)]<-0 # for interpolation purposes
    
    slope_matrix<-(sub_smurf[,,2]-sub_smurf[,,1])/3600 # s-1
    
    position<-as.numeric((times[i]-S_time[S_time < times[i] & abs(S_time - times[i]) < 1/24]))*86400
    
    smurf_interp_i<-sub_smurf[,,1]+(slope_matrix*position)
    smurf_interp<-abind(smurf_interp, smurf_interp_i, along=3)
  }
  
  # print(dim(smurf_interp))
  ## resample to footprint grid, by converting the arry to a brick and resampling
  smurf_interp_raster <- brick(smurf_interp, xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +datum=WGS84"))
  smurf_interp_resample <- resample(smurf_interp_raster, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")

  # print(dim(smurf_interp_resample))
  # print(dim(footprint))
  # bio<-sum(smurf_interp*footprint)

  
  bio <- sum(smurf_interp_resample[[1]][] * footprint, na.rm = TRUE)
  bios<-c(bios,bio)
  }
  
  sd<-sd(bios)
  avg<-mean(bios)
  print(sd)
  print(avg)
  keep<-bios[years == substr(times_save[1],8,9)]
  if(length(keep) == 0){keep<-NA} # 2019-2022 overpasses
  
  bios<-c(avg,sd,keep)
  return(bios)

}

# convilution of SMUrf bio and footprint for 1:nhrs
bio_func_nhrs<-function(foot_fns, foot_byid_fns, nhrs_all, city,shp_file){
  library(ncdf4);library(chron);library(abind)

   for (i in 1:length(foot_fns)){
    foot=foot_fns[i]
    foot_byid=foot_byid_fns[i]
    nhrs=nhrs_all[i]
    city_name=city[i]
    # if(nhrs=="-Inf") {
    #   nhrs=24
    # } 
    # deal with footprints that do not exist
    foot_exist<-file.exists(foot)
    
    foot_timestr<-substr(basename(foot), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')

    
    if(foot_exist == T){
       foot_brick<-try(brick(foot),silent = T) # broken foots issues
        
        if(is.null(dim(foot_brick)) == FALSE){
          foot_hours<-names(foot_brick)
          foot_hours<-gsub("X","", foot_hours)
          foot_hours<-substr(foot_hours, 1,16)
          foot_hours<-as.POSIXct(foot_hours, format = "%Y.%m.%d.%H.%M", tz = "UTC")
          
          new_flag<-foot_hours %in% foot_timestrs
        if(TRUE %in% new_flag){
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          foot_brick<-subset(foot_brick, new_flag) # this should deal with mssing hours in foots
          foot_sum <- (calc(foot_brick, sum))

      
      if (city_name=="Miami"){
        city_name="East_FL"
      }
      # if (city_name=="Washington"){
      #   city_name="WashingtonDC"
      # }
      SMUrF_home<-dir("/uufs/chpc.utah.edu/common/home/lin-group16/KaiW/SMUrF", full.names=T)
      SMUrF_home<-SMUrF_home[grepl(city_name, SMUrF_home)==TRUE]
      
    # get footprint ready
      footprint_file<-try(nc_open(foot), silent =T)
    
      if(length(footprint_file) > 1){
        lons<-ncvar_get(footprint_file, varid="lon")
        lats<-ncvar_get(footprint_file, varid="lat")
        times<-ncvar_get(footprint_file, varid="time")
        
        times<-times/86400
        times<-chron(times)
        # foot_time_chron <- chron(format(as.POSIXct(foot_timestrs, tz="UTC"), "%m/%d/%y %H:%M:%S"))
        footprint<-ncvar_get(footprint_file, varid="foot")
        # Reverse the times array
        times_rev <- rev(times)
        

        # Select from nhrs down to 1 (i.e., the first nhrs elements of the reversed array)
        times <- times_rev[nhrs:1]
       
        if (length(dim(footprint)) == 3) {
          footprint <- apply(footprint, c(1, 2), sum, na.rm = TRUE)  # Sum across the third dimension
          # foot_sum <- apply(foot_brick, c(1, 2), sum, na.rm = TRUE)  # Sum across the third dimension
        }
  
        times_save<-times
    
        # putting below in a loop for each year
        # putting below in a loop for each year
        years<-c("14","15","16","17","18")
        bios_nhrs<-c()
        bios_24h<-c()
        for(y in years){
            times<-chron(dates.=c(paste0(substr(times_save,2,7),y)), times.=c(substr(times_save, 11,18)))
            # read in appropriate monthly SMUrF file
            # will change reionally which is tough
            timestr<-unique(paste0("20",y, substr(times, 2,3))) # trying to account for overpasses at start/end of month
            
            print(timestr)
            if(length(timestr) == 1){
              SMUrF<-SMUrF_home[grepl(timestr, SMUrF_home) == TRUE]
            }
            if(length(timestr) == 2){
              SMUrF1<-SMUrF_home[grepl(timestr[1], SMUrF_home) == TRUE]
              SMUrF2<-SMUrF_home[grepl(timestr[2], SMUrF_home) == TRUE]
              SMUrF<-c(SMUrF1, SMUrF2)
            }
            smurf_obj<-NULL
            smurf_obj_time<-c()
            for(sm in SMUrF){
              SMUrF_sm_file<-try(nc_open(sm), silent =T)
              if(length(SMUrF_sm_file) ==1){next()}
                S_time_sm<-ncvar_get(SMUrF_sm_file, varid="time")
                smurf_sm<-try(ncvar_get(SMUrF_sm_file, varid="NEE_mean"), silent=T)
              if(length(smurf_sm) == 1){next()}
              
              smurf_obj<-abind(smurf_obj, smurf_sm, along=3)
              smurf_obj_time<-c(smurf_obj_time, S_time_sm)
            }
            if(is.null(smurf_obj)){next()}
                    
            S_lon<-ncvar_get(SMUrF_sm_file, varid="lon")
            S_lat<-ncvar_get(SMUrF_sm_file, varid="lat")
            
            S_time<-smurf_obj_time
            S_time<-chron(S_time/86400)
            smurf<-smurf_obj
 
          # this needs to go both ways for a few sites because of AGB issues
          # print(dim(smurf))  
          # subset SMUrF to match footprint, change digits to 1, which is a loose criteria
          # this is a bit of a hack, but it works for now
          valid_lon <- round(S_lon, digits=1) %in% round(lons, digits=1)
          valid_lat <- round(S_lat, digits=1) %in% round(lats, digits=1)
        
        if (any(valid_lon) && any(valid_lat)) {
          smurf <- smurf[valid_lon, valid_lat, ]
        } else {
          stop("Error: No matching longitude or latitude indices found for subsetting.")
        }
  
        # print(dim(smurf))  
        
        # subset the other way to fix places like Cape Town
        # only do for first iteration of loop
        # if(y == "14"){
        # footprint<-footprint[round(lons, digits=1) %in% round(S_lon, digits=1), round(lats, digits=1) %in% round(S_lat, digits=1),]
        # }
  
          # get times surrounding footprint_times
        S_time_min<-S_time[abs(S_time - min(times)) < 1/24 & S_time < min(times)]
        S_time_max<-S_time[abs(S_time - max(times)) < 1/24 & S_time > max(times)]
        
        smurf<-smurf[,, S_time <= S_time_max & S_time >= S_time_min]
        S_time<-S_time[S_time <= S_time_max & S_time >= S_time_min]
        
          # create interpolated SMUrF values to match footprint times
          smurf_interp<-NULL
        for(i in 1:length(times)){
          sub_smurf<-smurf[,,abs(S_time - times[i]) < 1/24]
          sub_smurf[is.na(sub_smurf)]<-0 # for interpolation purposes
          
          slope_matrix<-(sub_smurf[,,2]-sub_smurf[,,1])/3600 # s-1
          
          position<-as.numeric((times[i]-S_time[S_time < times[i] & abs(S_time - times[i]) < 1/24]))*86400
          
          smurf_interp_i<-sub_smurf[,,1]+(slope_matrix*position)
          smurf_interp<-abind(smurf_interp, smurf_interp_i, along=3)
        }
  
        # print(dim(smurf_interp))
        ## resample to footprint grid, by converting the arry to a brick and resampling
        smurf_interp_raster <- brick(smurf_interp, xmn=min(lons), xmx=max(lons), ymn=min(lats), ymx=max(lats), crs=CRS("+proj=longlat +datum=WGS84"))
        smurf_interp_resample <- resample(smurf_interp_raster, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")

 
  # bio<-sum(smurf_interp*footprint)


        bio_24h <- sum(smurf_interp_resample[[1]][] * footprint, na.rm = TRUE) 
        bio_nhrs <- sum(smurf_interp_resample[[1]][] * foot_sum[[1]][], na.rm = TRUE)
        # bio<-sum(smurf_interp*footprint)
  
        # bio<-sum(smurf_interp*foot_sum)
        bios_24h<-c(bios_24h,bio_24h)
        bios_nhrs<-c(bios_nhrs,bio_nhrs)
        # print(paste0("bio_24h:",bio_24h))
        # print(paste0("bio_nhrs:",bio_nhrs))
        
        nc_close(SMUrF_sm_file)
        } # end of year loop
        print(paste0("bio_24h: ", paste(bios_24h, collapse = ", ")))
        print(paste0("bio_nhrs: ", paste(bios_nhrs, collapse = ", ")))
       
        
        sd_nhrs<-sd(bios_nhrs)
        avg_nhrs<-mean(bios_nhrs)
        keep_nhrs<-bios_nhrs[years == substr(times_save[1],8,9)]
        
        sd_24h<-sd(bios_24h)
        avg_24h<-mean(bios_24h)
        keep_24h<-bios_24h[years == substr(times_save[1],8,9)]
        
        if(length(keep_nhrs) == 0){keep_nhrs<-NA} # 2019-2022 overpasses
        if(length(keep_24h) == 0){keep_24h<-NA} # 2019-2022 overpasses
        
        bios_nhrs<-c(avg_nhrs,sd_nhrs,keep_nhrs)
        bios_24h<-c(avg_24h,sd_24h,keep_24h)

        if (shp_file){
          fname1="/bio_nhrs_shp.txt"
          fname2="/bio_24h_shp.txt"
        }else{
          fname1="/bio_nhrs.txt"
          fname2="/bio_24h.txt"
        }
        write.table(bios_nhrs, file=paste0(foot_byid,fname1), quote=F, row.names=F)
        write.table(bios_24h, file=paste0(foot_byid,fname2), quote=F, row.names=F)
        
        nc_close(footprint_file)

      } # length of footprint >1  loop
    } # footprint file exists loop
    gc()
  }
}
}
} # end of function
# bio_func_nhrs<-function(foot, foot_byid, nhrs, city){
#   library(ncdf4);library(chron);library(abind)

#    for (i in 1:length(foot)){
#     foot=foot[i]
#     foot_byid=foot_byid[i]
#     nhrs=nhrs[i]
#     city=city[i]
#     # deal with footprints that do not exist
#     foot_exist<-file.exists(foot)
    
#     foot_timestr<-substr(basename(foot), 1, 12)
#     foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
#     foot_timestrs<-seq(from = foot_timestr - (nhrs)*60*60,
#                       to   = foot_timestr-1,
#                       by   = 'hour')

    
#     if(foot_exist == T){
#        foot_brick<-try(brick(foot),silent = T) # broken foots issues
        
#         if(is.null(dim(foot_brick)) == FALSE){
#           foot_hours<-names(foot_brick)
#           foot_hours<-gsub("X","", foot_hours)
#           foot_hours<-substr(foot_hours, 1,16)
#           foot_hours<-as.POSIXct(foot_hours, format = "%Y.%m.%d.%H.%M", tz = "UTC")
          
#           new_flag<-foot_hours %in% foot_timestrs
#         if(TRUE %in% new_flag){
#           new_flag<-(1:length(new_flag))[new_flag == TRUE]
#           foot_brick<-subset(foot_brick, new_flag) # this should deal with mssing hours in foots
#           foot_sum<-calc(foot_brick, sum)
         
#           # print(paste0("footprint: ", foot))
#     #     # sum across layers
#     #       foot<-calc(foot, sum)
        
#     #     # normalize the footprint
#     #       foot<-foot/cellStats(foot, sum)
        
#     # convolve with each year (2014-2018) - take SD and mean
#     # also keep the value for the appropriate year if possible
#     # cropping will be critical

#       # city<-basename(dirname(dirname(dirname(dirname(foot)))))
      
    
#       SMUrF_home<-dir("/uufs/chpc.utah.edu/common/home/lin-group16/KaiW/SMUrF", full.names=T)
#       SMUrF_home<-SMUrF_home[grepl(city, SMUrF_home)==TRUE]
      
#     # get footprint ready
#       footprint_file<-try(nc_open(foot), silent =T)
    
#       if(length(footprint_file) > 1){
#         lons<-ncvar_get(footprint_file, varid="lon")
#         lats<-ncvar_get(footprint_file, varid="lat")
#         times<-ncvar_get(footprint_file, varid="time")
        
#         times<-times/86400
#         times<-chron(times)
#         # foot_time_chron <- chron(format(as.POSIXct(foot_timestrs, tz="UTC"), "%m/%d/%y %H:%M:%S"))
#         footprint<-ncvar_get(footprint_file, varid="foot")
#         # Reverse the times array
#         times_rev <- rev(times)
        

#         # Select from nhrs down to 1 (i.e., the first nhrs elements of the reversed array)
#         times <- times_rev[nhrs:1]
       
        
#         # print(foot_timestrs)
#         # print(names(foot_brick))
#         # print(names(foot_sum))
#         #  print(dim(foot_sum))
#         #  print(dim(foot_brick))
#         # stop("stop")
#         times_save<-times
    
#         # putting below in a loop for each year
#         years<-c("14","15","16","17","18")
#         # years<-c("15","16","17","18")
#         bios<-c()
#         for(y in years){
#           times<-chron(dates.=c(paste0(substr(times_save,2,7),y)), times.=c(substr(times_save, 11,18)))
    
#         # for straddling the new year
#           time_unique<-unique(substr(times_save,8,9))
#           if(length(time_unique) == 2 & y %in% c("14","15","17")){
#             times[substr(times,2,3) == "01"]<-times[substr(times,2,3) == "01"]+365
#           }
#           if(length(time_unique) == 2 & y == "18"){
#             times[substr(times,2,3) == "12"]<-times[substr(times,2,3) == "12"]-365
#           }
#           if(length(time_unique) == 2 & y == "16"){
#             times[substr(times,2,3) == "01"]<-times[substr(times,2,3) == "01"]+366
#           }
    
#         # read in appropriate monthly SMUrF file
#         # will change reionally which is tough
#           timestr<-unique(paste0("20",y, substr(times, 2,3))) # trying to account for overpasses at start/end of month
    
#           if(length(timestr) == 1){
#             SMUrF<-SMUrF_home[grepl(timestr, SMUrF_home) == TRUE]
#           }
#           if(length(timestr) == 2){
#             SMUrF1<-SMUrF_home[grepl(timestr[1], SMUrF_home) == TRUE]
#             SMUrF2<-SMUrF_home[grepl(timestr[2], SMUrF_home) == TRUE]
#             SMUrF<-c(SMUrF1, SMUrF2)
#           }
      
#           smurf_obj<-NULL
#           smurf_obj_time<-c()
#           for(sm in SMUrF){
#             SMUrF_sm_file<-try(nc_open(sm), silent =T)
#             if(length(SMUrF_sm_file) ==1){next()}
#               S_time_sm<-ncvar_get(SMUrF_sm_file, varid="time")
#               smurf_sm<-try(ncvar_get(SMUrF_sm_file, varid="NEE_mean"), silent=T)
#             if(length(smurf_sm) == 1){next()}
            
#             smurf_obj<-abind(smurf_obj, smurf_sm, along=3)
#             smurf_obj_time<-c(smurf_obj_time, S_time_sm)
#           }
#           if(is.null(smurf_obj)){next()}
          
#           S_lon<-ncvar_get(SMUrF_sm_file, varid="lon")
#           S_lat<-ncvar_get(SMUrF_sm_file, varid="lat")
          
#           S_time<-smurf_obj_time
#           S_time<-chron(S_time/86400)
#           smurf<-smurf_obj
      
      
#           # this needs to go both ways for a few sites because of AGB issues
#           # subset SMUrF to match footprint
#           smurf<-smurf[round(S_lon, digits=3) %in% round(lons, digits=3), round(S_lat, digits=3) %in% round(lats, digits=3),]
      
#           # subset the other way to fix places like Cape Town
#           # only do for first iteration of loop
#           if(y == "14"){
#           footprint<-footprint[round(lons, digits=3) %in% round(S_lon, digits=3), round(lats, digits=3) %in% round(S_lat, digits=3),]
#           }
      
#           if(y %in% c("14","15","17") & length(time_unique) == 2){
#             S_time[substr(S_time,2,3) == "01"] <-S_time[substr(S_time,2,3) == "01"] + 365
#           }
#           if(y == "18" & length(time_unique) == 2){
#             S_time[substr(S_time,2,3) == "12"] <-S_time[substr(S_time,2,3) == "12"] -365
#           }
#           if(y == "16" & length(time_unique) == 2){
#             S_time[substr(S_time,2,3) == "01"] <-S_time[substr(S_time,2,3) == "01"] + 366
#           }
#           # get times surrounding footprint_times
#           S_time_min<-S_time[abs(S_time - min(times)) < 1/24 & S_time <= min(times)]
#           S_time_max<-S_time[abs(S_time - max(times)) < 1/24 & S_time >= max(times)]
          
#           smurf<-smurf[,, S_time <= S_time_max & S_time >= S_time_min]
#           S_time<-S_time[S_time <= S_time_max & S_time >= S_time_min]
      
      
#           # create interpolated SMUrF values to match footprint times
#           smurf_interp<-NULL
#           for(i in 1:length(times)){
#             sub_smurf<-smurf[,,abs(S_time - times[i]) < 1/24]
            
#             if(length(dim(sub_smurf)) >2 ){
#               sub_smurf[is.na(sub_smurf)]<-0 # for interpolation purposes
              
#               slope_matrix<-(sub_smurf[,,2]-sub_smurf[,,1])/3600 # s-1
              
#               position<-as.numeric((times[i]-S_time[S_time <= times[i] & abs(S_time - times[i]) < 1/24]))*86400
#               time_unique_val<-0
#               if(time_unique_val %in% position){ position <-0}
            
#                 smurf_interp_i<-sub_smurf[,,1]+(slope_matrix*position)
#                 smurf_interp<-abind(smurf_interp, smurf_interp_i, along=3)
#               }
#               if(length(dim(sub_smurf)) ==2 ){
#                 smurf_interp<-abind(smurf_interp, sub_smurf, along=3)
#               }
#           }
#         
#           # bio<-sum(smurf_interp*footprint)
#           print(dim(smurf_interp))
#           print(dim(smurf_interp_resample))
#           # print(smurf_interp)
#           print(dim(foot_sum))
#           bio<-sum(smurf_interp*foot_sum)
#           bios<-c(bios,bio)
          
#           nc_close(SMUrF_sm_file)
#         } # end of year loop
#         print(bios)
#         stop("stop")
#         sd<-sd(bios)
#         avg<-mean(bios)
#         keep<-bios[years == substr(times_save[1],8,9)]
#         if(length(keep) == 0){keep<-NA} # 2019-2022 overpasses
        
#         bios<-c(avg,sd,keep)
#         write.table(bios, file=paste0(foot_byid,"/bio.txt"), quote=F, row.names=F)
        
#         nc_close(footprint_file)

#       } # length of footprint >1  loop
#     } # footprint file exists loop
#     gc()
#   }
# }
# }
# } # end of function

bio_func_nhrs<-Vectorize(bio_func_nhrs)

# BIO_func<-Vectorize(bio_func)

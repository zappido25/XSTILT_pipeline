# calculates the EDGAR based sectoral breakdowns for XCO2 enhancements
# could calculate the values for the background sites and subtract off to get contribution to the urban enhancement for each sector

# also run this for 24 hour sections
# Need to subset for nhrs 
# need to subset time for nhrs
# need to check raster/matrix transpose issue

#### need to sort out the foot times portion of this ###########################################################!!!!!!!!!!!!!!##!#!#!

edgar_func_nhrs<-function(foot, foot_byid, nhrs, city){
  library(ncdf4); library(lutz); library(dplyr); library(raster); library(lubridate); library(abind)
  
 
  # deal with footprints that do not exist
  for (i in 1:length(foot)){
    foot=foot[i]
    foot_byid=foot_byid[i]
    nhrs=nhrs[i]
    city=city[i]
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
    ### files needed for downscaling
      source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/downscaling/edgar.sector.weighting.vector.r")
      downscaling_dir<-"/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/downscaling"
      
      ### footprint info for downscaling function
      footprint_file<-try(nc_open(foot), silent =T)
  
    if(length(footprint_file) > 1){
      foot_year<-substr(unlist(strsplit(basename(foot),"_"))[1],1,4)
      foot_month<-substr(unlist(strsplit(basename(foot),"_"))[1],5,6)
      lon<-as.numeric(unlist(strsplit(basename(foot),"_")))[2]
      lat<-as.numeric(unlist(strsplit(basename(foot),"_")))[3]
      tz<-tz_lookup_coords(lat, lon)
    
  # 
      F_times<-ncvar_get(footprint_file, varid="time")
      F_times<-as.POSIXlt(F_times, origin = "1970-01-01 00:00:00", tz = "UTC")
      F_times<-F_times - as.numeric(substr(F_times, 18,19)) # remove seconds
      new_flag<-F_times %in% foot_timestrs

      
    # new_flag<-F_times %in% foot_timestrs  
      if(TRUE %in% new_flag){
        new_flag<-(1:length(new_flag))[new_flag == TRUE]
        F_times<-F_times[new_flag]
          
        foot_lon<-ncvar_get(footprint_file, varid="lon")
        foot_lat<-ncvar_get(footprint_file, varid="lat")
        
        footprint<-ncvar_get(footprint_file, varid="foot")
        footprint<-footprint[,,new_flag]
  
        # foot_lon<-foot_lon[foot_lon < 180]
        foot_times<-F_times # save for later
      
  
  # get times surrounding footprint times
  # going to do interpolation myself to avoid weekend/weekday issues in the downscaling code
        min<-min(F_times)
        min<-min- ((1+ as.numeric(min)/3600) %% 1)*3600
        
        max<-max(F_times)
        max<- max + ((1 - (1+ as.numeric(max)/3600)) %% 1)*3600
        
        run_times <- seq(from = min, to   = max, by   = 'hour')
       
        # F_times<-paste0(substr(run_times,1,4), substr(run_times,6,7), substr(run_times,9,10), substr(run_times,12,13),"0000")
        F_times <- c()
        # fix fir midnight wehre nchar is substr 12,13=""
        for (ii in 1:length(run_times)) {
          
            if (nchar(substr(run_times[ii], 12, 13)) == 0) {
              F_times <- c(F_times, paste0(substr(run_times[ii], 1, 4), substr(run_times[ii], 6, 7), substr(run_times[ii], 9, 10), "000000"))
          } else {
              F_times <- c(F_times, paste0(substr(run_times[ii], 1, 4), substr(run_times[ii], 6, 7), substr(run_times[ii], 9, 10),
             substr(run_times[ii], 12, 13), "0000"))
          }
        }
        
        
       
        # sectors we need to scale for
        sectors<-c("ENE", "REF_TRF", "IND", "TNR_Aviation_LTO","TNR_Aviation_CDS","TNR_Aviation_CRS", "TRO_noRES", "TNR_Other", "TNR_Ship", "RCO", "PRO", "NMM","CHE","IRO", "NFE","NEU","PRU_SOL")
        
        # Perform downscaling

        downscale_df<-edgar.sector.weighting(citylon = lon, citylat = lat, local.tz = tz,
                                                            sector.name = sectors, temporal.downscaling.files = downscaling_dir,
                                                            time = F_times, monthly = TRUE)

        downscale_df$time<-as.POSIXlt(downscale_df$time, format = "%Y%m%d%H%M%S", tz="UTC")

  
        interp_downscale<-NULL
        for(i in 1:length(foot_times)){
          
          for(j in unique(downscale_df$activity_code)){
            i_down<-downscale_df[abs(as.numeric(downscale_df$time) - as.numeric(foot_times[i])) < 3600 & downscale_df$activity_code ==j, ]
            
            slope<-(i_down$weighting[2]-i_down$weighting[1])/3600 # s^-1
            
            interp<-i_down$weighting[1]+(slope*(as.numeric(foot_times[i])-as.numeric(i_down$time[1])))
            interp_downscale<-rbind(interp_downscale, c(as.character(foot_times[i]),j,interp))
          }
        }
     
        interp_downscale<-as.data.frame(interp_downscale)
  
      print(interp_downscale)
      stop("here")
  # shift gears to the actual convolution
  # looks like emissions are in kg m-2 s-1 average over the month 
        if(as.numeric(foot_year) > 2021){foot_year <- "2021"}
        # edgar<-dir("/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/EDGAR/", pattern = paste0(foot_year,"_", as.numeric(foot_month)), full.names=T)
        edgar<-dir("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/EDGAR/EDGAR_v7/EDGARv7_Glb_0.1x0.1_anthro_CO2_excl_short-cycle_org_C__yearly",
         pattern = paste0(foot_year,"_", as.numeric(foot_month)), full.names=T)
        sector_conv<-NULL

        for(e in edgar){
          print(e)
          # get sector and name the sector
          name<-substr(basename(e), 1, nchar(basename(e))-11)
          sect<-unlist(strsplit(name, "_"))[9:length(unlist(strsplit(name, "_")))]
          
          # get appropriate downscaling
          e_down<-interp_downscale[interp_downscale$V2 == sect[1], ]
          if(length(sect) == 2){sect<- paste0(sect[1],"_",sect[2])}
          if(length(sect) == 3){sect<- paste0(sect[1],"_",sect[2],"_", sect[3])}
          
          # Open emissions file for this sector
          emissions<-brick(e)
  
    # spatial subset
    # tough because need to regrid footprint
          adjust.EDGAR.long <- function(x) {
            east <- crop(x, extent(0, 180, -90, 90))
            west <- crop(x, extent(180, 360, -90, 90))
            
            # then change extent of west to negative long
            extent(west) <- c(-180, 0, -90, 90)
            adj.x <- merge(west, east)
            return(adj.x)
          }
          emissions<-adjust.EDGAR.long(emissions)
          emissions<-crop(emissions, extent(min(foot_lon), max(foot_lon), min(foot_lat), max(foot_lat)))
          
          # convert raster resolution
          blank<-raster(nrow = dim(emissions)[1]*2, ncol = dim(emissions)[2]*2, xmn = extent(emissions)[1], xmx = extent(emissions)[2], ymn = extent(emissions)[3], ymx = extent(emissions)[4])
          emissions<-resample(emissions, blank, method = "ngb")
          
          # convert emissions from kg m-2 s-1 averaged over the month to kg m-2 month-1
          days<-days_in_month(as.POSIXlt(paste0(foot_year,foot_month,"01000000"), format = '%Y%m%d%H%M%S', tz = "UTC"))
          emissions<-emissions*60*60*24*days
          
          # convert to matrix
          # abind with different downscaling factors
          downscaled_emissions<-NULL # will be emissions object for these times and sector
          for(t in 1:length(e_down$V1)){
            t_emissions<-emissions*as.numeric(e_down$V3[t])
            t_emissions<-t_emissions/3600 # kg m-2 s-1 average for the hour
            
            t_emissions<-as.matrix(t_emissions) # emissions over course of hour
            
            # need to turn matrix to address raster::as.matrix() issue
            t_emissions<-t(t_emissions)[,dim(t_emissions)[1]:1]
            
            downscaled_emissions<-abind(downscaled_emissions, t_emissions,  along=3)
          }
    
          # Sector specific, temporally downscaled and interpolated convolution
          # need to convert to umol (44.01 g/mol) - need umol/kg
          downscaled_emissions<-downscaled_emissions*(1/44.01)*1000*1000000 # mol/g * g/kg * umol/mol

        
            
            
            
            # Sum footprint along the time domain
            footprint_sum <- apply(footprint, c(1, 2), sum, na.rm = TRUE)
         
            
            # Sum downscaled_emissions along the time domain
            downscaled_emissions_sum <- apply(downscaled_emissions, c(1, 2), sum, na.rm = TRUE)
           
            
            # Perform convolution
            # Resample footprint_sum and downscaled_emissions_sum to the same size
            footprint_raster <- raster(as.matrix(footprint_sum))
            extent(footprint_raster) <- extent(min(foot_lon), max(foot_lon), min(foot_lat), max(foot_lat))
           
            downscaled_emissions_raster <- raster(as.matrix(downscaled_emissions_sum))
            extent(downscaled_emissions_raster) <- extent(min(foot_lon), max(foot_lon), min(foot_lat), max(foot_lat))
           
            
            # resample the downscaled emissions to match the footprint
            downscaled_emissions_resampled <- resample(downscaled_emissions_raster, footprint_raster, method = "bilinear")
            
            # Convert resampled rasters back to matrices
            footprint_resampled <- as.matrix(footprint_raster)
            downscaled_emissions_resampled <- as.matrix(downscaled_emissions_resampled)
           
            # Perform convolution
            convolution <- sum(footprint_resampled * downscaled_emissions_resampled, na.rm = TRUE)
          
          sector_conv<-rbind(sector_conv, c(sect, convolution))
        }
  
        sector_conv<-data.frame( sect = sector_conv[,1], ppm = sector_conv[,2])
      
        sector_conv<-sector_conv[order(sector_conv$sect), ] # order to return
        print("sector conv")
        print(sector_conv)
        
        # one check on LA has ODIAC at 1.26ppm and EDGAR at 1.59
        write.table(sector_conv, file=paste0(foot_byid,"/edgar.txt"), quote=F, row.names=F)
        nc_close(footprint_file)
      }
   }
  }
  gc()
 }
}

edgar_func_nhrs<-Vectorize(edgar_func_nhrs)
# trouble shoot in serial 

#for(i in 1:length(df$nhrs)){
#  foot<-df$foot[i]
#  foot_byid<-df$foot_byid[i]
#  nhrs<-df$nhrs[i]
#  
#  edgar_func(foot, foot_byid, nhrs)
#  print(i)
#}


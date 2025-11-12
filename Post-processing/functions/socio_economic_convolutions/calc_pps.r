library(dplyr)
# function to calculate receptor level effective population density 
# requires population density dataset and X-STILT footprint at the appropriate resolution
# could add GDP dataset or other data we are interested in here

PPS_func<-function(foot, pop_dat){
  library(ncdf4); library(raster)
  
  if(is.na(foot)){return(NA)}
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
  
  # read in GPW data
  gpw<-brick(pop_dat)
  gpw<-subset(gpw, 1) # don't want layers even though it only has 1 layer
  # subset raster to match footprint
  
  gpw <- crop(gpw, extent(min(lons),max(lons),min(lats),max(lats)))

  # Resample GPW raster to match the resolution of the footprint
  gpw <- resample(gpw, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
  
  gpw<-as.matrix(gpw)
  gpw[is.na(gpw)]<-0
  
  # print(dim(gpw))
  # print(dim(footprint))
  pps<-sum(footprint*gpw)
  print(paste0("PPS: ", pps))
  return(pps)
}


eff_PPS_func_nhrs<-function(foot_ff, foot_byid_ff, nhrs_ff, city,shp_file){
  library(ncdf4); library(raster)
  
  for (i in seq_along(foot_ff)) {

  # deal with footprints that do not exist
  # for (i in 1:length(foot_ff)){
    foot=foot_ff[i]
    foot_byid=foot_byid_ff[i]
    nhrs=nhrs_ff[i]
    print(paste0("nhrs: ",nhrs))
    # if(nhrs=="-Inf") {
    #   nhrs=24
    # } 
     if (is.na(foot) || is.na(foot_byid)) {
      message("Skipping i=", i, " due to NA path")
      next
    }
    if(is.na(foot)){return(NA)}
    print(paste0("footprint: ", foot, " i: ", i))
    # print(paste0("footprint file: ", foot_ff[i], " i: ", i))
    # print(paste0("footprint file: ", foot_byid_ff[i], " i: ", i))
    # check if file exist
    

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
        # raster footprint cols and rows
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
  
      
      # # read in GPW data
      #   pop_dat<-"/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
        pop_dat<-"/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
      
        gpw<-brick(pop_dat)
        gpw<-subset(gpw, 1) # don't want layers even though it only has 1 layer
      # subset raster to match footprint
      
        # gpw <- crop(gpw, extent(min(lons)-0.002,max(lons)+0.002,min(lats)-0.002,max(lats)+0.002))
        # gpw <- resample(gpw, rasterFromXYZ(expand.grid(lons, lats)), method = "bilinear")
        pw <- crop(gpw, extent(fp_r))
        gpw <- resample(gpw, fp_r, method = "bilinear")
        gpw<-as.matrix(gpw)
       
        gpw[is.na(gpw)]<-0
      
        # annoying matrix turning issue
        gpw<-t(gpw)[,dim(gpw)[1]:1]
      
      # calc pps
        pps<-sum(footprint*gpw)
        if (shp_file){
            fname="/pps_shp.txt"
          }else{
            fname="/pps.txt"
          }
      # write output
        print(paste0(foot_byid,fname))
      
        write.table(pps, file=paste0(foot_byid,fname), quote=F, row.names=F)

        nc_close(footprint_file)
      
      }
    }
    }
    gc()
  }
}

eff_PPS_FUNC_nhrs<-Vectorize(eff_PPS_func_nhrs)

eff_PPS_func_nhrs_yearly <- function(foot, foot_byid, nhrs, city, shp_file) {
  library(ncdf4); library(raster)

  message("length foot: ", length(foot))

  for (i in seq_along(foot)) {
    foot_i      <- foot[i]
    byid_i      <- foot_byid[i]
    nhrs_i      <- nhrs[i]

    message("nhrs[", i, "] = ", nhrs_i)

    if (is.na(foot_i) || is.na(byid_i)) {
      message("Skipping i=", i, " due to NA path")
      next
    }

    message("footprint: ", foot_i, "  i: ", i)

    foot_exist <- file.exists(foot_i)
    message("foot_exist: ", foot_exist)
    if (!foot_exist) {
      next
    }

    # Parse timestamp from filename
    foot_timestr <- substr(basename(foot_i), 1, 12)
    foot_timestr <- as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_year    <- as.integer(format(foot_timestr, "%Y"))   # <- numeric year
    cat("foot_year: ", foot_year, "\n")

    # Build the hours sequence
    # (if nhrs_i is "-Inf" or similar, coerce/fix here)
    if (is.na(nhrs_i) || is.infinite(nhrs_i)) nhrs_i <- 24
    foot_timestrs <- seq(from = foot_timestr - (nhrs_i) * 3600,
                         to   = foot_timestr - 1,
                         by   = "hour")

    # Choose population raster by year
    base_path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
    if (foot_year < 2021) {
      pop_dat <- file.path(base_path, "gpw_v4_population_density_rev11_2020_30_sec.tif")
    } else {
      pop_dat <- file.path(base_path, paste0("usa_pop_", foot_year, "_CN_1km_R2025A_UA_v1.tif"))
    }

    gpw <- brick(pop_dat)
    gpw <- subset(gpw, 1)

    # Open footprint file
    nc <- try(nc_open(foot_i), silent = TRUE)
    if (inherits(nc, "try-error")) {
      message("Failed to open: ", foot_i)
      next
    }

    on.exit(try(nc_close(nc), silent = TRUE), add = TRUE)

    # Read vars
    lons <- ncvar_get(nc, "lon")
    lats <- ncvar_get(nc, "lat")
    foot_arr <- ncvar_get(nc, "foot")

    foot_hours <- ncvar_get(nc, "time")
    foot_hours <- as.POSIXct(foot_hours, tz = "UTC", origin = "1970-01-01 00:00:00")
    # (your seconds rounding—keep if needed)
    foot_hours <- foot_hours - as.numeric(substr(foot_hours, 18, 19))

    new_flag <- foot_hours %in% foot_timestrs
    if (!any(new_flag)) {
      message("No matching hours for i=", i)
      next
    }

    idx <- which(new_flag)
    foot_2d <- apply(foot_arr[, , idx, drop = FALSE], c(1, 2), sum)

    # Normalize footprint (if desired)
    s <- sum(foot_2d)
    if (is.na(s) || s == 0) {
      message("Zero/NA sum footprint at i=", i)
      next
    }
    foot_2d <- foot_2d / s

    # Build raster template from lon/lat centers
    nx <- length(lons); ny <- length(lats)
    dx <- mean(diff(lons)); dy <- mean(diff(lats))
    xmn <- min(lons) - dx/2; xmx <- max(lons) + dx/2
    ymn <- min(lats) - dy/2; ymx <- max(lats) + dy/2

    fp_r <- raster(nrows = ny, ncols = nx, xmn = xmn, xmx = xmx, ymn = ymn, ymx = ymx,
                   crs = CRS("+proj=longlat +datum=WGS84"))

    # Align population to footprint grid
    gpw_c <- crop(gpw, extent(fp_r))
    gpw_r <- resample(gpw_c, fp_r, method = "bilinear")
    gpw_m <- as.matrix(gpw_r)
    gpw_m[is.na(gpw_m)] <- 0

    # The raster matrix is row-major (y fastest). Flip to match footprint indexing if needed:
    gpw_m <- t(gpw_m)[, nrow(gpw_m):1]

    # PPS
    pps <- sum(foot_2d * gpw_m)
    message("PPS: ", pps)

    # Output file name by year & shp flag
    if (foot_year < 2021) {
      fname <- if (isTRUE(shp_file)) "/pps_shp.txt" else "/pps.txt"
    } else {
      fname <- if (isTRUE(shp_file)) "/pps_shp_yearly.txt" else "/pps_yearly.txt"
    }

    # Write
    out_path <- paste0(byid_i, fname)
    write.table(pps, file = out_path, quote = FALSE, row.names = FALSE)
    
    # close this nc explicitly (also handled by on.exit)
    try(nc_close(nc), silent = TRUE)
    gc()
  } # for
}
# eff_PPS_func_nhrs_yearly<-Vectorize(eff_PPS_func_nhrs_yearly)
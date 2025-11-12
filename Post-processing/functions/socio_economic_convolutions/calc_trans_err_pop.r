# function to calculate receptor level horizontal transport error in the context of effective population density

# Are the "log" aspects of these calculations correct?
# Returns < 1 if variance of perturbed parcels ends up being less
# Address vertical error corellations? 
# definitely want to address spatial correlations in error when aggregating receptors later

horr_pop<-function(trajs, pop_dat){
  library(raster);library(sf);library(dplyr);library(MASS)
  

  if(is.na(trajs)){return(NA)}
  # load trajectories
  trajs<-readRDS(trajs)
  trajs_err<-trajs$particle_error
  trajs_orig<-trajs$particle
  
  # load and subset population dataset
  gpw<-brick(pop_dat)
  #gpw<-subset(gpw, 1) 
  min_lon<-min(c(min(trajs$particle$long), min(trajs$particle_error$long)))-0.05
  min_lat<-min(c(min(trajs$particle$lati), min(trajs$particle_error$lati)))-0.05
  max_lon<-max(c(max(trajs$particle$long), max(trajs$particle_error$long)))+0.05
  max_lat<-max(c(max(trajs$particle$lati), max(trajs$particle_error$lati)))+0.05
  
  gpw <- crop(gpw, extent(min_lon,max_lon,min_lat,max_lat))
  
  # extract values from raster brick
  ras<-extract(gpw, cbind(trajs_orig$long, trajs_orig$lati))
  ras_err<-extract(gpw, cbind(trajs_err$long, trajs_err$lati))
  
  # combine population value with trajs
  trajs_orig$pop_dens<-as.vector(ras)
  trajs_err$pop_dens<-as.vector(ras_err)
  
  # subset columns
  trajs_orig<-trajs_orig[colnames(trajs_orig) %in% c('xhgt','ak.pwf','foot', 'pop_dens')]
  trajs_err<-trajs_err[colnames(trajs_err) %in% c('xhgt','ak.pwf','foot', 'pop_dens')]
  
  # get product of population density and footprint 
  trajs_orig$foot_pps<-trajs_orig$foot*trajs_orig$pop_dens
  trajs_err$foot_pps_err<-trajs_err$foot*trajs_err$pop_dens
  
  trajs_orig<-as.data.frame(trajs_orig)
  trajs_err<-as.data.frame(trajs_err)
  
  # xhgt for each set of trajs is identical, so not going to use inner_join() to save time
  trajs<-trajs_orig[, c(1,2,5)]
  trajs$foot_pps_err<-trajs_err$foot_pps_err
  
  # aggregate in time (using xhgt as indx since unique to each particle)
  # massive differences in cases where perturbed parcel is in PBL and hits high pop density compared to unperturbed above PBL and over pop density 0
  summary <- trajs %>% group_by(xhgt, ak.pwf) %>% summarise(foot_pps_sum = sum(foot_pps), foot_pps_sum_err = sum(foot_pps_err))
  summary<-as.data.frame(summary)
  
  # At time of Dien's ERL paper, X-STILT used discrete levels, so our calcs must be a bit different
  # To get per level errors, Dien would calculate the difference in log-normal standard deviations
  
  summary<-as.data.frame(summary)
  # toss out cases where both are zero because we are interested in differences
  #summary<-summary[summary$foot_pps_sum+ summary$foot_pps_sum_err > 0, ]
  
  # definitely need log space
  # Is getting rid of the zeros fair? 
  # Fit the distribution or just convert and calculate? 
  #fit_orig <- fitdistr(summary$foot_pps_sum[summary$foot_pps_sum != 0], "log-normal")
  #fit_orig_var<-fit_orig$estimate[2]**2
  
  #fit_err <- fitdistr(summary$foot_pps_sum_err[summary$foot_pps_sum_err != 0], "log-normal")
  #fit_err_var<-fit_err$estimate[2]**2
  
  # get log-normal variances
  orig_var<-sd(log(summary$foot_pps_sum[summary$foot_pps_sum != 0]))**2
  err_var<-sd(log(summary$foot_pps_sum_err[summary$foot_pps_sum_err != 0]))**2
  
  wind_err_var_surplus<-err_var - orig_var
  
  # Think we have to go back to linear space
  
  horr_trans_err<-exp(wind_err_var_surplus) # in terms of effective PPS - means anything less than 1 has smaller variance in perturbed parcels
  
  return(horr_trans_err)
}

# function to calculate receptor level horizontal transport error in the context of effective population density

# Are the "log" aspects of these calculations correct?
# Returns < 1 if variance of perturbed parcels ends up being less
# Address vertical error corellations? 
# definitely want to address spatial correlations in error when aggregating receptors later

horr_pop_update<-function(trajs,foot_fn, pop_dat){
  library(raster);library(sf);library(dplyr);library(MASS)
  
   print("start err")
   print(trajs)
  if(is.na(trajs)){return(NA)}
  # load trajectories

  trajs<-readRDS(trajs)
  trajs_err<-trajs$particle_error
  trajs_orig<-trajs$particle

   
  # load and subset population dataset
  gpw<-brick(pop_dat)
  gpw<-subset(gpw, 1)  
  min_lon<-min(c(min(trajs$particle$long), min(trajs$particle_error$long)))-0.05
  min_lat<-min(c(min(trajs$particle$lati), min(trajs$particle_error$lati)))-0.05
  max_lon<-max(c(max(trajs$particle$long), max(trajs$particle_error$long)))+0.05
  max_lat<-max(c(max(trajs$particle$lati), max(trajs$particle_error$lati)))+0.05
  
  # Check for NA in bounding box values
  if (any(is.na(c(min_lon, max_lon, min_lat, max_lat)))) {
    warning("One or more bounding box coordinates (min_lon, max_lon, min_lat, max_lat) are NA. using .nc file for lat lon")
    print(paste0("min_lon: ", min_lon))
    print(paste0("min_lat: ", min_lat))
    print(paste0("max_lon: ", max_lon))
    print(paste0("max_lat: ", max_lat))

    footprint<-nc_open(foot_fn)
    lons<-ncvar_get(footprint, varid="lon")
    lats<-ncvar_get(footprint, varid="lat")
    gpw <- crop(gpw, extent(min(lons),max(lons),min(lats),max(lats)))
  } else {
    gpw <- crop(gpw, extent(min_lon,max_lon,min_lat,max_lat))
  }
  gpw[is.na(gpw)]<-0
  
  
  # extract values from raster brick
  ras<-extract(gpw, cbind(trajs_orig$long, trajs_orig$lati))
  ras_err<-extract(gpw, cbind(trajs_err$long, trajs_err$lati))
  
  # combine population value with trajs
  trajs_orig$pop_dens<-as.vector(ras)
  trajs_err$pop_dens<-as.vector(ras_err)
  
  # subset columns
  trajs_orig<-trajs_orig[colnames(trajs_orig) %in% c('xhgt','ak.pwf','foot', 'pop_dens')]
  trajs_err<-trajs_err[colnames(trajs_err) %in% c('xhgt','ak.pwf','foot', 'pop_dens')]
  
  # get product of population density and footprint 
  trajs_orig$foot_pps<-trajs_orig$foot*trajs_orig$pop_dens
  trajs_err$foot_pps_err<-trajs_err$foot*trajs_err$pop_dens
  
  trajs_orig<-as.data.frame(trajs_orig)
  trajs_err<-as.data.frame(trajs_err)
  
  # xhgt for each set of trajs is identical, so not going to use inner_join() to save time
 
  trajs<-trajs_orig[, c(1,2,5)]
  
  # trajs<-trajs[trajs$xhgt %in% trajs_err$xhgt, ]
  
  # Check if dimensions match before assignment
  if (nrow(trajs) == nrow(trajs_err)) {
    trajs$foot_pps_err <- trajs_err$foot_pps_err
  } else {
    print("Dimensions of trajs and trajs_err do not match. Attempting to merge by keys.")
    
    trajs <- trajs %>% distinct(xhgt, ak.pwf, .keep_all = TRUE)
    trajs_err <- trajs_err %>% distinct(xhgt, ak.pwf, .keep_all = TRUE)
    trajs <- inner_join(trajs, trajs_err, by = c("xhgt", "ak.pwf"), suffix = c("", "_err"))

  }
   
  #trajs_err<-trajs_err[,c(1,2,5)]
  #trajs<-inner_join(trajs, trajs_err)# have to do this in a few cases
  
  # aggregate in time (using xhgt as indx since unique to each particle)
  # massive differences in cases where perturbed parcel is in PBL and hits high pop density compared to unperturbed above PBL and over pop density 0
  summary <- trajs %>% group_by(xhgt, ak.pwf) %>% summarise(foot_pps_sum = sum(foot_pps), foot_pps_sum_err = sum(foot_pps_err))
  summary<-as.data.frame(summary)
  
  # At time of Dien's ERL paper, X-STILT used discrete levels, so our calcs must be a bit different
  # To get per level errors, Dien would calculate the difference in log-normal standard deviations
  
  summary<-as.data.frame(summary)
  # toss out cases where both are zero because we are interested in differences
  #summary<-summary[summary$foot_pps_sum+ summary$foot_pps_sum_err > 0, ]
  
  # definitely need log space
  # Is getting rid of the zeros fair? 
  # Fit the distribution or just convert and calculate? 
  #fit_orig <- fitdistr(summary$foot_pps_sum[summary$foot_pps_sum != 0], "log-normal")
  #fit_orig_var<-fit_orig$estimate[2]**2
  
  #fit_err <- fitdistr(summary$foot_pps_sum_err[summary$foot_pps_sum_err != 0], "log-normal")
  #fit_err_var<-fit_err$estimate[2]**2
  
  # get log-normal variances
  orig_var<-sd(log(summary$foot_pps_sum[summary$foot_pps_sum != 0]))**2
  err_var<-sd(log(summary$foot_pps_sum_err[summary$foot_pps_sum_err != 0]))**2
  
  wind_err_var_surplus<-err_var - orig_var
  
  # Think we have to go back to linear space
  print(paste("wind_err_var_surplus:", wind_err_var_surplus))
  
  horr_trans_err<-exp(wind_err_var_surplus) # in terms of effective PPS - means anything less than 1 has smaller variance in perturbed parcels


  return(horr_trans_err)
}

HORR_pop<-Vectorize(horr_pop_update)
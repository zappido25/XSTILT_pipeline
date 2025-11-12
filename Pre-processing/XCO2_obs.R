#' extract XCO2, for any given overpass

library(dplyr)
homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)

setwd(xstilt_wd); source('r/dependencies.r')

# Please insert your API in the 'insert_ggAPI.csv' for use of ggplot and ggmap
# google API can be obtained from https://console.developers.google.com/
api.key = readLines('insert_ggAPI.csv')

OCO.DIR = Sys.getenv("OCO2_DIR")
sensor  = c('OCO-2', 'OCO-3', 'TROPOMI', NA)[1]

sensor_ver     = c('V11r', 'V10p4r', NA)[1]       # retrieval algo ver if there is
sensor_gas = c('CO2', 'CO', 'NO2', 'CH4')[1]  # only 1 gas per run
oco_path    = file.path(OCO.DIR, sensor, paste0('L2_Lite_FP_', sensor_ver))
sensor_path=oco_path
trp_path    = file.path(OCO.DIR, sensor, sensor_gas)

args <- commandArgs(trailingOnly = TRUE)

run_failed_jobs=FALSE

if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  run_failed_jobs <- as.logical(args[1])
  print(paste0("Run failed jobs mode is ", ifelse(run_failed_jobs, "ON", "OFF")))
}
if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  city <- as.character(args[2])
  print(paste0("Selected city is ", city))
}

if (run_failed_jobs) {
       overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  
        
} else {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
        incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t") 
}


    
# city_list <- file.path(xstilt_wd, "Cities_input_files/CONUS_cities_list_subset.txt")  # Replace "data.txt" with your file name


# CONUS <- read.table(city_list, header = TRUE, sep = "\t")  # Assuming tab-separated values with headers
    # Find indices where city from overpasses matches city from CONUS
# site=c("SanDiego")
    # for (ss in CONUS$City) {

store_path = file.path(OCO.DIR, 'XSTILT_output', city)
overpass_obs_path    = file.path(OCO.DIR, sensor, paste0('overpass_obs'))

for (ss in city) {
  matching_indices <- which(overpass_to_model$site %in% ss)

# Print matching indices for verification
    print(ss)
    print(matching_indices)

    for (ii in matching_indices) {
        site=ss
        timestr=overpass_to_model$timestr[ii]
        
        if (!run_failed_jobs) {
          if ((timestr %in% incomplete_runs$timestr)) {
              print("skipping failed runs")
              print(paste0("CITY::", ss))
              print(paste0("Timestr::", timestr))
              next
          }
        }
# *** modify the info path/directory that stores OCO-2/3 or TROPOMI data -------

      print(timestr)
   # Check if overpass_obs_path exists, else create it
      if (!dir.exists(overpass_obs_path)) {
        dir.create(overpass_obs_path, recursive = TRUE)
      }

      lon_lat  = get.lon.lat(site = site, dlon = 1, dlat = 1, api.key = api.key)
      site_lon = lon_lat$site_lon
      site_lat = lon_lat$site_lat



      obs_df = get.sensor.obs(site, timestr, sensor, sensor_gas, sensor_fn = NULL,
                                sensor_path, qfTF=T, tropomi_qa = sensor_qa, lon_lat)
      if ( is.null(obs_df) ) return()
      obs_df = obs_df %>% rename(val = xco2, val_uncert = xco2.uncert, 
                               time_utc = timestr) 


        # keep only distinct values
      obs_df = obs_df %>%
        dplyr::select(lon, lat, val, val_uncert, time_utc) %>%
        rename(XCO2 = val, XCO2_uncert = val_uncert, timestr = time_utc) %>%
        mutate(timestr = as.character(timestr)) %>%
        distinct() 

      obs_txtfile= paste0( site, '_', timestr, '.txt') 
      write.table(obs_df, file = file.path(overpass_obs_path, obs_txtfile), 
            col.names = T, sep = '\t', row.names = F, quote = F)          
      print(colnames(obs_df))
    }
}

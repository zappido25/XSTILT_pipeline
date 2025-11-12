library(grid)
library(gridExtra)
library(raster)
library(ncdf4)
library(terra)
library(sf)
library(rslurm)
library(dplyr)
library(stringr)


extract_recp_lat_lon <- function(file_name) {
  # Extract the longitude and latitude from the file name
  # Assumes the format "YYYYMMDDHHMM_lon_lat_X"
  # Updated pattern to match paths like ".../by-id/202206301755_-73.81953_41_X" or ".../by-id/202206301755_-73.81953_41.0_X"
  pattern <- ".*_(-?\\d+(?:\\.\\d+)?)_(-?\\d+(?:\\.\\d+)?)_X$"
  # pattern <- ".*_(-?\\d+\\.\\d+)_(-?\\d+\\.\\d+)_X$"
  matches <- regexec(pattern, file_name)
  coords <- regmatches(file_name, matches)[[1]]
  # print(coords)

  if (length(coords) < 3) {
    print(coords)
    stop("Invalid file name format. Expected format: YYYYMMDDHHMM_lon_lat_X")
  }
  
  lon <- as.numeric(coords[2])
  lat <- as.numeric(coords[3])


  return(list(lon = lon, lat = lat))
}

create_df <- function(obs,dir) {
  # Create a data frame with the extracted data
  byid.path = file.path(dir, 'by-id')

  folders <- list.files(byid.path, full.names = TRUE)
  for (folder in folders) {
    # Find the file matching the pattern "bio_nhrs.txt" in the folder
    
    smurf_file<- file.path(folder, paste0("bio_nhrs_shp.txt"))
    bio=read.table(smurf_file, header=T, stringsAsFactors = F)
    
    pps_file<- file.path(folder, paste0("pps_shp.txt"))
    pps=read.table(pps_file, header=T, stringsAsFactors = F)
    
    # odiac_shp_file<- file.path(folder, paste0("odiac_urban_influence.txt"))
    odiac_urban_shp_file<- file.path(folder, paste0("odiac_urban_influence.txt"))
    odiac_shp_urban=read.table(odiac_urban_shp_file, header=T, stringsAsFactors = F)
    
    odiac_city_shp_file<- file.path(folder, paste0("odiac_city_influence.txt"))
    odiac_shp_city=read.table(odiac_city_shp_file, header=T, stringsAsFactors = F)
    
    odiac_pop_city_file<- file.path(folder, paste0("city_influence.txt"))
    odiac_pop_city=read.table(odiac_pop_city_file, header=T, stringsAsFactors = F)
    
    odiac_pop_urban_file<- file.path(folder, paste0("urban_influence.txt"))
    odiac_pop_urban=read.table(odiac_pop_urban_file, header=T, stringsAsFactors = F)

    # print(paste0("Processing folder: ", folder))
    
    lat_lon=extract_recp_lat_lon(folder)
   
    matching_rows <- obs %>%
      filter(round(lat, 4) == round(lat_lon$lat, 4) & round(lon, 4) == round(lat_lon$lon, 4))
    if (nrow(matching_rows) > 0) {
      matching_rows <- matching_rows %>%
      mutate(
        odiac_shp_urban = mean(odiac_shp_urban$conv),
        odiac_shp_city = mean(odiac_shp_city$conv),
        odiac_pop_city = mean(odiac_pop_city$conv),
        odiac_pop_urban = mean(odiac_pop_urban$conv),
        smurf = ifelse(is.na(bio[1,]), 0, bio[1,]),
        pps = pps[1,]
        # smurf = ifelse(is.na(bio[3,]), 0, bio[3,])
      )
      if (!exists("data_df")) {
        data_df <- matching_rows
      } else {
        data_df <- bind_rows(data_df, matching_rows)
      }
      
    }

  }
    return(data_df)
}


binning <- function(xco2_data, bin_size,dir, timestr) {
    xco2_data$XCO2_bioadj <- xco2_data$XCO2 - xco2_data$smurf

    xco2_data$lat_bin<- floor(xco2_data$lat / bin_size) * bin_size
    bin_data <- xco2_data %>%
      group_by(lat_bin) %>%
      summarise(
        Avg_ODIAC_shp_urban = mean(odiac_shp_urban),
        Avg_ODIAC_shp_city = mean(odiac_shp_city),
        Avg_ODIAC_pop_city = mean(odiac_pop_city),
        Avg_ODIAC_pop_urban = mean(odiac_pop_urban),
        Avg_pdens_eff = mean(pps),
        Avg_XCO2_bioadj = mean(XCO2_bioadj),
        receptor_count = n()  # Count receptors in each bin
        
      ) %>%
      ungroup()
   
   # only use urban data for background receptors calculation
    bin_data <- bin_data %>%
      mutate(
        Norm_ODIAC_shp_urban = Avg_ODIAC_shp_urban / sum(Avg_ODIAC_shp_urban, na.rm = TRUE),
        Norm_ODIAC_pop_urban = Avg_ODIAC_pop_urban / sum(Avg_ODIAC_pop_urban, na.rm = TRUE),
        Norm_pdens_eff = Avg_pdens_eff / sum(Avg_pdens_eff, na.rm = TRUE),
        Norm_XCO2_bioadj = Avg_XCO2_bioadj / sum(Avg_XCO2_bioadj, na.rm = TRUE)
      ) %>%
      rowwise() %>%
      mutate(Anthro_Index = sum(Norm_ODIAC_shp_urban, Norm_pdens_eff, Norm_XCO2_bioadj, na.rm = TRUE)) %>%
      mutate(Anthro_Index_pop = sum(Norm_ODIAC_pop_urban, Norm_pdens_eff, Norm_XCO2_bioadj, na.rm = TRUE)) %>%
      ungroup()

      bin_data <- bin_data %>% arrange(Anthro_Index)
      # bin_data <- bin_data %>% arrange(Anthro_Index_pop)
    # anthro_index is the score    
   
    # Initialize an empty vector for selected bins and a variable to keep track of total receptors
    selected_bins <- c()
    total_receptors <- 0
    
    # Loop through bins sorted by the anthropogenic index (in ascending order)
    for (bin in bin_data$lat_bin) {
      current_receptor_count <- bin_data$receptor_count[bin_data$lat_bin == bin]
    
      # Add the current bin's receptor count to the total receptors
      total_receptors <- total_receptors + current_receptor_count
      
      # Add the current bin to the selected_bins
      selected_bins <- c(selected_bins, bin)
      
      # Check if the total receptor count is >= 5, if so, stop the loop
      if (total_receptors >= 5) {
        print(paste("Total receptors in selected bins:", total_receptors, "for bin:", bin))
        # Print the selected bins
        break
      }
    }

    
    # Label the bins as "Background" based on selected_bins
    bin_data <- bin_data %>%
      mutate(Background = ifelse(lat_bin %in% selected_bins, "Yes", "No"))
    
    # Print the selected bins (those labeled as "Background")
    print(bin_data %>% filter(Background == "Yes"))

    bin_data_file <- file.path(dir, paste0("bin_data_shp_",timestr,".csv"))
    
    
    xco2_data <- xco2_data %>%
      mutate(Background = ifelse(lat_bin %in% bin_data$lat_bin[bin_data$Background == "Yes"], "Yes", "No"))
    
    # Save updated extracted_data
    #xco2_data_file <- file.path(dir, "xco2_data_region.csv")
    # urban_threshold <- quantile(xco2_data$ODIAC_city, probs = 0.9, na.rm = TRUE)  
    urban_threshold_shp <- quantile(xco2_data$odiac_shp_city, probs = 0.9, na.rm = TRUE)  
    urban_threshold_pop <- quantile(xco2_data$odiac_pop_city, probs = 0.9, na.rm = TRUE)  
  
# Assign 'Urban-enhanced' label based on the threshold at the bin level
  bin_data <- bin_data %>%
    mutate(Urban_shp = ifelse(Avg_ODIAC_shp_city >= urban_threshold_shp, "Yes", "No"))
  bin_data <- bin_data %>%
    mutate(Urban_pop = ifelse(Avg_ODIAC_pop_city >= urban_threshold_pop, "Yes", "No"))


  # Sort bins by latitude for sequential analysis
  bin_data <- bin_data %>% arrange(lat_bin)

  # Identify bins that are unassigned but fall between urban-enhanced bins
  urban_bins_shp <- bin_data$lat_bin[bin_data$Urban_shp == "Yes"]
  urban_bins_pop <- bin_data$lat_bin[bin_data$Urban_pop == "Yes"]


  
  if (length(urban_bins_shp) > 1) {
    print(timestr)
    for (i in seq_along(urban_bins_shp[-length(urban_bins_shp)])) {
      start_bin <- urban_bins_shp[i]
      end_bin <- urban_bins_shp[i + 1]
      
      # Assign bins between start_bin and end_bin as 'Urban-enhanced'
      bin_data <- bin_data %>%
        mutate(Urban_shp = ifelse(lat_bin > start_bin & lat_bin < end_bin & Urban_shp == "No",
                                    "Yes", Urban_shp))

    }
  }
  if (length(urban_bins_pop) > 1) {
    for (i in seq_along(urban_bins_pop[-length(urban_bins_pop)])) {
      start_bin <- urban_bins_pop[i]
      end_bin <- urban_bins_pop[i + 1]
      
      # Assign bins between start_bin and end_bin as 'Urban-enhanced'
      bin_data <- bin_data %>%
        mutate(Urban_pop = ifelse(lat_bin > start_bin & lat_bin < end_bin & Urban_pop == "No",
                                    "Yes", Urban_pop))

    }
}

  print(paste0("writting to file in loop length(urban_bins_pop)>1::", bin_data_file))
  write.csv(bin_data, bin_data_file, row.names = FALSE)
  # print(bin_data)
  xco2_data <- xco2_data %>%
    # left_join(bin_data %>% select(lat_bin, Urban_pop), by = "lat_bin")
    left_join(bin_data %>% select(lat_bin, Urban_shp), by = "lat_bin")
      return(xco2_data)
  # else{
  #   print(paste0("writing to file::", bin_data_file))
  #   write.csv(bin_data, bin_data_file, row.names = FALSE)
  # }

 
}

group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
r_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/plotting_XSTILT/get.urban.extent.r')
 
print(r_files)
invisible(lapply(r_files, source))

# base_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/Phoenix/V11r/"
# out.dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/OVERPASS_OUTPUTS/test_outputs/dataframe_output"

# Get all directories starting with "out_"
OCO.DIR = Sys.getenv("OCO2_DIR")
overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_all.txt"),
                header = TRUE, sep = "\t")  

city=c("SanDiego")
# for (ss in CONUS$City) {
for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)
 
  
    dir_path <- (paste0(OCO.DIR, "/XSTILT_output/",
                      ss, "/", version))
    base_dir <- dir(dir_path, full.names = TRUE)  
      

    for (ii in matching_indices) {
        site=ss
        timestr=overpass_to_model$timestr[ii]
        # print(paste0("CITY::", site))
        # print(paste0("Timestr::", timestr))

        if (timestr < '2015000000' || timestr %in% incomplete_runs$timestr) {
            print("skipping runs outside date range or failed runs")
            print(paste0("CITY::", ss))
            print(paste0("Timestr::", timestr))
            next
        }

      dirs <- base_dir[grepl(paste0("out_",timestr), base_dir)]  # Filter directories starting with "out_"

          
      xco2_fname=paste0(site,"_",timestr,".txt")
      obs <- read.table(paste0(file.path(OCO.DIR, "/OCO-2/overpass_obs/",xco2_fname)),
          header = TRUE) 
      out.dir <- paste0(out.path,"/dataframe_output/", site)

      print(paste0("timestr::",timestr, " dirs::", dirs))     

        df_recp=create_df(obs,dirs)
      # Check if out.dir exists, if not, create it
      if (!dir.exists(out.dir)) {
        dir.create(out.dir, recursive = TRUE)
      }

      xco2_binned=binning(df_recp,0.05, out.dir, timestr)
      # print(xco2_binned)

      xco2_data_file <- file.path(out.dir, paste0("xco2_data_region_shp_",timestr,".csv"))
          write.csv(xco2_binned, xco2_data_file, row.names = FALSE)

    }
}
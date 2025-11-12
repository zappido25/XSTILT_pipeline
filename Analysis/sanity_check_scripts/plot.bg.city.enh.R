library(grid)
library(gridExtra)
library(raster)
library(ncdf4)
library(terra)
library(sf)
library(ggplot2)
library(dplyr)
library(ggmap)

out.path=Sys.getenv("OUT_DF_DIR")
OCO.DIR = Sys.getenv("OCO2_DIR")

functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
print(functions_files)

urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
# cluster code for this city 

# get stadia maps API key from file and register
api.key = readLines('../../insert_ggAPI.csv')
register_google(key = api.key)
stadia.api.key = readLines('../../insert_stadia_key.csv')
register_stadiamaps(key = stadia.api.key)

shp_file = FALSE  # whether to use the shapefile based clustering or not

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  shp_file <- as.logical(args[1])
  print(paste0("Running for shp_file is ", ifelse(shp_file, "TRUE", "FALSE")))
}


enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                          "int_sd", "int_epc_mean", "int_epc_sd", "int_epc_gdp_mean", "int_epc_gdp_sd",
                           "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                          "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                          "consumption_mean", "consumption_sd",
                            "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")

data_cols_vulcan=c("lat_mid","lat_bin","mean_XCO2","mean_XCO2.uncert","bio_ppm",
            "bio_uncert","city_ppm","city_sd","odiac_ppm","odiac_sd","odiac_total",
            "odiac_tot_sd","recp_num","edgar_ppm","sd_edgar","mean_pps","sd_pps",
            "mean_gdp","mean_gdp_footprint","vulcan_ppm","sd_vulcan","mean_foot_pps","mean_err_pps","mean_lights",
            "na_count","city_frac","category","enhancement","Epc")
# data_cols=c("lat_mid","lat_bin","mean_XCO2","mean_XCO2.uncert","bio_ppm",
#             "bio_uncert","city_ppM","city_sd","odiac_ppM","odiac_sd","odiac_total",
#             "odiac_tot_sd","recp_num","edgar_ppM","sd_edgar","mean_ppS","sd_ppS",
#             "mean_gdp","mean_gdp_footprint","mean_foot_ppS","mean_err_ppS","mean_lights",
#             "na_count","city_frac","category","enhancement","Epc")

# Read in cities file and prompt user to select city
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities <- read.table(cities_file, header = FALSE, skip=0, stringsAsFactors = FALSE)[,1]
cat("Available cities:\n")
for (i in seq_along(cities)) {
  cat(sprintf("%d: %s\n", i, cities[i]))
}

# Force interactive prompt even under Rscript
cat("Enter the number of the city to analyze: ")
city_idx <- as.integer(readLines(file("stdin"), n = 1))

if (is.na(city_idx) || city_idx < 1 || city_idx > length(cities)) {
  stop("Invalid city selection.")
}
city <- cities[city_idx]
cat(paste0("Selected city: ", city, "\n"))

# Check if "sanity_check_figures" folder exists, else create it
if (!dir.exists("sanity_check_figures/")) {
  dir.create("sanity_check_figures/", recursive = TRUE)
}

if (!shp_file){
  # pdf_filename <- paste0("sanity_check_figures/",city, "_CC_bg_city.pdf")
  pdf_filename <- paste0("sanity_check_figures/",city, "_deltaCO2.pdf")
}else{
  pdf_filename <- paste0("sanity_check_figures/", city,"_UMICH_bg_city.pdf")
}
pdf(pdf_filename, width = 8, height = 6)
# get map dimensions for this city
half_width <- 1.0   # in degrees
half_height <- 1.0  # in degrees

center_lon <- urban_core$lon[urban_core$city == city]
center_lat <- urban_core$lat[urban_core$city == city]

bbox <- c(
  left   = center_lon - half_width,
  bottom = center_lat - half_height,
  right  = center_lon + half_width,
  top    = center_lat + half_height
)

# create the basemap
basemap <- get_stadiamap(
    bbox = bbox,
    zoom = 10,
    maptype = "outdoors"#"stamen_terrain"  # Options: stamen_terrain, stamen_toner, stamen_watercolor
)



# enh_folder=file.path(out.path, "ENHANCEMENTS", city)
enh_folder=file.path(out.path, "ENHANCEMENTS_vulcan", city)
data_folder=file.path(out.path, "DATAFRAME_vulcan", city)

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  

na_counts=list()
# main loop start here
# saves the enhancements and sectoral data to a dataframe

for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)

    for (ii in matching_indices) {
    # for (ii in 1:1) {

      timestr=overpass_to_model$timestr[ii]
      print(paste0("Processing city ", ss, " with timestr ", timestr))
      if (timestr < '2015000000' || timestr %in% incomplete_runs$timestr) {
              print("skipping runs outside date range or failed runs")
              print(paste0("CITY::", ss))
              print(paste0("Timestr::", timestr))
              next
      }

      if(!shp_file){
        enh_cc_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
        enh_cc_readin <- t(read.table(enh_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
        colnames(enh_cc_readin) <- enhancement_cols
        df <- as.data.frame(enh_cc_readin)
        data_cc_file <- file.path(data_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
        data_cc_readin <- (read.table(data_cc_file, header = TRUE, sep = "")) # transpose the table
        df_data <- as.data.frame(data_cc_readin)
      
      }else{
        enh_shp_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
        enh_shp_readin <- t(read.table(enh_shp_file, header = FALSE, sep = ",", skip = 1))
        data_shp_file <- file.path(data_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
        data_shp_readin <- (read.table(data_shp_file,header = TRUE, sep = "")) # transpose the table
        colnames(enh_shp_readin) <- enhancement_cols
        df <- as.data.frame(enh_shp_readin)
        df_data <- as.data.frame(data_shp_readin)
      
      }


        # IF enh_cc_readin values are nan, create a new column in df_data to indicate this
      # if (as.numeric(df[,"na_count"]) == 100000 ) {
      #   cat("only considering runs where the values are nans for timestr:", timestr, "\n")
      #   df_data = df_data %>%
      #   mutate(enh_out_is_nan=TRUE)
      # } else {
      #   df_data = df_data %>%
      #   mutate(enh_out_is_nan=FALSE)
      # }    
        df_data = df_data %>%
        mutate(enh_out_is_nan=FALSE)
        # print(nrow(df_data))
        print(any(df_data$category == "bg", na.rm = TRUE))
        print(timestr)
      if (any(df_data$category == "bg", na.rm = TRUE)) {
        # na_counts[[paste0(ss,"_",timestr)]] <- df$na_count
        print(df_data$mean_vulcan_ppm)
        p_plot_bg_city_enh <- deltaCO2_comparisons(ss, basemap,timestr, df_data)
        print(p_plot_bg_city_enh)  
      }
    #   p_plot_bg_city_enh <- plot_bg_city_enh(ss, basemap,timestr, df_data)

    #  print(p_plot_bg_city_enh)
    }
}

dev.off()
# -- core processing pipeline --
# compare the enh and epc for multiple cities
# this script compares the enhancement and emissions per capita data
# uses the sectoral and enhancements data to process the seasonality of the emissions per capita and sectoral 
# contributions to these emissions.


library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidygeocoder)
library(sf)
library(maps)
library(ggmap)
library(raster)
library(sf)
# options(error = traceback)

## ploting and parsing functions are here
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
# print(functions_files)
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


if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  us_areas <- as.logical(args[2])
  print(paste0("Running for us_areas is ", ifelse(us_areas, "TRUE", "FALSE")))
}

group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
OCO.DIR = Sys.getenv("OCO2_DIR")

sector_cols <- c(
  "CHE","ENE","IND","IRO","NEU","NFE","NMM","PRO","PRU_SOL","RCO",
  "REF_TRF","TNR_Aviation_CDS","TNR_Aviation_CRS","TNR_Aviation_LTO",
  "TNR_Other","TNR_Ship","TRO_noRES"
)



# Read in cities file and prompt user to select city
# Read the list of cities
# if not_us_areas, read from simulated_cities.txt

cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
cities_file_all <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]
cities_all <- read.table(cities_file_all, header = FALSE, skip = 1, stringsAsFactors = FALSE)[, 1]

# p_usa=plot.us.maps(cities)
# p_usa_current_simulations=plot.us.maps_all_cities(cities, cities_all)
df_coords <- auto_region(cities)
print(df_coords)

library(dplyr)
library(tidygeocoder)
library(stringr)

# Create Census lookup table
census_lookup <- tribble(
  ~state, ~division, ~region,
  "CT","New England","Northeast",
  "ME","New England","Northeast",
  "MA","New England","Northeast",
  "NH","New England","Northeast",
  "RI","New England","Northeast",
  "VT","New England","Northeast",
  "NJ","Middle Atlantic","Northeast",
  "NY","Middle Atlantic","Northeast",
  "PA","Middle Atlantic","Northeast",
  "IL","East North Central","Midwest",
  "IN","East North Central","Midwest",
  "MI","East North Central","Midwest",
  "OH","East North Central","Midwest",
  "WI","East North Central","Midwest",
  "IA","West North Central","Midwest",
  "KS","West North Central","Midwest",
  "MN","West North Central","Midwest",
  "MO","West North Central","Midwest",
  "NE","West North Central","Midwest",
  "ND","West North Central","Midwest",
  "SD","West North Central","Midwest",
  "DE","South Atlantic","South",
  "DC","South Atlantic","South",
  "FL","South Atlantic","South",
  "GA","South Atlantic","South",
  "MD","South Atlantic","South",
  "NC","South Atlantic","South",
  "SC","South Atlantic","South",
  "VA","South Atlantic","South",
  "WV","South Atlantic","South",
  "AL","East South Central","South",
  "KY","East South Central","South",
  "MS","East South Central","South",
  "TN","East South Central","South",
  "AR","West South Central","South",
  "LA","West South Central","South",
  "OK","West South Central","South",
  "TX","West South Central","South",
  "AZ","Mountain","West",
  "CO","Mountain","West",
  "ID","Mountain","West",
  "MT","Mountain","West",
  "NV","Mountain","West",
  "NM","Mountain","West",
  "UT","Mountain","West",
  "WY","Mountain","West",
  "AK","Pacific","West",
  "CA","Pacific","West",
  "HI","Pacific","West",
  "OR","Pacific","West",
  "WA","Pacific","West"
)



regions= unique(df_coords$region)  

enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                        "int_sd", "Epc", "int_epc_sd", "CI", "CI_sd", #"int_epc_gdp_mean" is carbon intensity
                          "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                        "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                        "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")



# save the results to a PDF in the folder "figures"
if (!shp_file){
  pdf_filename <- paste0("../figures/", "CC_us_regions_Seasonality.pdf")
}else{
  pdf_filename <- paste0("../figures/", "UMICH_us_regions_Seasonality.pdf")
}
# pdf_filename <- paste0("figures/", "cities_Seasonality.pdf")
pdf(pdf_filename, width = 8, height = 6)
print(pdf_filename)
# get enhancements and sectoral data folders


overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  
na_counts=list()

# now lets look at the pop change
urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
path_pop="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
population_density_data_file<-paste0(path_pop,"gpw_v4_population_density_rev11_2020_30_sec.tif")

# us census pop data
pop_file=read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/City_Population_By_Year.csv",
                    header = TRUE,stringsAsFactors = FALSE)

colnames(pop_file) <- c("city", "year", "population")
pop_ratio_data=read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/sanity_check_scripts/population_ratio_scaled_to_2020.csv",
                    header = TRUE,stringsAsFactors = FALSE)
# interpolate for missing 2020 year
# pop_interp <- pop_file %>%
#   arrange(city, year) %>%
#   group_by(city) %>%
#   # Fill missing years with complete() then interpolate population
#   complete(year = full_seq(year, 1)) %>%
#   arrange(year) %>%
#   mutate(
#     population = zoo::na.approx(population, x = year, na.rm = FALSE),  # linear interpolation
#     growth_abs = population - lag(population),
#     growth_pct = 100 * (population - lag(population)) / lag(population)
#   ) %>%
#   ungroup()

# population_density_data_file <-  "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
gpw_dens <- raster::brick(population_density_data_file)    
gpw_dens<-subset(gpw_dens, 1)
successfull_overpasses <- c()
# main loop start here
# saves the enhancements and sectoral data to a dataframe
# print(regions)
all_regions <- list()
# for (reg in regions) {
for (reg in regions) {
    print(paste0("Processing region: ", reg))
    selected_cities <- df_coords %>% filter(region == reg) %>% pull(city)
    print(selected_cities)
  # fresh for each region
  enh_list <- list()
  for (city in selected_cities) {
    city <- sub("^New York$", "NewYork", city)
    city <- sub("^Los Angeles$", "LosAngeles", city)
    city <- sub("^San Diego$", "SanDiego", city)

    print(paste0("city::", city))

    default_cutoff <- '2015000000'  # Default cutoff for most cities
    strict_cutoff <- '2015050000'  # strict cutoff for few cities

    print(paste0("Processing city: ", city," from region: ", reg))
    matching_indices <- which(overpass_to_model$site %in% city)

    enh_folder=file.path(out.path, "EPC", city)
    sect_folder=file.path(out.path, "SECTORS", city)
    
    # get the time string for the city

    files <- list.files(enh_folder, pattern = paste0("^", city, "_[0-9]+.*\\.txt$"),
                    full.names = FALSE)
    if (!length(files)) {
      message("  • No files found for city ", city)
      next
        }

    ## extract timestr parts from filenames
    ts_from_files <- sub(paste0("^", city, "_([0-9]+).*"), "\\1", files)

    ## choose cutoff based on minimum timestr in folder
    min_ts <- min(ts_from_files, na.rm = TRUE)
    city_cutoff <- if (min_ts >= strict_cutoff) strict_cutoff else default_cutoff
    message("City=", city, " → min_ts=", min_ts, " → cutoff=", city_cutoff)
    
    cluster_code<-urban_core$ID[urban_core$city == city]
    cluster_city<-cluster[cluster$cluster_id == cluster_code, ] 
    cluster_pop=urban_core$core_pop[urban_core$city == city]
    cluster_lat=urban_core$lat[urban_core$city==city]
    cluster_lon=urban_core$lon[urban_core$city==city]

    print(paste0("Cluster code is ", cluster_code, " with core pop ", cluster_pop))

    city_points <- SpatialPointsDataFrame(coords = cluster_city[,1:2], data = cluster_city[,1:2],
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  

      for (ii in matching_indices) {
        
        timestr=overpass_to_model$timestr[ii]


        if (timestr < city_cutoff || timestr %in% incomplete_runs$timestr) {
                print("skipping runs outside date range or failed runs")
                print(paste0("CITY::", city))
                print(paste0("Timestr::", timestr))
                next
        }
        
        year=substr(timestr, 1, 4) 
      
        city_df <- pop_ratio_data[pop_ratio_data$city == city, ]
    
        # ratio_pop=worldpop_total_city/gpw_total_pop
        # population_growth_percent=city_df$growth_pct[city_df$year == as.numeric(year)]  
        # print(paste0("Ratio of core pop to WorldPop total pop in cluster: ", ratio_pop))
       
        # print(paste0("City population density daqta points: ", nrow(gpw_dens_city), " from GPW and ", nrow(world_pop_city), " from WorldPop"))

        
        if (!shp_file) {
          enh_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
          enh_readin <- t(read.table(enh_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table  
          colnames(enh_readin) <- enhancement_cols
          sect_file <- file.path(sect_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
          method_val= "CC"
        } else{
          enh_file <- file.path(enh_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
          enh_readin <- t(read.table(enh_file, header = FALSE, sep = ",", skip = 1))
          colnames(enh_readin) <- enhancement_cols
          sect_file <- file.path(sect_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
          method_val= "Umich"
        }
        
        # Read the data from the files, skipping the first row and transposing
    
        # indicates that the enhamcements and sectorla data is empty or filled with NaNs
        if (as.numeric(enh_readin[,"na_count"]) == 100000 ) {
          cat("Skipping iteration due to city clustering na_count == 100000 for timestr:", timestr, " for city:", city, "\n")
          na_counts[[length(na_counts) + 1]]  <- timestr
          next
        } else {
          print(timestr)
              sect_readin <- t(read.table(sect_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
              # if (!identical(dim(sect_readin), dim(sect_cols))) {
              #     next
              # }else {
                colnames(sect_readin) <- sector_cols
              # }
          }

        # if (!exists("enh_list")) enh_list <- list()

  #       # Combine enhancement and sectoral data by columns, then add to the lists
        enh_combined  <- cbind(as.data.frame(enh_readin),  as.data.frame(sect_readin))  
        enh_combined$region <- reg
        enh_combined$pop_ratio <- city_df$pop_ratio[city_df$year==year]  #ratio_pop
        enh_combined$lat = cluster_lat
        enh_combined$lon = cluster_lon
        enh_list[[length(enh_list) + 1]]  <- enh_combined
      }
  }
  # for each region, combine the list into a dataframe to check the rows
  reg_df <- dplyr::bind_rows(enh_list)
  message( "Rows of dataframe which have good data = ", nrow(reg_df), " for region ", reg, "\n")
   if (length(enh_list)) {
    all_regions[[reg]] <- dplyr::bind_rows(enh_list)
  } else {
    message("No valid rows for region ", reg)
  }
}
  # # Final per-method dataframes
enh_df <- dplyr::bind_rows(all_regions)


# # MAIN DATAFRAME with pasrsed timestrs, i.e categorizing it in seasons, SON, DJF, MAM, JJA
enh_df  <- parse_times(enh_df)
message( "Rows of dataframe which have good data = ", nrow(enh_df))

## Extract city coordinates
df_city_coords <- enh_df %>%
  select(city, lat, lon) %>%
  distinct()

# Reverse geocode to State
df_city_state <- df_city_coords %>%
  reverse_geocode(lat = lat, long = lon, method = "osm") %>%
  mutate(State = str_extract(display_name, ",\\s*([A-Z]{2})") %>% str_remove(", "))

# Merge Census Division + Region
df_city_region <- df_city_state %>%
  left_join(census_lookup, by = c("State" = "state")) %>%
  select(city, State, region, division)

print(df_city_region)
# remove outliers based on Kai's method -> beyond ±2 SD of median of Epc
enh_df  <- remove_outliers(enh_df)
cat("for region ", reg, " after removing outliers, dim is ", dim(enh_df), "\n")


print(paste0("Final dim of the combined dataframe is ", dim(enh_df)))
# print(colnames(enh_df))



# get seasonal profiles
enh_seasonal  <- seasonal_epc_us(enh_df)
enh_light_dark  <- light_dark_us(enh_df)



# If enh_df is combined across all regions and already parse_times()-ed:
# EPC (tg CO2/person/year) with SD bars
#period is 2015-2019, 2020, 2021-2024
p_epc_period <- plot_epc_by_region_period(enh_df,
                                   method = "CC",
                                   value_col = "Epc",
                                   regions = regions)

# Carbon intensity: CI originally in tg CO2/USD → scaled to kg CO2/USD
# p_ci_period <- plot_epc_by_region_period(enh_df,
#                                   method = "CC",
#                                   value_col = "CI",
#                                   regions = regions)

# 1) EPC timeseries trends yearly means with SD bars
p_epc_ts <- plot_yearly_epc_by_region(enh_df)

p_epc_season=plot_epc_region_season(enh_df, value = "Epc", scale_multiplier = 1)
# p_ci_season=plot_epc_region_season(enh_df, value = "CI", scale_multiplier = 1e6)

# epc vs lat
p_epc_lat=plot_epc_vs_latitude(enh_df,)
#lat long epc
p_epc_lat_lon=plot_epc_lat_lon(enh_df)

# print(p_usa)
print(p_epc_period)
# print(p_ci_period)
print(p_epc_ts)
print(p_epc_season)
print(p_epc_lat)
print(p_epc_lat_lon)
# print(p_ci_season)
dev.off()




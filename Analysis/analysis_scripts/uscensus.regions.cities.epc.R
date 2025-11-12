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
library(stringr)

library(FSA)   # For Dunn test
library(rstatix)
# options(error = traceback)

## plotting and parsing functions are here
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))

# get Stadia/Google API keys and register
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

group    = Sys.getenv("group")
version  = Sys.getenv("version")
out.path = Sys.getenv("OUT_DF_DIR")
OCO.DIR  = Sys.getenv("OCO2_DIR")

sector_cols <- c(
  "CHE","ENE","IND","IRO","NEU","NFE","NMM","PRO","PRU_SOL","RCO",
  "REF_TRF","TNR_Aviation_CDS","TNR_Aviation_CRS","TNR_Aviation_LTO",
  "TNR_Other","TNR_Ship","TRO_noRES"
)

# Read lists
cities_file     <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
cities_file_all <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities          <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]
cities_all      <- read.table(cities_file_all, header = FALSE, skip = 1, stringsAsFactors = FALSE)[, 1]



# (If you prefer 4 Census REGIONS instead, swap to:)
# regions <- unique(df_coords$census_region)

enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                     "int_sd", "Epc", "int_epc_sd", "CI", "CI_sd",
                     "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                     "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd",
                     "bg_gdp_mean", "bg_gdp_sd", "consumption_mean", "consumption_sd",
                     "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")

# save the results to a PDF in the folder "figures"
if (!shp_file){
  pdf_filename <- paste0("../figures/", "CC_us_censusDivisions_Seasonality.pdf")
}else{
  pdf_filename <- paste0("../figures/", "UMICH_us_censusDivisions_Seasonality.pdf")
}
pdf(pdf_filename, width = 8, height = 6)
print(pdf_filename)

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs_4analysis.txt"),
                header = TRUE, sep = "\t")

na_counts <- list()

# pop & cluster inputs
urban_core <- read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt",
                         header=TRUE, stringsAsFactors = FALSE)
cluster    <- readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
path_pop   <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
population_density_data_file <- paste0(path_pop,"gpw_v4_population_density_rev11_2020_30_sec.tif")

pop_file <- read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/City_Population_By_Year.csv",
                     header = TRUE,stringsAsFactors = FALSE)
colnames(pop_file) <- c("city", "year", "population")

pop_ratio_data <- read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/sanity_check_scripts/population_ratio_scaled_to_2020.csv",
                           header = TRUE,stringsAsFactors = FALSE)

gpw_dens <- raster::brick(population_density_data_file)    
gpw_dens <- subset(gpw_dens, 1)

# 🔁 New: census-based region/division classification
df_coords <- auto_us_region_from_urban_core(cities, urban_core)
print(df_coords)

p_region_map <- ggplot(df_coords, aes(lon, lat, color=census_region, shape=census_region)) +
  borders("state") +
  geom_point(size = 5, stroke = 1.5) +
  geom_text(aes(label = city), nudge_y = 0.8, show.legend = FALSE) +
  coord_fixed(1.3) +
  scale_color_brewer(palette="Dark2") +
  labs(title="US Census Regions Assigned to Cities") +
  theme_minimal(base_size = 14)

print(p_region_map)
# Choose grouping:
# 🔽 Use Census DIVISIONS (9) as groups (recommended)
# regions <- unique(df_coords$division)
regions <- unique(df_coords$census_region)
print(regions)

successfull_overpasses <- c()
all_regions <- list()

for (reg in regions) {
  cat("Processing group (Census division/region):", reg, "\n")
  # Pick cities in this division (or region)
  # selected_cities <- df_coords %>% filter(division == reg) %>% pull(city)
  selected_cities <- df_coords %>% filter(census_region == reg) %>% pull(city)
  # If using regions instead, use: filter(census_region == reg)
  print(selected_cities)

  enh_list <- list()

  for (city in selected_cities) {
    # normalize naming to match your file convention
    city <- sub("^New York$", "NewYork", city)
    city <- sub("^Los Angeles$", "LosAngeles", city)
    city <- sub("^San Diego$", "SanDiego", city)

    message("city:: ", city, " | group: ", reg)

    default_cutoff <- '2015000000'
    strict_cutoff  <- '2015050000'

    matching_indices <- which(overpass_to_model$site %in% city)

    enh_folder  <- file.path(out.path, "EPC", city)
    sect_folder <- file.path(out.path, "SECTORS", city)

    files <- list.files(enh_folder, pattern = paste0("^", city, "_[0-9]+.*\\.txt$"),
                        full.names = FALSE)
    if (!length(files)) {
      message("  • No files found for city ", city)
      next
    }

    ts_from_files <- sub(paste0("^", city, "_([0-9]+).*"), "\\1", files)

    min_ts <- min(ts_from_files, na.rm = TRUE)
    city_cutoff <- if (min_ts >= strict_cutoff) strict_cutoff else default_cutoff
    message("City=", city, " → min_ts=", min_ts, " → cutoff=", city_cutoff)

    cluster_code <- urban_core$ID [urban_core$city == city]
    cluster_city <- cluster    [cluster$cluster_id == cluster_code, ] 
    cluster_pop  <- urban_core$core_pop[urban_core$city == city]
    cluster_lat  <- urban_core$lat     [urban_core$city == city]
    cluster_lon  <- urban_core$lon     [urban_core$city == city]

    message("Cluster code ", cluster_code, " | core pop ", cluster_pop)

    city_points <- SpatialPointsDataFrame(coords = cluster_city[,1:2], data = cluster_city[,1:2],
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

    for (ii in matching_indices) {

      timestr <- overpass_to_model$timestr[ii]

      if (timestr < city_cutoff || timestr %in% incomplete_runs$timestr) {
        message("  • Skipping (date range/failed): ", timestr)
        next
      }

      year <- substr(timestr, 1, 4)
      # print(pop_ratio_data[pop_ratio_data$city == city, ])
      city_df <- pop_ratio_data[pop_ratio_data$city == city, ]

      if (!shp_file) {
        enh_file   <- file.path(enh_folder,  paste0(city, "_", timestr, "_MC_brute.txt"))
        enh_readin <- t(read.table(enh_file, header = FALSE, sep = ",", skip = 1))
        colnames(enh_readin) <- enhancement_cols

        sect_file  <- file.path(sect_folder, paste0(city, "_", timestr, "_MC_brute.txt"))
        method_val <- "CC"
      } else {
        enh_file   <- file.path(enh_folder,  paste0(city, "_", timestr, "_MC_brute_shp.txt"))
        enh_readin <- t(read.table(enh_file, header = FALSE, sep = ",", skip = 1))
        colnames(enh_readin) <- enhancement_cols

        sect_file  <- file.path(sect_folder, paste0(city, "_", timestr, "_MC_brute_shp.txt"))
        method_val <- "Umich"
      }

      if (as.numeric(enh_readin[, "na_count"]) == 100000) {
        cat("  • Skipping due to na_count=100000 :: ", city, " :: ", timestr, "\n")
        na_counts[[length(na_counts) + 1]] <- timestr
        next
      } else {
        sect_readin <- t(read.table(sect_file, header = FALSE, sep = ",", skip = 1))
        colnames(sect_readin) <- sector_cols
      }

      enh_combined <- cbind(as.data.frame(enh_readin), as.data.frame(sect_readin))

      # 🔁 Swap to Census grouping
      # reg is the "division" label (or region if you flipped it above)
      enh_combined$region        <- reg                       # keep compatibility with plotting funcs
      enh_combined$division      <- df_coords$division     [df_coords$city == sub("([A-Za-z]+).*","\\1", city)][1]
      enh_combined$census_region <- df_coords$census_region[df_coords$city == sub("([A-Za-z]+).*","\\1", city)][1]

      enh_combined$pop_ratio <- city_df$pop_ratio[city_df$year == year]
      enh_combined$lat       <- cluster_lat
      enh_combined$lon       <- cluster_lon

      enh_list[[length(enh_list) + 1]] <- enh_combined
    } # ii
  } # city

  reg_df <- dplyr::bind_rows(enh_list)
  message("Rows in group '", reg, "' = ", nrow(reg_df), "\n")

  if (length(enh_list)) {
    all_regions[[reg]] <- reg_df
  } else {
    message("No valid rows for group ", reg)
  }
} # reg loop

enh_df <- dplyr::bind_rows(all_regions)
print(colnames(enh_df))
# MAIN DATAFRAME with parsed timestrs, i.e. seasons SON, DJF, MAM, JJA
enh_df <- parse_times(enh_df)
message("Rows of dataframe which have good data = ", nrow(enh_df))

# remove outliers beyond ±2 SD of median of Epc
enh_df <- remove_outliers(enh_df)
cat("After removing outliers, dim is ", dim(enh_df), "\n")

print(paste0("Final dim of the combined dataframe is ", paste(dim(enh_df), collapse=" x ")))

# seasonal profiles & light/dark split
enh_seasonal    <- seasonal_epc_us(enh_df)
enh_light_dark  <- light_dark_us(enh_df)

# Figures (these still consume 'region' as group label, now set to Division)
p_epc_period <- plot_epc_by_region_period(
  enh_df, method = "CC", value_col = "Epc", regions = regions
)

p_epc_ts     <- plot_yearly_epc_by_region(enh_df)
p_epc_season <- plot_epc_region_season(enh_df, value = "Epc", scale_multiplier = 1)

# epc vs lat / lon
p_epc_lat     <- plot_epc_vs_latitude(enh_df)
p_epc_lat_lon <- plot_epc_lat_lon(enh_df)
p_epc_pps =plot_epc_vs_pps(enh_df,save_path = "../figures/Epc_vs_pps_loglog.png")
p_epc_pps_periods=plot_epc_vs_pps_periods(enh_df,
                        save_path = "../figures/Epc_vs_pps_by_period.png")

# Print to PDF
print(p_epc_period)
print(p_epc_ts)
print(p_epc_season)
print(p_epc_lat)

print(p_epc_pps)
print(p_epc_pps_periods)
dev.off()


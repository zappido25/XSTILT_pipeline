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
library(raster)
library(sp)
# options(error = traceback)

## ploting and parsing functions are here
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
# print(functions_files)

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

# sector_cols <- c(
#   "CHE","ENE","IND","IRO","NEU","NFE","NMM","PRO","PRU_SOL","RCO",
#   "REF_TRF","TNR_Aviation_CDS","TNR_Aviation_CRS","TNR_Aviation_LTO",
#   "TNR_Other","TNR_Ship","TRO_noRES"
# )
sector_cols <- c("Chemical", "Power-Gen","manufra_Comb",
                 "Iron_Steel", "Non-Energy_fuels", "Non-Ferrous_Metals", "Non-metalllic_minerals",
                 "Fuel_exp","Solvents", "Residential_other_sectors","Oil_Ref",
                 "Avi-Climb/Descent","Avi-Cruise",
                 "Avi-Landing/Takeoff", "Non-road_Trans",
                 "Ships", "Road_Trans")

urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
path_pop="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
population_density_data_file<-paste0(path_pop,"gpw_v4_population_density_rev11_2020_30_sec.tif")

pop_file=read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/City_Population_By_Year.csv",
                    header = TRUE,stringsAsFactors = FALSE)
pop_ratio_data=read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/sanity_check_scripts/population_ratio_scaled_to_2020.csv",
                    header = TRUE,stringsAsFactors = FALSE)
colnames(pop_file) <- c("city", "year", "population")
# colnames(pop_file) <- c("city", "year", "population")
print(colnames(pop_ratio_data))

# interpolate for missing 2020 year
pop_interp <- pop_file %>%
  arrange(city, year) %>%
  group_by(city) %>%
  # Fill missing years with complete() then interpolate population
  complete(year = full_seq(year, 1)) %>%
  arrange(year) %>%
  mutate(
    population = zoo::na.approx(population, x = year, na.rm = FALSE),  # linear interpolation
    growth_abs = population - lag(population),
    growth_factor = 1+ ((population - lag(population)) / lag(population))
  ) %>%
  ungroup()

# population_density_data_file <-  "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
gpw_dens <- raster::brick(population_density_data_file)    
gpw_dens<-subset(gpw_dens, 1)


# Read in cities file and prompt user to select city
# Read the list of cities
# if not_us_areas, read from simulated_cities.txt
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")

cities <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]

pop_file=read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/City_Population_By_Year.csv",
                    header = TRUE,stringsAsFactors = FALSE)
                
  # Prompt for number of cities
cat("How many cities would you like to compare? ")
flush.console()
n_compare <- as.integer(readLines(file("stdin"), n = 1))
# Display available cities
cat("Available cities:\n")
for (i in seq_along(cities)) {
  cat(sprintf("%d: %s\n", i, cities[i]))
}

# cat("EHow many cities would you like to compare? ")
# city_idx <- as.integer(readLines(file("stdin"), n = 1))

if (is.na(n_compare) || n_compare < 1 || n_compare > length(cities)) {
  stop("Invalid number of cities.")
}

# Prompt for city indices
cat(sprintf("Enter %d city numbers separated by spaces: ", n_compare))
flush.console()
line <- readLines(file("stdin"), n = 1)         # waits for Enter
city_idx <- as.integer(strsplit(line, "\\s+")[[1]])

# validate
if (length(city_idx) != n_compare || any(is.na(city_idx))) {
  stop("Please enter exactly ", n_compare, " integers separated by spaces.")
}

# Validate
if (length(city_idx) != n_compare || any(city_idx < 1 | city_idx > length(cities))) {
  stop("Invalid city selection.")
}

# Get city names
selected_cities <- cities[city_idx]
cat("Selected cities:\n")
cat(paste0(" - ", selected_cities, collapse = "\n"), "\n")



enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                        "int_sd", "Epc", "int_epc_sd", "CI", "CI_sd", #"int_epc_gdp_mean" is carbon intensity
                          "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                        "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                        "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")



# save the results to a PDF in the folder "figures"
if (!shp_file){
  pdf_filename <- paste0("../figures/", "CC_cities_Seasonality.pdf")
}else{
  pdf_filename <- paste0("../figures/", "UMICH_cities_Seasonality.pdf")
}
# pdf_filename <- paste0("figures/", "cities_Seasonality.pdf")
pdf(pdf_filename, width = 8, height = 6)
print(pdf_filename)
# get enhancements and sectoral data folders


overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs_4analysis.txt"),
                header = TRUE, sep = "\t")  
na_counts=list()

# main loop start here
# saves the enhancements and sectoral data to a dataframe

for (city in selected_cities) {
    matching_indices <- which(overpass_to_model$site %in% city)

    enh_folder=file.path(out.path, "EPC", city)
    sect_folder=file.path(out.path, "SECTORS", city)
    

    cluster_code<-urban_core$ID[urban_core$city == city]
    cluster_city<-cluster[cluster$cluster_id == cluster_code, ] 
    cluster_pop=urban_core$core_pop[urban_core$city == city]
    print(paste0("Cluster code is ", cluster_code, " with core pop ", cluster_pop))

    city_points <- SpatialPointsDataFrame(coords = cluster_city[,1:2], data = cluster_city[,1:2],
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    #gpw city density
    gpw_dens_city<-pop.density(gpw_dens, cluster_city, city_points)
    # print(head(gpw_dens_city))
    # if your dataframe is named df
    gpw_total_pop <- sum(gpw_dens_city[[3]], na.rm = TRUE)
    print(paste0("GPW Total Population: ", gpw_total_pop))
    default_cutoff <- '2015000000'  # Default cutoff for most cities
    strict_cutoff <- '2015050000'  # strict cutoff for few cities

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
    
    for (ii in matching_indices) {

      timestr=overpass_to_model$timestr[ii]

    if (timestr < city_cutoff || timestr %in% incomplete_runs$timestr) {
                print("skipping runs outside date range or failed runs")
                print(paste0("CITY::", city))
                print(paste0("Timestr::", timestr))
                next
        }
      # city_df <- pop_interp[pop_interp$city == city, ]
      city_df <- pop_ratio_data[pop_ratio_data$city == city, ]
      print(city_df)
      year=substr(timestr, 1, 4) 
      # pop_dat<-paste0(path_pop,"usa_pop_",year,"_CN_1km_R2025A_UA_v1.tif")
      # world_pop=raster::brick(pop_dat)    
      # world_pop<-subset(world_pop, 1)

      # if (year!=2020) {
      #   worldpop_total_city <- pop_file$population[pop_file$city == city & pop_file$year == year]
      #   print(paste0("City Total Population: ", worldpop_total_city))
      #   # print(worldpop_total_city$population)
      #   print(gpw_total_pop)
      #   city_pop=as.numeric(worldpop_total_city)
      
      #   ratio_pop=city_pop/gpw_total_pop
      # } else{
      #   ratio_pop=1
      # }
      # ratio_pop=1
      # print(paste0("Ratio of core pop to WorldPop total pop in cluster: ", ratio_pop))
    
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
        cat("Skipping iteration due to city clustering na_count == 100000 for timestr:", timestr, "\n")
        na_counts[[length(na_counts) + 1]]  <- timestr
        next
      } else {
        print("here")
        print(timestr)
            sect_readin <- t(read.table(sect_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
            colnames(sect_readin) <- sector_cols
         
        }

      if (!exists("enh_list")) enh_list <- list()

#       # Combine enhancement and sectoral data by columns, then add to the lists
      enh_combined  <- cbind(as.data.frame(enh_readin),  as.data.frame(sect_readin))
      enh_combined$ratio_pop <- city_df$pop_ratio[city_df$year==year] #ratio_pop
      enh_list[[length(enh_list) + 1]]  <- enh_combined
    }
}

  # # Final per-method dataframes

enh_df  <- dplyr::bind_rows(enh_list)


# # check nrows of good data
message( "Rows of dataframe which have good data = ", nrow(enh_df))

# # MAIN DATAFRAME with pasrsed timestrs, i.e categorizing it in seasons, SON, DJF, MAM, JJA
enh_df  <- parse_times(enh_df)

# ## add method to the dataframe. not useful now since we are only processing one method at a time
enh_df  <- enh_df  %>% mutate(method = "CC")

# remove outliers based on Kai's method -> beyond ±2 SD of median of Epc
enh_df  <- remove_outliers(enh_df)
# print(enh_df)
# stop("checkpoint")

# df_season <- enh_df %>%
#   mutate(
#     half = case_when(
#       month %in% 3:8  ~ "SS (Mar–Aug)",
#       month %in% c(9:12, 1:2) ~ "FW (Sep–Feb)"
#     )
#   )


# get seasonal profiles
enh_seasonal  <- seasonal_epc(enh_df,  "CC")

# # plot the seasonal means with error bars
p1_seasonal=plot_seasonality_city(enh_seasonal)
p_timeseries <- plot_epc_timeseries_city(enh_df)
p_epc_timeseries_city_pop <- plot_epc_timeseries_city_pop(enh_df)

# 2015-2019, 2020 and 2021-2024
p_epc_years=plot_epc_groups_cities(enh_df)

# plot carbon intensity
p_ci= plot_carbon_intensity_city(enh_df)
p_ci_box= plot_carbon_intensity_box_plot_city(enh_df)
p_ci_yearly_grouped= plot_carbon_intensity_grouped_yearly_city(enh_df)

# popluation ratio
p_pop_ratio=plot_ratio_population_step(enh_df)
# print(enh_df$ratio_pop)

# plot epc winter summer
p_winter_summer_timeseries=plot_season_trends_light_dark_city(enh_df)

##  plot timeseries for temporal analysis 
#  p_trend_timeseries <- trend_timeseries(epc_combined)

# check trends of Epc  use mann-kendall test
trend_cc  <- check_trend(enh_df,  method_val)
mk_val  <- mk_test(enh_df,  method_val)

print(mk_val)

# # create epc_monthly
# # Ensure 'month' column exists by extracting it from 'timestr' (or appropriate date column)
cc_monthly <- enh_df %>%
  mutate(month = as.integer(substr(timestr, 5, 6)))  # Adjust 'timestr' if needed
# print(cc_monthly)

monthly_summary <- cc_monthly %>%
      group_by(city, month) %>%
      summarise(
        mean_epc = mean(as.numeric(Epc), na.rm = TRUE),
        median_epc = median(as.numeric(Epc), na.rm = TRUE),
        sd_epc = sd(as.numeric(Epc), na.rm = TRUE),
        n = n(),
        .groups = "drop"
      )

  # print(monthly_summary)    
p_monthly_profiles=plot_monthly_profiles_city(monthly_summary)

# ### ADD PLOTS TO THE FILE
print(p1_seasonal)
print(p_epc_years)
print(p_monthly_profiles)
print(p_timeseries)
print(p_winter_summer_timeseries)
# print(p_pop_ratio)
print(p_epc_timeseries_city_pop)


# print(p_ci)
# print(p_ci_box)
# print(p_ci_yearly_grouped)




# ##### lets shift our focus to the sectoral analysis.
# # ------------------------------------------------------------------------------
# # This script processes emission percentage contribution (EPC) data by sector.
# #
# # Steps:
# # 1. Converts sector percentage columns to fractions.
# # 2. Calculates the absolute sectoral EPC values by multiplying the fractions
# #    with the mean integrated EPC value (`Epc`).
# # 3. Selects relevant columns and reshapes the data from wide to long format,
# #    resulting in one row per city, method, year, season, and sector.
# # 4. Cleans up sector names by removing the "pc_" prefix.
# #
# # Input:
# #   - epc_combined: Data frame containing EPC data, including sector columns,
# #     city, method, year, season, and `Epc`.
# #   - sector_cols: Character vector of sector column names.
# #
# # Output:
# #   - sector_long: Data frame in long format with columns:
# #       city, method, year, season, sector, epc_value
# # ------------------------------------------------------------------------------

df= enh_df

df_pc <-df %>%
  mutate(across(all_of(sector_cols), ~ as.numeric(.)/100)) %>%  # convert % to fractions
  mutate(across(all_of(sector_cols), ~ .x * as.numeric(Epc), .names = "pc_{.col}"))

# # print(colnames(df_pc))
pc_cols <- grep("^pc_", names(df_pc), value = TRUE)

sector_long <- df_pc %>%
  dplyr::select(city,method, year, season, all_of(pc_cols)) %>%
  pivot_longer(cols = all_of(pc_cols), names_to = "sector", values_to = "epc_value") %>%
  mutate(sector = sub("^pc_", "", sector))  


# # ### get yearly sectoral contributions
sector_yearly <- sector_long %>%
  group_by(city, year, sector) %>%
  summarise(
    epc_mean = mean(epc_value, na.rm = TRUE),
    epc_sd   = sd(epc_value, na.rm = TRUE),
    n        = sum(!is.na(epc_value)),
    .groups = "drop"
  )

# # p_sectors_yearly=plot_sectoral_profiles_yearly_timeseries(sector_yearly)
# # p_sectors_change_yearly=plot_sectoral_change_yearly(sector_yearly)
# # p_sectors_stacked=plot_sectoral_stacked_area(sector_yearly)
# # p_sectors_normalized=plot_normalized_contributions(sector_yearly)
# # p_sectors_normalized_100=plot_normalized_contributions_100_percent_stacked_bar(sector_yearly)
# # p_sectors_normalized_split=plot_normalized_contributions_split(sector_yearly)
# # p_sectors_normalized_filtered=plot_normalized_contributions_filtered(sector_yearly)

# p_sectors_greater_pct10=plot_sectors_greater_pct10_city(sector_yearly)
p_seasonal_rel=seasonality_sectors_city(df_pc)
p_seasonal_abs=seasonality_sectors_city_absolute_values(df_pc)

# print(p_sectors_yearly)
# print(p_sectors_change_yearly)
# print(p_sectors_stacked)
# print(p_sectors_normalized)
# print(p_sectors_normalized_100)
# print(p_sectors_normalized_split)
# print(p_sectors_normalized_filtered)
# print(p_sectors_greater_pct10)
# print(p_seasonal_rel)
# print(p_seasonal_abs)
dev.off()
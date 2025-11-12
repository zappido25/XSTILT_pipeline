# -- core processing pipeline --
# uses the sectoral and enhancements data to process the seasonality of the emissions per capita and sectoral 
# contributions to these emissions.

library(ggplot2)
library(dplyr)



group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
OCO.DIR = Sys.getenv("OCO2_DIR")



# Read in cities file and prompt user to select city
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
cities <- read.table(cities_file, header = FALSE, skip=0, stringsAsFactors = FALSE)[,1]


enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                        "int_sd", "int_epc_mean", "int_epc_sd", "int_epc_gdp_mean", "int_epc_gdp_sd",
                          "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                        "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                        "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")



# # save the results to a PDF in the folder "figures"
# pdf_filename_emi_conv <- paste0("../figures/", city, "_Seasonality.pdf")
# pdf(pdf_filename_emi_conv, width = 8, height = 6)

# get enhancements and sectoral data folders


overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  

na_counts=list()
# main loop start here
# saves the enhancements and sectoral data to a dataframe

for (ss in cities) {
    count=0
    enh_folder=file.path(out.path, "ENHANCEMENTS", ss)
    sect_folder=file.path(out.path, "SECTORAL", ss)
    matching_indices <- which(overpass_to_model$site %in% ss)

    for (ii in matching_indices) {

      timestr=overpass_to_model$timestr[ii]

      if (timestr < '2015050000' || timestr %in% incomplete_runs$timestr) {
              print("skipping runs outside date range or failed runs")
              print(paste0("CITY::", ss))
              print(paste0("Timestr::", timestr))
              next
      }
  
      
      enh_cc_file <- file.path(enh_folder, paste0(ss, "_", timestr, "_MC_brute.txt"))

         # Read the data from the files, skipping the first row and transposing
      
      enh_cc_readin <- t(read.table(enh_cc_file,header = FALSE, sep = ",", skip = 1) ) # transpose the table
      
     
        # print("Using other enhancement columns")
      colnames(enh_cc_readin) <- enhancement_cols
 
  
    
      # Check if na_count is 100000 in either file
      # indicates that the enhamcements and sectorla data is empty or filled with NaNs
      if (as.numeric(enh_cc_readin[,"na_count"]) == 100000 ) {
        cat("Skipping iteration due to city clustering na_count == 100000 for timestr:", timestr, "\n")
        na_counts[[length(na_counts) + 1]]  <- timestr
        next
      } else {
        count=count+1
       
        # }
      }
  
    }
    if (!exists("overpass_list")) overpass_list <- list()
    print(paste0("Total successful overpasses for city ", ss, ": ", count))
      city_overpasses <- cbind(ss, count)
      overpass_list[[length(overpass_list) + 1]] <- city_overpasses
}

# Convert overpass_list (a list of c(city, count)) into a data.frame
if (!exists("overpass_list") || length(overpass_list) == 0) {
  overpass_list <- data.frame(city = character(0), count = integer(0), stringsAsFactors = FALSE)
} else {
  overpass_list <- do.call(rbind, overpass_list)
  overpass_list <- as.data.frame(overpass_list, stringsAsFactors = FALSE)
  names(overpass_list) <- c("city", "count")
  overpass_list$count <- as.integer(overpass_list$count)
}
print(overpass_list)

cities_file_all <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")

cities_all <- read.table(cities_file_all, header = FALSE, skip = 1, stringsAsFactors = FALSE)[, 1]


plot.us.maps_all_cities_overpasses <- function(
  cities_subset, cities_all, overpasses_list,
  plot_name = "../figures/conus_cities_overpasses.png"
) {
  library(ggplot2)
  library(dplyr)
  library(tidygeocoder)
  library(maps)
  library(ggrepel)
  library(tibble)

  # --- Normalize city names ---
  fix_city_names <- function(x) {
    x <- sub("^NewYork$", "New York", x)
    x <- sub("^LosAngeles$", "Los Angeles", x)
    x <- sub("^SanDiego$", "San Diego", x)
    x <- sub("^Eastern_Florida$", "Eastern Florida", x)
    x <- sub("^Dallas_FortWorth$", "Dallas-Fort Worth", x)
    x <- sub("^SanFransico_SanJose$", "San Francisco - San Jose", x)
    x <- sub("^LasVegas$", "Las Vegas", x)
    x <- sub("^SanAntonio$", "San Antonio", x)
    x <- sub("^SaltLakeCity$", "Salt Lake City", x)
    x
  }

  cities_all    <- fix_city_names(cities_all)
  cities_subset <- fix_city_names(cities_subset)
  common_cities <- intersect(cities_all, cities_subset)

  # --- Manual fallback coordinates for key cities ---
  manual_coords <- tribble(
    ~city,                ~lon,       ~lat,
    "New York",           -74.0060,    40.7128,
    "Los Angeles",        -118.2437,   34.0522,
    "San Diego",          -117.1611,   32.7157,
    "Dallas-Fort Worth",  -97.0403,    32.8998,
    "San Francisco - San Jose", -122.4194, 37.7749,
    "Salt Lake City",     -111.8910,   40.7608,
    "Las Vegas",          -115.1398,   36.1699,
    "San Antonio",        -98.4936,    29.4241,
    "Phoenix",            -112.0740,   33.4484,
    "Portland",           -122.6765,   45.5231
  )

  # --- Geocode all cities (with fallback) ---
  df_all <- tibble(city = cities_all, query = paste0(city, ", USA")) %>%
    tidygeocoder::geocode(address = "query", method = "osm",
                          lat = "lat", long = "lon", limit = 1,
                          custom_query = list(countrycodes = "us"))

  # Patch with manual coordinates
  df_all <- df_all %>%
    dplyr::rows_patch(manual_coords, by = "city", unmatched = "ignore")

  # Ensure New York always exists
  if (!"New York" %in% df_all$city) {
    df_all <- bind_rows(df_all, tibble(city = "New York", lon = -74.0060, lat = 40.7128))
  } else {
    df_all <- df_all %>%
      mutate(
        lon = ifelse(city == "New York", -74.0060, lon),
        lat = ifelse(city == "New York",  40.7128, lat)
      )
  }

  # --- Robust merge of overpass counts ---
  overpasses_df <- overpasses_list %>%
    mutate(city_clean = tolower(gsub("\\s+", "", city))) %>%
    rename(count = count)

  df_all <- df_all %>%
    mutate(city_clean = tolower(gsub("\\s+", "", city))) %>%
    left_join(overpasses_df %>% select(city_clean, count), by = "city_clean")

  df_all$count[is.na(df_all$count)] <- 0

  # --- Filter & highlight simulated cities ---
  df_all <- df_all %>%
    filter(!is.na(lat), !is.na(lon)) %>%
    filter(between(lon, -125, -66), between(lat, 24, 50)) %>%
    mutate(color = ifelse(city %in% common_cities, "red", "grey50"))

  # Force "New York" red even if missed
  df_all <- df_all %>%
    mutate(color = ifelse(grepl("new.?york", city, ignore.case = TRUE), "red", color))

  # --- US basemap ---
  us_map <- map_data("state") %>%
    filter(!region %in% c("alaska", "hawaii"))

  # --- Plot ---
  p <- ggplot() +
    geom_polygon(
      data = us_map,
      aes(x = long, y = lat, group = group),
      fill = "grey95", color = "white"
    ) +
    geom_point(
      data = df_all,
      aes(x = lon, y = lat, color = color, size = count),
      alpha = 0.85
    ) +
    ggrepel::geom_text_repel(
      data = df_all %>% filter(color == "red"),
      aes(x = lon, y = lat, label = paste0(city, "\n(", count, ")")),
      size = 3.2, color = "red",
      seed = 42, box.padding = 0.3,
      min.segment.length = 0,
      max.overlaps = Inf
    ) +
    scale_color_identity() +
    scale_size_continuous(range = c(2, 5)) +
    coord_quickmap(xlim = c(-125, -66), ylim = c(24, 50)) +
    labs(
      title = "U.S. Cities: Simulated (Red) vs Planned (Grey)",
      subtitle = "Number of OCO-2 overpasses shown in parentheses",
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "none"
    )

  # --- Save ---
  ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
  message("✅ Saved map to: ", plot_name)

  invisible(p)
}


p_usa_overpasses=plot.us.maps_all_cities_overpasses(cities, cities_all, overpass_list)

# Final per-method dataframes

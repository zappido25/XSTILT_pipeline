library(ggplot2)
library(sp)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidygeocoder)
library(sf)
library(maps)
library(raster)
library(zoo)
library(tidyr)

# --- Helper function ---------------------------------------------------------
adjust_population <- function(city_df, gpw_2020){
  
  # Ensure complete years and interpolate missing (including 2020)
  city_df <- city_df %>%
    arrange(city, year) %>%
    group_by(city) %>%
    complete(year = full_seq(year, 1)) %>%
    arrange(year) %>%
    mutate(
      population = zoo::na.approx(population, x = year, na.rm = FALSE)
    ) %>%
    ungroup()
  
  # 2020 must exist now
  if(!2020 %in% city_df$year){
    stop(paste("Interpolation failed — no 2020 row for", unique(city_df$city)))
  }
  
  # Interpolated Census 2020
  P2020_interp <- city_df$population[city_df$year == 2020]
  
  # Bias factor
  beta <- gpw_2020 / P2020_interp
  print(beta)
  city_df <- city_df %>%
    mutate(
      pop_adj = population * beta,     # GPW anchored Census trend
      pop_ratio = pop_adj / gpw_2020   # r_i = P_i / P_2020 (keep for EPC later)
    )
  
  return(city_df)
}

# -----------------------------------------------------------------------------
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/',
                              full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))

# cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]

pdf_filename <- paste0("sanity_check_figures/", "population_ratio_scaled.pdf")
pdf(pdf_filename, width = 8, height = 6)
print(pdf_filename)

pop_file <- read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/City_Population_By_Year.csv",
                     header = TRUE,stringsAsFactors = FALSE)
urban_core <- read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt",
                         header = TRUE, stringsAsFactors = FALSE)
cluster <- readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
path_pop <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
population_density_data_file <- paste0(path_pop, "gpw_v4_population_density_rev11_2020_30_sec.tif")

gpw_dens <- raster::brick(population_density_data_file)
gpw_dens <- subset(gpw_dens, 1)

results_list <- list()

for (city in cities) {
  message("Processing city: ", city)
  
  city_df <- pop_file[pop_file$city == city, ]
  
  cluster_code <- urban_core$ID[urban_core$city == city]
  cluster_city <- cluster[cluster$cluster_id == cluster_code, ]
  
  city_points <- SpatialPointsDataFrame(coords = cluster_city[,1:2],
                                        data = cluster_city[,1:2],
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
  
  gpw_dens_city <- pop.density(gpw_dens, cluster_city, city_points)
  gpw_total_pop <- sum(gpw_dens_city[[3]], na.rm = TRUE)
  
  print(paste0("GPW Total Population: ", gpw_total_pop))
  adj_city_df <- adjust_population(city_df, gpw_total_pop)
  results_list[[city]] <- adj_city_df
  
  # --- Updated plot with pop_adj overlay ---
  p <- ggplot() +
    
    # Original Census data (red points, blue line)
    geom_line(data = city_df, aes(x = year, y = population, color = "Census Original"), size = 1) +
    geom_point(data = city_df, aes(x = year, y = population, color = "Census Original"), size = 2) +
    
    # GPW anchored adjusted population
    geom_line(data = adj_city_df, aes(x = year, y = pop_adj, color = "GPW-Anchored Adj"), linewidth = 1.1, linetype = "dashed") +
    
    # GPW 2020 reference only
    geom_point(aes(x = 2020, y = gpw_total_pop, color = "GPW 2020"), size = 4, shape = 17) +
    
    scale_color_manual(values = c(
      "Census Original" = "blue",
      "GPW-Anchored Adj" = "darkorange",
      "GPW 2020" = "black"
    )) +
    
    ggtitle(paste("Population Trend for", city)) +
    xlab("Year") + ylab("Population") +
    scale_x_continuous(
      breaks = seq(min(adj_city_df$year), max(adj_city_df$year), by = 1),
      labels = as.integer
    ) +
    theme_minimal(base_size = 13) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "bottom",
          legend.title = element_blank())
  
  print(p)
}

dev.off()

all_results <- bind_rows(results_list, .id = "city")
write.csv(all_results, "population_ratio_scaled_to_2020.csv", row.names = FALSE)

print("✅ Done! Output written to sanity_check_figures/population_ratio_scaled.pdf and population_ratio_outputs.csv")
# =====================================================================
# OCO-2 Overpass Filtering by Latitude Coverage (Figure 2.8 Equivalent)
# =====================================================================

library(ggplot2)
library(dplyr)
library(ggmap)
library(raster)
library(sf)
library(ggnewscale)
library(patchwork)
library(tidyr)
library(ncdf4)
library(ggridges)
library(ggrepel)

# --------------------------
# Source helper functions
# --------------------------
functions_files <- list.files(
  '/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/',
  full.names = TRUE, pattern = "\\.r$"
)
invisible(lapply(functions_files, source))
print(functions_files)

# --------------------------
# Setup + Inputs
# --------------------------
cities_file <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list_remaining.txt"
cities <- read.table(cities_file, header = FALSE, stringsAsFactors = FALSE)[,1]

OCO.DIR <- Sys.getenv("OCO2_DIR")
homedir <- Sys.getenv("XSTILT_PATH")
oco.ver  <- 'V11r'

urban_core <- read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt", 
                         header = TRUE, stringsAsFactors = FALSE)

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                                header = TRUE, sep = ",")

dir.create("overpasses_plots", showWarnings = FALSE)

# --------------------------
# Constants
# --------------------------
half_width  <- 1.0
half_height <- 1.0

# --------------------------
# Loop over each city
# --------------------------
all_city_filtered <- data.frame()

for (city in cities) {
  message("\n-------------------------------------")
  message(" Processing city: ", city)
  message("-------------------------------------")

  matching_indices <- which(overpass_to_model$site %in% city)
  if (length(matching_indices) == 0) {
    message("⚠ No matching overpasses found for ", city)
    next
  }

  center_lon <- urban_core$lon[urban_core$city == city]
  center_lat <- urban_core$lat[urban_core$city == city]

  bbox <- c(
    left   = center_lon - half_width,
    bottom = center_lat - half_height,
    right  = center_lon + half_width,
    top    = center_lat + half_height
  )

  # one df for all xco2 values
  all_XCO2 <- data.frame()

  for (ii in matching_indices) {
    timestr <- overpass_to_model$timestr[ii]
    xco2_fname <- paste0(city, "_", timestr, ".txt")
    file_path <- file.path(OCO.DIR, "OCO-2/overpass_obs", xco2_fname)

    if (!file.exists(file_path)) {
      message("Missing XCO2 file: ", file_path)
      next
    }

    XCO2_overpass <- read.table(file_path, header = TRUE)
    XCO2_overpass$timestr <- timestr
    all_XCO2 <- rbind(all_XCO2, XCO2_overpass)
  }

  if (nrow(all_XCO2) == 0) {
    message("⚠ No valid XCO2 data found for ", city)
    next
  }

  # --------------------------------------------------------
  # ✅ Latitude coverage filtering
  # --------------------------------------------------------
  lat_min <- min(all_XCO2$lat, na.rm = TRUE)
  lat_max <- max(all_XCO2$lat, na.rm = TRUE)
  lat_bins <- seq(lat_min, lat_max, by = 0.05)
  print(paste0("latmin: ",lat_min, ", lat max: ", lat_max))
  
  bin_counts <- all_XCO2 %>%
    mutate(lat_bin = cut(lat, breaks = lat_bins, include.lowest = TRUE)) %>%
    group_by(timestr) %>%
    summarise(n_bins = n_distinct(lat_bin, na.rm = TRUE)) %>%
    ungroup()

  if (nrow(bin_counts) == 0) next

  p10  <- quantile(bin_counts$n_bins, 0.10, na.rm = TRUE)
  p15  <- quantile(bin_counts$n_bins, 0.15, na.rm = TRUE)
  p20  <- quantile(bin_counts$n_bins, 0.20, na.rm = TRUE)
  mean_bins <- mean(bin_counts$n_bins, na.rm = TRUE)

  valid_overpasses <- bin_counts %>%
    filter(n_bins >= p10) %>%
    pull(timestr)

  all_XCO2_filtered <- all_XCO2 %>%
    filter(timestr %in% valid_overpasses)

  message(city, ": retained ", length(unique(all_XCO2_filtered$timestr)),
          " of ", length(unique(all_XCO2$timestr)), " overpasses after filtering")

  # --------------------------------------------------------
  # ✅ Histogram plot (Figure 2.8 style)
  # --------------------------------------------------------
  # pdf(file.path("overpasses_plots", paste0(city, "_lat_coverage_hist.pdf")),
  #     width = 7, height = 5)
    # --------------------------------------------------------
  # ✅ Histogram plot (Figure 2.8 style, fixed)
  # --------------------------------------------------------
    # --------------------------------------------------------
  # ✅ Histogram plot (Figure 2.8 style with legend)
  # --------------------------------------------------------
  png_out <- file.path("overpasses_plots", paste0(city, "_lat_coverage_hist.png"))

  # Create data frame for reference lines + legend
  ref_lines <- data.frame(
    xintercept = c(p10, p15, p20, mean_bins),
    label = c("10th percentile", "15th percentile", "20th percentile", "Mean"),
    color = c("red", "blue", "orange", "black")
  )

  p <- ggplot(bin_counts, aes(x = n_bins)) +
    geom_histogram(
      binwidth = 2,
      fill = "darkseagreen3",
      color = "black",
      alpha = 0.8
    ) +
    geom_density(
      aes(y = after_stat(count * max(bin_counts$n_bins) / max(count))),
      color = "darkgreen",
      linewidth = 1
    ) +
    geom_vline(
      data = ref_lines,
      aes(xintercept = xintercept, color = label),
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_color_manual(
      name = "Reference Lines",
      values = c(
        "10th percentile" = "red",
        "15th percentile" = "blue",
        "20th percentile" = "orange",
        "Mean" = "black"
      )
    ) +
    labs(
      title = paste0("Latitude Bin Coverage – ", city),
      x = "Number of Unique Latitude Bins",
      y = "Frequency (# Overpasses)"
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      legend.background = element_rect(fill = "white", color = "gray80"),
      plot.title = element_text(face = "bold", size = 14)
    )

  ggsave(
    filename = png_out,
    plot = p,
    width = 8,
    height = 6,
    dpi = 300,
    bg = "white"
  )

  # --------------------------------------------------------
  # ✅ Save filtered overpasses for this city
  # --------------------------------------------------------
  city_overpasses <- overpass_to_model %>%
    filter(site == city)

  city_overpasses_filtered <- city_overpasses %>%
    filter(timestr %in% valid_overpasses)

  all_city_filtered <- bind_rows(all_city_filtered, city_overpasses_filtered)
  if (city=="SaltLakeCity") {
    all_city_filtered <- all_city_filtered %>%
    filter(timestr %in% valid_overpasses)

  }

  write.table(city_overpasses_filtered,
              file = file.path("overpass_filtered_files",
                               paste0(city, "_overpass_filtered.txt")),
              sep = ",", quote = FALSE, row.names = FALSE)

  message("✅ Saved filtered overpass list for ", city)
}

# ============================================================
# ✅ Combine and save global filtered overpass file
# ============================================================
if (nrow(all_city_filtered) > 0) {
  out_path <- file.path(OCO.DIR, "OCO-2/overpass_city/overpass_to_model_filtered.txt")
  write.table(all_city_filtered, file = out_path,
              sep = ",", quote = FALSE, row.names = FALSE)
  message("\n🎯 Wrote combined filtered file → ", out_path)
} else {
  warning("⚠ No filtered overpasses found for any city")
}
library(grid)
library(gridExtra)
library(raster)
library(ncdf4)
library(terra)
library(sf)
library(rslurm)

get_urban_extent <- function(city_name) {
  print("Getting urban extent for ODIAC-city...")
  print(city_name)
  
  
  # Load the urban area US Census shapefile 
  shapefile_path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/data/Urban_Shapefiles/tl_2024_us_uac20.shp"
  gdf <- st_read(shapefile_path)
  
  
  # Find the matching city in the NAME20 attribute
  
  grepl_result <- grepl(city_name, gdf$NAME20, ignore.case = TRUE)
  gdf_city <- gdf[grepl_result, ]
  
  print(paste("Used:", gdf_city[1, ]$NAME20))
  print("All Options:")
  print(gdf_city)
  
  if (nrow(gdf_city) == 0) {
    stop(paste("City not found in the shapefile:", city_name))
  }
  
  # If multiple matches, take the first one (you can modify this logic as needed)
  bbox <- st_bbox(gdf_city[1, ])
  
  # Return the bounding box
  return(list(
    lat = c(bbox$ymin, bbox$ymax),
    lon = c(bbox$xmin, bbox$xmax)
  ))
}


# Function to extract year and month from the directory name
extract_year_month <- function(base_dir) {
  # Extract year and month from the directory name (assumes format "out_YYYYMMDD")
  matches <- regexpr("\\d{8}", basename(base_dir))  # Match the first 8 digits (YYYYMMDD)
  year_month <- substr(basename(base_dir), start = regexpr("\\d{8}", basename(base_dir)), 
                       stop = regexpr("\\d{8}", basename(base_dir)) + 7)
  year <- substr(year_month, 1, 4)
  month <- substr(year_month, 5, 6)
  return(list(year = as.integer(year), month = sprintf("%02d", as.integer(month))))
}

# Define the base directory
base_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/Phoenix/V11r/"

# Get all directories starting with "out_"
dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
dirs <- dirs[grepl("out_", dirs)]  # Filter directories starting with "out_"

# Loop through each directory
for (dir in dirs) {
  # Extract the year and month 
  year_month <- extract_year_month(dir)
  print(year_month)
  
  # Construct the file name using the extracted year and month
  csv_file <- file.path(dir, paste0("xco2_data_region_", year_month$year, year_month$month, ".csv"))
  print(csv_file)
  
  # Check if xco2_data.csv exists
  if (!file.exists(csv_file)) {
    warning(paste("xco2_data.csv not found in:", dir))
    next
  }
  
  # Read 
  extracted_data <- read.csv(csv_file)
  
  
  # Check for required columns
  required_columns <- c("lati", "XCO2", "smurf_co2", "ODIAC_conv")
  missing_columns <- setdiff(required_columns, colnames(extracted_data))
  
  if (length(missing_columns) > 0) {
    message(paste("Skipping:", dir, "- missing columns:", paste(missing_columns, collapse = ", ")))
    next
  }
  
  # Check if extracted_data$ODIAC_conv is a data frame or list
  if (is.data.frame(extracted_data$ODIAC_conv) || is.list(extracted_data$ODIAC_conv)) {
    # If it is a data frame or list, extract the emissvector
    extracted_data$ODIAC_conv <- extracted_data$ODIAC_conv$emissvector
  } else {
    # If it is not a data frame or list, print a message or handle the case
    message("ODIAC_conv is not a data frame or list, skipping extraction of emissvector.")
  }
  
  city_name <- basename(dirname(base_dir))
  urban_extent <- get_urban_extent(city_name)
  
  
  background_region <- extracted_data$Background == "Yes"
  urban_region <- extracted_data$Urban == "Yes"
  
  
  # Calculate averages for the background region
  avg_smurf_co2_bg <- mean(extracted_data$smurf_co2[background_region], na.rm = TRUE)
  avg_odiac_bg <- mean(extracted_data$ODIAC_conv[background_region], na.rm = TRUE)
  avg_xco2_bg <- mean(extracted_data$XCO2[background_region], na.rm = TRUE)
  
  # Calculate averages for the urban region
  avg_smurf_co2_urban <- mean(extracted_data$smurf_co2[urban_region], na.rm = TRUE)
  avg_odiac_urban <- mean(extracted_data$ODIAC_conv[urban_region], na.rm = TRUE)
  avg_xco2_urban <- mean(extracted_data$XCO2[urban_region], na.rm = TRUE)
  
  # AUC calculation for CO2 types
  library(pracma)
  
  
  auc_smurf_co2 <- trapz(extracted_data$lati, extracted_data$smurf_co2)
  auc_odiac_conv <- trapz(extracted_data$lati, extracted_data$ODIAC_conv)
  
  
  # Create the plot
  library(ggplot2)
  
  # Subtract background XCO2 from XCO2 values
  extracted_data$XCO2_adj <- extracted_data$XCO2 - avg_xco2_bg
  
  # Define size of latitude bin
  bin_size <- 0.05
  
  # Aggregate data into 0.05-degree latitude bins
  aggregated_data <- extracted_data %>%
    mutate(lati_bin = floor(lati / bin_size) * bin_size) %>%
    group_by(lati_bin) %>%
    summarise(
      avg_XCO2 = mean(XCO2_adj, na.rm = TRUE)
    ) %>%
    ungroup()

  
  plot1 <- ggplot(extracted_data, aes(x = lati)) +
    # Regular observations as circles
    geom_point(aes(y = XCO2_adj, color = "Delta CO2 (Obs-bg)", 
                   shape = "Delta CO2 (Obs-bg)"), 
               size = 3, show.legend = TRUE) +
    
    # Urban-enhanced points as red circles
    geom_point(data = extracted_data %>% filter(Urban == "Yes"), 
               aes(x = lati, y = XCO2_adj, color = "Delta CO2 (Obs-bg, urban-enhanced region)", 
                   shape = "Delta CO2 (Obs-bg, urban-enhanced region)"), 
               size = 3, alpha = 0.4, show.legend = TRUE) +  
    
    geom_point(data = extracted_data %>% filter(Background == "Yes"), 
               aes(x = lati, y = XCO2_adj, color = "Delta CO2 (Obs-bg, background region)", 
                   shape = "Delta CO2 (Obs-bg, background region)"), 
               size = 3, alpha = 0.4, show.legend = TRUE) +
    
    # Line plots for other CO2 components
    geom_line(aes(y = smurf_co2, color = "Delta CO2 - Bio (SMUrF)"), linewidth = 1) +
    geom_line(aes(y = `ODIAC_conv`, color = "Delta CO2 - Anthro (ODIAC)"), linewidth = 1) +
    
    # Black triangles for average XCO2 values in each latitude bin
    geom_point(data = aggregated_data, 
               aes(x = lati_bin, y = avg_XCO2, shape = "Average 0.05 deg bin Delta CO2 (Obs-bg)"), 
               color = "black", size = 4) +
    
    # Urban region shaded area
    annotate("rect", xmin = urban_extent$lat[1], xmax = urban_extent$lat[2], 
             ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) +
    
    annotate("text", x = ((urban_extent$lat[1]+urban_extent$lat[2])/2), y = min(extracted_data$XCO2_adj), 
             label = "US Census urban region", color = "red", size = 5) +
    
    # Color scale
    scale_color_manual(values = c(
      "Delta CO2 - Bio (SMUrF)" = "seagreen3", 
      "Delta CO2 - Anthro (ODIAC)" = "indianred", 
      "Delta CO2 (Obs-bg)" = "grey",
      "Delta CO2 (Obs-bg, urban-enhanced region)" = "red",
      "Delta CO2 (Obs-bg, background region)" = "blue"
    )) +
    
    # Shape scale: keep circles for both regular and urban points
    scale_shape_manual(values = c(
      "Delta CO2 (Obs-bg)" = 16,  
      "Average 0.05 deg bin Delta CO2 (Obs-bg)" = 17, 
      "Delta CO2 (Obs-bg, urban-enhanced region)" = 16,
      "Delta CO2 (Obs-bg, background region)" = 16
    )) +  
    
    guides(
      color = guide_legend(override.aes = list(shape = 16)),  
      guides(
        color = guide_legend(override.aes = list(shape = c(16, 16, 16, 16, NA))),  
        shape = guide_legend(override.aes = list(color = "black"))  # Keep shape legend for bin averages
      )
    ) +
    
    labs(x = "Latitude", y = "Delta CO2 (ppm)", title = "CO2 vs Latitude", 
         color = "Legend", shape = "Legend") +  
    theme_minimal(base_size = 14)
  
  
  # Create table for the averages
  library(gridExtra)
  
  
  avg_table1 <- data.frame(
    Region= c("Urban", "Background", "Urban-Background Gradient"),
    "Average XCO2 Obs (ppm)" = c(avg_xco2_urban, avg_xco2_bg,(avg_xco2_urban - avg_xco2_bg)),
    "Average Delta CO2 Anthro (ppm)" = c(avg_odiac_urban, avg_odiac_bg, (avg_odiac_urban - avg_odiac_bg)),
    "Average Delta CO2 Bio (ppm)"= c(avg_smurf_co2_urban, avg_smurf_co2_bg, (avg_smurf_co2_urban-avg_smurf_co2_bg)),
    check.names=FALSE
  )
  
  # New Table: Signal/Noise Ratio
  signal_noise_table <- data.frame(
    Region = "Urban-Background Gradient",
    "Average Delta CO2 Anthro/Bio (Signal/Noise Ratio) (ppm)" = (avg_odiac_urban - avg_odiac_bg) / (avg_smurf_co2_urban - avg_smurf_co2_bg),
    check.names=FALSE
  )
  
  
  auc_table <- data.frame(
    "Delta CO2" = c("Anthro", "Bio", "Anthro/Bio (Signal/Noise Ratio)"),
    "Area Under Curve (ppm*deg)" = c(auc_odiac_conv, auc_smurf_co2, (auc_odiac_conv/auc_smurf_co2)),
    
    check.names= FALSE
  )
  
  # Create the table grobs
  table_grob1 <- tableGrob(avg_table1, rows = NULL, theme = ttheme_default(base_size = 12))
  signal_noise_grob <- tableGrob(signal_noise_table, rows = NULL, theme = ttheme_default(base_size = 12))
  auc_table_grob <- tableGrob(auc_table, rows = NULL, theme = ttheme_default(base_size = 12))
  
  # Arrange both plots in a grid with a title for the whole plot
  dir_date <- sub(".*out_(\\d{8}).*", "\\1", basename(dir))  
  dir_date <- paste0(substr(dir_date, 1, 4), "-", substr(dir_date, 5, 6), "-", substr(dir_date, 7, 8))
  
  combined_title <- paste(city_name, dir_date)
  
  combined_title <- textGrob(combined_title, gp = gpar(fontsize = 20, fontface = "bold"))
  
  
  # Arrange the plots with the combined title
  
  layout_matrix <- matrix(c(1,  # Plot1 in the first row
                            2,  # table_grob1 in the second row
                            3,  # signal_noise_grob in the third row
                            4), 
                          nrow = 4, ncol = 1, byrow = TRUE)
  
  # Adjust the heights to control space between tables (the lower the value, the less space)
  heights <- c(0.4, 0.1, 0.1, 0.1)  # You can tweak these values to your preference
  
  combined_plot <- grid.arrange(plot1, table_grob1, signal_noise_grob, auc_table_grob,
                                layout_matrix = layout_matrix, top = combined_title, heights = heights)
  # Save the plot to the current directory
  output_path <- file.path(dir, "biosphere_plot_4-2.pdf")
  ggsave(output_path, combined_plot, width = 16, height = 13)
  
  message(paste("Saved plot to:", output_path))
  
  
}
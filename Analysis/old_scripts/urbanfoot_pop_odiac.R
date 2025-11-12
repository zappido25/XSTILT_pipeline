# Load required libraries
library(ggplot2)
library(ggmap)
library(raster)
library(dplyr)
library(viridis)
library(ggnewscale)
library(gridExtra)
library(rslurm)

# Define the function to generate the plots for each directory
plot_func <- function(current_dir) {
  
  # Initialize a list to hold all the plots
  plot_list <- list()
  
  # Extract the year and month from the directory name
  dir_name <- basename(current_dir)
  date_info <- unlist(strsplit(dir_name, "_"))[2]  # Assuming the date is the second part of the directory name, e.g., 2015111720
  year <- as.numeric(substr(date_info, 1, 4))
  month <- as.numeric(substr(date_info, 5, 6))
  
print(date_info)
print(year)
print(month)
  
  # Define the correct ODIAC path based on the year
  if (year >= 2014 && year <= 2018) {
    odiac_base_path <- "/uufs/chpc.utah.edu/common/home/lin-group16/KaiW/ODIAC"
  } else if (year >= 2020) {
    odiac_base_path <- "/uufs/chpc.utah.edu/common/home/lin-group23/lb/XSTILT_output/ODIAC"
  } else {
    return(NULL)  # Skip this iteration if the year doesn't match the range
  }
  

# Build the full path to the ODIAC files for the corresponding year
odiac_path <- file.path(odiac_base_path, as.character(year))

# Extract the last two digits of the year
year_suffix <- sprintf("%02d", year %% 100)  # Get the last two digits of the year

# Construct the month part in 'yymm' format
month_code <- sprintf("%02d", month)  # Ensure month is two digits

#Create the suffix pattern for matching files
suffix_pattern <- paste0(year_suffix, month_code, ".tif")

# List the ODIAC files ending with the specified suffix
odiac_files <- list.files(path = odiac_path, pattern = suffix_pattern, full.names = TRUE)

# List the ODIAC files matching the pattern
odiac_files <- list.files(path = odiac_path, pattern = pattern, full.names = TRUE)

# Print the found files
print(odiac_files)
  
  # Check if any ODIAC file for the specific month is found
  if (length(odiac_files) == 0) {
    return(NULL)  # Skip this iteration if no ODIAC file is found for the month
  }
  
  # Load the first matching ODIAC file for the current month
  odiac_file <- odiac_files[1]
  
  # Process the ODIAC data
  odiac <- raster(odiac_file)
  phoenix_extent <- extent(-114, -110, 32, 35)
  odiac_cropped <- crop(odiac, phoenix_extent)
  areas <- area(odiac_cropped) * 1000 * 1000
  odiac_cropped <- odiac_cropped / areas / 2678400 * 1000000 / 12.011
  odiac_df <- as.data.frame(odiac_cropped, xy = TRUE)
  colnames(odiac_df) <- c("lon", "lat", "odiac_emiss")
  filtered_data <- odiac_df %>%
    filter(odiac_emiss > 0 & odiac_emiss < 0.00002)
  
  print(paste("Loaded ODIAC data from:", odiac_file))
  
  # Process the urban footprint files
  footprint_dir <- file.path(current_dir, "_rslurm_urbanfoot")
  result_files <- list.files(path = footprint_dir, pattern = "urbanfootnode", full.names = TRUE)
  
  combined_foot <- NULL
  lon <- NULL
  lat <- NULL
  
  for (result_file in result_files) {
    data <- readRDS(result_file)
    if (is.null(combined_foot)) {
      combined_foot <- data$foot
      lon <- data$lon
      lat <- data$lat
    } else {
      combined_foot <- combined_foot + data$foot
    }
  }
  
  df <- data.frame(lon = rep(lon, each = length(lat)),
                   lat = rep(lat, times = length(lon)),
                   foot = as.vector(combined_foot))
  df <- df[df$foot > 0, ]
  
  # Process the population data
  pop_dat <- "/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/city_selection/city_clustering/gpw_v4_population_density_rev11_2020_30_sec.tif"
  gpw <- raster(pop_dat)
  gpw_cropped <- crop(gpw, phoenix_extent)
  gpw_df <- as.data.frame(gpw_cropped, xy = TRUE)
  colnames(gpw_df) <- c("lon", "lat", "pop_density")
  gpw_df <- gpw_df[gpw_df$pop_density > 0, ]
  
  # Merge footprint and population data
  combined_df <- merge(gpw_df, df, by = c("lon", "lat"))
  combined_df$log_pop_density <- log10(combined_df$pop_density)  # Updated line as requested
  
  
  print("Starting plot generation...")
  
  # Generate the plots
  # Plot 1: Urban footprint and population density
  plot1 <- ggmap(phoenix_map) +
    geom_tile(data = df, aes(x = lon, y = lat, fill = foot), alpha = 1) +
    scale_fill_gradient(low = "grey", high = "black", name = expression("linear footprint" ~ (ppm/Âµmol*m^2*s^-1))) +
    ggnewscale::new_scale_fill() +
    geom_tile(data = combined_df, aes(x = lon, y = lat, fill = log_pop_density), alpha = 0.5) +  # Updated to use log_pop_density
    scale_fill_viridis_c(option = "inferno", name = "Population Density (log10)", begin = 0.1, end = 0.9, direction = 1) +
    labs(x = "Longitude", y = "Latitude", title = "Urban Footprints and Population Density") +
    theme_minimal()
  
  # Plot 2: Urban footprint and filtered ODIAC emissions
  plot2 <- ggmap(phoenix_map) +
    geom_tile(data = df, aes(x = lon, y = lat, fill = foot), alpha = 1) +
    scale_fill_gradient(low = "grey", high = "black", name = expression("linear footprint" ~ (ppm/Âµmol*m^2*s^-1))) +
    ggnewscale::new_scale_fill() +
    geom_tile(data = filtered_data, aes(x = lon, y = lat, fill = odiac_emiss), alpha = 0.7) +
    scale_fill_viridis_c(option = "magma", limits = c(0, 0.00002), 
                         begin = 0.6, end = 0.1, name = "ODIAC Emissions (mol CO2/m^2)") +
    labs(x = "Longitude", y = "Latitude", title = "Urban Footprints and Filtered ODIAC Emissions (Phoenix)") +
    theme_minimal()
  
  # Store plots in the list
  plot_list[[1]] <- plot1
  plot_list[[2]] <- plot2
  
  # Save the plots as a PDF
  pdf_filename <- file.path("/uufs/chpc.utah.edu/common/home/lin-group23/lb/XSTILT_output/Phoenix/V11.1r", paste0("urbanfoot_pop_odiac_", basename(current_dir), ".pdf"))
  pdf(pdf_filename, width = 16, height = 8)
  
  print(paste("Saved plot to:", pdf_filename))
  
  # Print each plot to the PDF
  for (p in plot_list) {
    print(p)
  }
  
  # Close the PDF device
  dev.off()
}

# Get the list of directories to process
base_dir <- "/uufs/chpc.utah.edu/common/home/lin-group23/lb/XSTILT_output/Phoenix/V11.1r/"
foot <- list.dirs(path = base_dir, recursive = FALSE, full.names = TRUE)

#Convert foot list to a data frame
foot_df <- data.frame(current_dir = foot)

# Run the function in parallel with SLURM using slurm_apply
slurm_options <- list(time = '3:00:00', 
                      account = 'lin-np', 
                      partition = 'lin-np', 
                      mem = "0")

slurm_apply(plot_func, params = foot_df, nodes = 1, cpus_per_node = 27, 
            slurm_options = slurm_options, jobname = "odiac_pop_plot")

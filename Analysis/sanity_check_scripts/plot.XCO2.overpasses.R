### plotting the population, footprint and xco2 obs.

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

library(ggplot2)
library(dplyr)


## ploting and parsing functions are here
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
print(functions_files)


# Read in cities file and prompt user to select city
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list_remaining.txt")
cities <- read.table(cities_file, header = FALSE, skip=0, stringsAsFactors = FALSE)[,1]

OCO.DIR = Sys.getenv("OCO2_DIR")
homedir = Sys.getenv("XSTILT_PATH")
oco.ver     = 'V11r'
urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt", header=T, stringsAsFactors = F)
# cluster code for this city 

# all clusters - subset to this cluster
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")

half_width <- 1.0   # in degrees
half_height <- 1.0  # in degrees


api.key = readLines('../../insert_ggAPI.csv')
register_google(key = api.key)
stadia.api.key = readLines('../../insert_stadia_key.csv')
register_stadiamaps(key = stadia.api.key)


overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model_modified_filtered.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  
# cities=c("Cleveland")
for (city in cities) {
    matching_indices <- which(overpass_to_model$site %in% city)
    # save the results to a PDF in the folder "figures"
    pdf_filename_emi_conv <- paste0("overpasses_plots/", city, "_xco2.overpasses.pdf")
    pdf(pdf_filename_emi_conv, width = 8, height = 6)

    center_lon <- urban_core$lon[urban_core$city == city]
    center_lat <- urban_core$lat[urban_core$city == city]

    bbox <- c(
      left   = center_lon - half_width,
      bottom = center_lat - half_height,
      right  = center_lon + half_width,
      top    = center_lat + half_height
    )

    basemap <- get_stadiamap(
        bbox = bbox,
        zoom = 10,
        maptype = "stamen_terrain"#"stamen_terrain"  # Options: stamen_terrain, stamen_toner, stamen_watercolor
    )
# Print matching indices for verification
    print(city) 
    print(matching_indices)

    # one df for all xco2 values
    all_XCO2 <- data.frame()

    for (ii in matching_indices) {
  
      timestr <- overpass_to_model$timestr[ii]

      if (timestr < '2015000000' || timestr %in% incomplete_runs$timestr) {
        message("Skipping: ", city, " ", timestr)
        next
      }

      xco2_fname <- paste0(city,"_",timestr,".txt")
      file_path <- file.path(OCO.DIR, "OCO-2/overpass_obs", xco2_fname)

      if (!file.exists(file_path)) {
        message("Missing XCO2 file: ", file_path)
        next
      }

      # ✅ Read the XCO2 observations
      XCO2_overpass <- read.table(file_path, header = TRUE)

      # ✅ Create the plot properly
      p <- ggmap(basemap) +
        geom_point(
          data = XCO2_overpass,
          aes(x = lon, y = lat, color = XCO2),
          size = 2.5, alpha = 0.8
        ) +
        scale_color_viridis_c(option = "inferno",
                              name = expression(XCO[2]~"(ppm)")) +
        labs(
          title = paste0(city, " — XCO₂ Overpass: ", timestr),
          subtitle = paste0("Points: ", nrow(XCO2_overpass)),
          x = "Longitude",
          y = "Latitude"
        ) +
        coord_sf(default_crs = sf::st_crs(4326)) +
        theme_bw(base_size = 13) +
        theme(legend.position = "right")

      print(p)
    }
    dev.off()
}






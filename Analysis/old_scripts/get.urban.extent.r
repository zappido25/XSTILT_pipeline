
library(sf)
get_urban_extent <- function(city_name) {
  print("Getting urban extent for ODIAC-city...")
  print(city_name)
  if (city_name == "NewYork") {
      city_name <- "New York"
    }
  
  
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

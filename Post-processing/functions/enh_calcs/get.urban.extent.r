get_urban_extent <- function(city_name) {
  library(sf)
   
    #  print(city_name)
     if (city_name == "NewYork") {
      city_name <- "New York"
    }
    if (city_name == "SanDiego") {
      city_name <- "San Diego, CA"
    }
    if (city_name == "Austin") {
      city_name <- "Austin, TX"
    }
    if (city_name == "Atlanta") {
      city_name <- "Atlanta, GA"
    }
    if (city_name == "Portland") {
      city_name <- "Portland, OR"
    }
    if (city_name == "Philadelphia") {
      city_name <- "Philadelphia, PA"
    }
    if (city_name == "SaltLakeCity") {
      city_name <- "Salt Lake City"
    }
    if (city_name == "Baltimore") {
      city_name <- "Baltimore, MD"
    }

     if (city_name == "Washington") {
      city_name <- "Washington--Arlington"
    }
  
     if (city_name == "Dallas_FortWorth") {
      city_name <- "Dallas--Fort Worth--Arlington, TX"
    }
     if (city_name == "LasVegas") {
      city_name <- "Las Vegas--Henderson--Paradise, NV"
    }

     if (city_name == "LosAngeles") {
      city_name <- "Los Angeles"
    }
     if (city_name == "SanAntonio") {
      city_name <- "San Antonio"
    }
     if (city_name == "Boston") {
      city_name <- "Boston, MA--NH"
    }
    if (city_name == "Miami") {
      city_name <- "Miami--Fort Lauderdale, FL"
    }
    if (city_name == "Indianapolis") {
      city_name <- "Indianapolis, IN"
    }
    if (city_name == "Columbus") {
      city_name <- "Columbus, OH"
    }
    if (city_name == "Cleveland") {
      city_name <- "Cleveland, OH"
    }
    # Load the urban area US Census shapefile 
    shapefile_path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/data/Urban_Shapefiles/tl_2024_us_uac20.shp"
    gdf <- st_read(shapefile_path)
  

    # Find the matching city in the NAME20 attribute
    
    print(city_name)
    # print(gdf$NAME20)
    grepl_result <- grepl(city_name, gdf$NAME20, ignore.case = TRUE)
    # grepl_result <- grepl(city_name, gdf$NAME20, ignore.case = TRUE)
    gdf_city <- gdf[grepl_result, ]

    print(paste("Used:", gdf_city[1, ]$NAME20))
  
    # print("All Options:")
    # # print(gdf_city)
  
    if (nrow(gdf_city) == 0) {
      stop(paste("City not found in the shapefile:", city_name))
    }
  
    # If multiple matches, take the first one (you can modify this logic as needed)
    bbox <- st_bbox(gdf_city[1, ])
  
    # Return the bounding box in the required format
    return(list(
      lat = c(bbox$ymin, bbox$ymax),
      lon = c(bbox$xmin, bbox$xmax)
    ))
  }

# get_urban_extent <- function(city_name) {
#    library(sf)
#    print("Getting urban extent for ODIAC city...")
#      print(city_name)
#     if (city_name == "NewYork") {
#       city_name <- "New York"
#     }
#     if (city_name == "SanDiego") {
#       city_name <- "San Diego"
#     }

#     if (city_name == "Austin") {
#       city_name <- "Austin, TX"
#     }
#     if (city_name == "Atlanta") {
#       city_name <- "Atlanta, GA"
#     }
#      if (city_name == "Portland") {
#       city_name <- "Portland, OR"
#     }
#     #  if (city_name == "Baltimore") {
#     #   city_name <- "Baltimore, VA"
#     # }
#     # Load the urban area US Census shapefile 
#     shapefile_path <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/data/Urban_Shapefiles/tl_2024_us_uac20.shp"
#     gdf <- st_read(shapefile_path)
  

#     # Find the matching city in the NAME20 attribute
    
#     print(city_name)
#     grepl_result <- grepl(city_name, gdf$NAME20, ignore.case = TRUE)
#     gdf_city <- gdf[grepl_result, ]
  
#     # print(paste("Used:", gdf_city[1, ]$NAME20))
#     # print("All Options:")
#     # # print(gdf_city)
  
#     if (nrow(gdf_city) == 0) {
#       stop(paste("City not found in the shapefile:", city_name))
#     }
  
#     # If multiple matches, take the first one (you can modify this logic as needed)
#     bbox <- st_bbox(gdf_city[1, ])
  
#     # Return the bounding box in the required format
#     return(list(
#       lat = c(bbox$ymin, bbox$ymax),
#       lon = c(bbox$xmin, bbox$xmax)
#     ))
#   }
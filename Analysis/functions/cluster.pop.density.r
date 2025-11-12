
## plotting cluster population density plots
cluster.pop.density=function(dens,cluster, phoenix_map, center_lat, center_lon, city_points, footprint,
                        XCO2_overpass, timestr, site, dataframe ) {

    library(raster)
    # library(tabularaster)
    library(sp)
    library(ggmap)
    library(ggnewscale)
    library(viridisLite)

    band_half <- 0.05
    city_lat_mids <- dataframe$lat_mid[dataframe$category == "city"]   # your dataframe with lat_mid & category

# For each point latitude, check if it's within any city band
    if (length(city_lat_mids)) {
    XCO2_overpass$is_city_band <- vapply(
      XCO2_overpass$lat,
      function(phi) any(abs(phi - city_lat_mids) <= band_half),
      logical(1)
    )
  } else {
    XCO2_overpass$is_city_band <- FALSE
  }
    # create some plots
    p_city<-ggmap(phoenix_map)

    # to provide some context for density here
    i_dens<-crop(dens, extent(min(cluster$long)-0.15, max(cluster$long)+0.15, min(cluster$lat)-0.15, max(cluster$lat)+0.15))
    
    city<- raster::extract(i_dens, city_points, cellnumbers=TRUE)[,"cells"]
    all<-1:(dim(i_dens)[1]*dim(i_dens)[2])
    not_city<-setdiff(all, city)
    
    city_dens<-i_dens
    city_dens[not_city]<-NA
    city_dens<-rasterToPoints(city_dens)
    city_dens<-as.data.frame(city_dens)
    city_dens<-na.omit(city_dens)
    
    not_city_dens<-i_dens
    not_city_dens[city]<-NA
    not_city_dens<-rasterToPoints(not_city_dens)
    not_city_dens<-as.data.frame(not_city_dens)
    not_city_dens<-na.omit(not_city_dens)
    not_city_dens<-not_city_dens[not_city_dens$gpw_v4_population_density_rev11_2020_30_sec > 0, ]
    
    city_dens$city<-TRUE
    not_city_dens$city<-FALSE
    
    context<-rbind(city_dens, not_city_dens)
    colnames(context)[3]<-"dens"
  
    # add density information
    # Plot population density on log10 scale
    context$dens_log10 <- log10(context$dens + 1) # add 1 to avoid log(0)
   
   
    footprint_df <- as.data.frame(rasterToPoints(footprint))#footprint_df <- as.data.frame(rasterToPoints(footprint))
    colnames(footprint_df) <- c("lon", "lat", "foot")

    q <- ggmap(phoenix_map) +
    geom_tile(data = footprint_df, aes(x = lon, y = lat, fill = foot), alpha = 0.5) +
    scale_fill_gradient(low = "grey90", high = "grey10", name = expression("Footprint (ppm / "*mu*"mol m"^-2*" s"^-1*")")) +
    # add pop density
    
    ggnewscale::new_scale_fill() +
    geom_tile(data = context, aes(x = x, y = y, fill = dens_log10), alpha = 0.7) +  # Updated to use log_pop_density
    scale_fill_viridis_c(option = "mako", name = "Population Density (log10)", begin = 0.1, end = 0.9, direction = 1) +
    labs(x = "Longitude", y = "Latitude", title = paste0("Footprints, Population Density and XCO2 for: ", timestr)) +
    # theme_minimal()  
    ## add XCO2
    # XCO2 colored points (own color scale)
    ggnewscale::new_scale_color() +
    geom_point(
      data = XCO2_overpass,
      aes(lon, lat, color = XCO2),
      size = 3, alpha = 0.8          # <-- outside aes()
    ) +
    scale_color_viridis_c(option = "inferno", name = expression("XCO"[2]*" (ppm)")) +

  # BLACK OUTLINE ONLY for city-band points (overlay; no new scale needed)
    ggnewscale::new_scale_fill() +
    geom_point(
      data = subset(XCO2_overpass, is_city_band),
      aes(lon, lat, fill = XCO2),  # interior matches XCO2 color
      shape = 21,                   # circle with fill + outline
      color = scales::alpha("black", 0.7),  # ring color (black; change if you want same as fill)
      stroke = 1.2,
      size = 3.6
    ) +
    # Use the same palette for fill, but hide its legend to avoid duplication
    scale_fill_viridis_c(option = "inferno", guide = "none")+
  
  # open color OUTLINE ONLY for bg-band points (overlay; no new scale needed)
    ggnewscale::new_scale_fill() +
      geom_point(
        data = subset(XCO2_overpass, !is_city_band),
        aes(lon, lat, fill = XCO2),  # interior matches XCO2 color
        shape = 21,                   # circle with fill + outline
        color = scales::alpha("white", 0.7),  # ring color (white for bg-band)
        stroke = 1.2,
        size = 3.6
      ) +
      # Use the same palette for fill, but hide its legend to avoid duplication
      scale_fill_viridis_c(option = "inferno", guide = "none")+
    coord_fixed() +
    theme_minimal() 
  
}
  

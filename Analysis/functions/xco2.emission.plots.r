
## plotting XCO2 overpasses for each timestr
plot_xco2_maps <- function(phoenix_map, XCO2_overpass, rects, timestr) {
  p <- ggmap(phoenix_map) +
          geom_point(
            data = XCO2_overpass,
            aes(x = lon, y = lat, color = XCO2),
            size = 3,
            alpha = 0.8
          ) +
          scale_color_viridis_c(option = "plasma", name = "XCO₂ (ppm)") +

          ggnewscale::new_scale_color() +  #  this resets the color scale

          geom_rect(
            data = rects,
            mapping = aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method),
            inherit.aes = FALSE,
            fill = NA,
            linetype = "dashed",
            linewidth = 1
          ) +
          scale_color_manual(
            values = c("City Clustering" = "red", "Umich" = "blue"),
            name = "Extent Method"
          ) +
          coord_fixed() +
          labs(
            title = paste("Phoenix XCO₂ Overpass:", timestr),
            x = "Longitude",
            y = "Latitude"
          ) +
          theme_minimal()

          print(p)
        
}
## plotting emissions for each timestr and each method (Umich and City Clustering)       
emission_plot <- function(phoenix_map, df_shp, df_cc,
                          xmin_shp,xmax_shp, ymin_shp, ymax_shp,
                          xmin_cc,xmax_cc, ymin_cc, ymax_cc,
                           timestr) {
    # color_scale <- c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")  # vivid viridis-like
    color_scale <- "heat" 
      
    p_shp <- ggmap(phoenix_map) +
      geom_tile(data = df_shp, aes(x = lon, y = lat, fill = value), alpha = 0.7) +
      scale_fill_gradientn(
        colors = color_scale,
        trans = "log10",
        limits = common_limits,
        na.value = "gray30",
        name = expression("Emissions ("*mu*"mol/m"^2*"/s)")
      ) +
      labs(title = paste("Umich", timestr), x = "Longitude", y = "Latitude") +
      geom_rect(
        aes(xmin = xmin_shp, xmax = xmax_shp, ymin = ymin_shp, ymax = ymax_shp),
        fill = NA, color = "blue", linetype = "dashed", linewidth = 1.2
      ) +
      labs(title = paste("Umich", timestr), x = "Longitude", y = "Latitude") +
      theme_minimal()

    # # Plot for City Clustering method
      p_cc <- ggmap(phoenix_map) +
      geom_tile(data = df_cc, aes(x = lon, y = lat, fill = value), alpha = 0.7) +
      scale_fill_gradientn(
        colors = color_scale,
        trans = "log10",
        limits = common_limits,
        na.value = "gray30",
        name = expression("Emissions ("*mu*"mol/m"^2*"/s)")
      ) +
      geom_rect(
        aes(xmin = xmin_cc, xmax = xmax_cc, ymin = ymin_cc, ymax = ymax_cc),
        fill = NA, color = "red", linetype = "dashed", linewidth = 1.2
      ) +
      labs(title = paste("City Clustering", timestr), x = "Longitude", y = "Latitude") +
      theme_minimal()
      
      print((p_shp | p_cc) + plot_layout(guides = "collect"))

}     

## plotting emission differences between Umich and City Clustering methods
emission_diff <- function(phoenix_map, df_shp, df_cc,
                          xmin_shp,xmax_shp, ymin_shp, ymax_shp,
                          xmin_cc,xmax_cc, ymin_cc, ymax_cc,
                           timestr) {
      df_shp$lon_round <- round(df_shp$lon, 4)
      df_shp$lat_round <- round(df_shp$lat, 4)
      df_shp$value_shp <- df_shp$value
      df_cc$lon_round <- round(df_cc$lon, 4)
      df_cc$lat_round <- round(df_cc$lat, 4)
      df_cc$value_cc <- df_cc$value

      df_diff <- full_join(
        df_shp, df_cc,
        by = c("lon_round", "lat_round"),
        suffix = c("_shp", "_cc")
      ) %>%
        mutate(
          lon = coalesce(lon_shp, lon_cc),
          lat = coalesce(lat_shp, lat_cc),
          value_shp = replace_na(value_shp, 0),
          value_cc = replace_na(value_cc, 0),
          diff = value_shp - value_cc
        )

      
      # Get basemap for Phoenix
      # bbox <- c(left = -113, bottom = 32.5, right = -111, top = 34.0)
      # phoenix_map <- get_stadiamap(bbox = bbox, zoom = 10, maptype = "outdoors")

      # Define bounding boxes from extent or rects
      diff_rects <- data.frame(
        xmin = c(xmin_shp, xmin_cc),
        xmax = c(xmax_shp, xmax_cc),
        ymin = c(ymin_shp, ymin_cc),
        ymax = c(ymax_shp, ymax_cc),
        method = c("Umich", "City Clustering")
      )


      # Plot difference
      p_diff <- ggmap(phoenix_map) +
        geom_tile(
          data = df_diff,
          aes(x = lon, y = lat, fill = diff),
          alpha = 0.7
        ) +
        scale_fill_gradient2(
          name = expression(Delta ~ Emissions ~ (mu*mol/m^2/s)),
          low = "blue", mid = "white", high = "red",
          midpoint = mean(df_diff$diff, na.rm = TRUE),
          limits = c(-max(abs(df_diff$diff), na.rm = TRUE), max(abs(df_diff$diff), na.rm = TRUE))
        ) +
        geom_rect(
          data = diff_rects,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method),
            inherit.aes = FALSE,
          fill = NA, linetype = "dashed", linewidth = 1.2
        ) +
        scale_color_manual(values = c("City Clustering" = "red", "Umich" = "blue")) +
        coord_fixed() +
        labs(
          title = paste0("Difference in Emissions: Shapefile vs. City Clustering",timestr),
          x = "Longitude", y = "Latitude"
        ) +
        theme_minimal()

        print(p_diff)

}        

## plotting convolution between the emission differences between Umich and City Clustering methods and footprint
emission_footprint_convolution <- function(phoenix_map, df_shp, df_cc,
                          xmin_shp,xmax_shp, ymin_shp, ymax_shp,
                          xmin_cc,xmax_cc, ymin_cc, ymax_cc,
                           timestr,footprint ) {

        # Convolve the emissions with the footprint
      # emissions_cc_conv <- resample(emissions_city_cc, footprint_raster, method = "bilinear")
      # emissions_shp_conv <- resample(emissions_city_shp, footprint_raster, method = "bilinear")                 
      
      df_shp$lon_round <- round(df_shp$lon, 4)
      df_shp$lat_round <- round(df_shp$lat, 4)
      df_shp$value_shp <- df_shp$value
      df_cc$lon_round <- round(df_cc$lon, 4)
      df_cc$lat_round <- round(df_cc$lat, 4)
      df_cc$value_cc <- df_cc$value

      df_diff <- full_join(
        df_shp, df_cc,
        by = c("lon_round", "lat_round"),
        suffix = c("_shp", "_cc")
      ) %>%
        mutate(
          lon = coalesce(lon_shp, lon_cc),
          lat = coalesce(lat_shp, lat_cc),
          value_shp = replace_na(value_shp, 0),
          value_cc = replace_na(value_cc, 0),
          diff = value_shp - value_cc
        )
     

      # Convert df_diff to raster
      diff_raster <- rasterFromXYZ(df_diff[, c("lon", "lat", "diff")])

      # Ensure footprint is a raster of the same extent and resolution
      # Resample footprint raster to match diff_raster's extent and resolution
      footprint_resampled <- raster::resample(footprint, diff_raster, method = "bilinear")

      # Multiply rasters
      multiplied_raster <- diff_raster * footprint_resampled

      # Convert result back to data.frame for plotting
      multiplied_df <- as.data.frame(rasterToPoints(multiplied_raster))
      colnames(multiplied_df) <- c("lon", "lat", "diff")
      df_diff <- multiplied_df

      
      # Get basemap for Phoenix
      # bbox <- c(left = -113, bottom = 32.5, right = -111, top = 34.0)
      # phoenix_map <- get_stadiamap(bbox = bbox, zoom = 10, maptype = "outdoors")

      # Define bounding boxes from extent or rects
      diff_rects <- data.frame(
        xmin = c(xmin_shp, xmin_cc),
        xmax = c(xmax_shp, xmax_cc),
        ymin = c(ymin_shp, ymin_cc),
        ymax = c(ymax_shp, ymax_cc),
        method = c("Umich", "City Clustering")
      )

      # Plot difference
      p_diff <- ggmap(phoenix_map) +
        geom_tile(
          data = df_diff,
          aes(x = lon, y = lat, fill = diff),
          alpha = 0.7
        ) +
        scale_fill_gradient2(
          name = expression("Umich"[conv] - "CC"[conv] (ppm)),
          low = "blue", mid = "white", high = "red",
          midpoint = 0,
          limits = c(-max(abs(df_diff$diff), na.rm = TRUE), max(abs(df_diff$diff), na.rm = TRUE))
        ) +
        geom_rect(
          data = diff_rects,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, color = method),
            inherit.aes = FALSE,
          fill = NA, linetype = "dotted", linewidth = 1.2
        ) +
        scale_color_manual(values = c("City Clustering" = "red", "Umich" = "blue")) +
        coord_fixed() +
        labs(
          title = paste0("Difference in Emissions: Shapefile vs. City Clustering",timestr),
          x = "Longitude", y = "Latitude"
        ) +
        theme_minimal()

        print(p_diff)

}        


enhancements_plot <- function(phoenix_map, df_shp, df_cc,
                          xmin_shp,xmax_shp, ymin_shp, ymax_shp,
                          xmin_cc,xmax_cc, ymin_cc, ymax_cc,
                           rects) {
    color_scale <- c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151", "#FDE725")  # vivid viridis-like

    ## five points with highest enhancement mean
    N <- 5
    shp_labels <- df_shp %>%
      arrange(desc(enh_mean)) %>%
      slice(1:N) %>%
      mutate(label = round(enh_mean, 3))  # label with rounded values
    cc_labels <- df_cc %>%
      arrange(desc(enh_mean)) %>%
      slice(1:N) %>%
      mutate(label = round(enh_mean, 3))  # label with rounded values


    common_limits <- c(
      min(c(df_shp$enh_mean, df_cc$enh_mean), na.rm = TRUE),
      max(c(df_shp$enh_mean, df_cc$enh_mean), na.rm = TRUE)
    )

    p_shp <- ggmap(phoenix_map) +
      geom_point(data = df_shp, aes(x = lon, y = lat, color = enh_mean), size = 3, alpha = 0.7) +
      scale_color_gradientn(
        colors = color_scale,
        limits = common_limits,
        na.value = "gray30",
        name = expression("Enhancement Mean (ppm)")
      ) +
      geom_text_repel(
        data = shp_labels,
        aes(x = lon, y = lat, label = label),
        size = 4,
        fontface = "bold",
        color = "black",
        box.padding = 0.4,
        point.padding = 0.5
      ) +
      labs(title = paste("Umich"), x = "Longitude", y = "Latitude") +
      geom_rect(
        aes(xmin = xmin_shp, xmax = xmax_shp, ymin = ymin_shp, ymax = ymax_shp),
        fill = NA, color = "blue", linetype = "dashed", linewidth = 1.2
      ) +
      labs(title = paste("Umich"), x = "Longitude", y = "Latitude") +
      theme_minimal()

    # # Plot for City Clustering method
      p_cc <- ggmap(phoenix_map) +
      geom_point(data = df_cc, aes(x = lon, y = lat, color = enh_mean), size = 3, alpha = 0.7) +
      scale_color_gradientn(
        colors = color_scale,
        limits = common_limits,
        na.value = "gray30",
        name = expression("Enhancement Mean (ppm)")
      ) +
      geom_text_repel(
        data = cc_labels,
        aes(x = lon, y = lat, label = label),
        size = 4,
        fontface = "bold",
        color = "black",
        box.padding = 0.4,
        point.padding = 0.5
      ) +
      geom_rect(
        aes(xmin = xmin_cc, xmax = xmax_cc, ymin = ymin_cc, ymax = ymax_cc),
        fill = NA, color = "red", linetype = "dashed", linewidth = 1.2
      ) +
      labs(title = paste("City Clustering"), x = "Longitude", y = "Latitude") +
      theme_minimal()
      
      # timeseries
      combined_df <- rbind(df_shp, df_cc)
      # combined_df$timestr <- as.character(combined_df$timestr)
      # combined_df$date <- as.POSIXct(combined_df$timestr, format = "%Y%m%d%H", tz = "UTC")

      # p_timeseries <- ggplot(combined_df, aes(x = date, y = pps_mean, color = method)) +
      #   geom_line(alpha = 0.7) +
      #   geom_point(alpha = 0.8) +
      #   labs(title = paste("Enhancement Mean Time Series for", city),
      #       x = "Date", 
      #       y = "Enhancement Mean (ppm)",
      #       color = "Method") +
      #   theme_minimal() +
      #   scale_color_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))
        print(paste0("city_clustering:",median(df_cc$int_epc_mean, na.rm = TRUE)))
        print(paste0("UMICH:",median(df_shp$int_epc_mean, na.rm = TRUE)))

        p1 <- ggplot(combined_df, aes(x = method, y = int_epc_mean, fill = method)) +
          geom_boxplot(alpha = 0.7, outlier.shape = NA) +
          geom_jitter(width = 0.2, alpha = 0.6, color = "black") +
          stat_summary(
            fun = mean,
            geom = "point",
            shape = 23,
            size = 3,
            color = "darkblue",
            fill = "yellow"
          ) +
          stat_summary(
            fun.data = function(y) {
          # Compute mean and sd for errorbar
          m <- mean(y, na.rm = TRUE)
          print(paste0("m is:", m))
          s <- mean(combined_df$pps_sd[combined_df$method == unique(combined_df$method[which(y == y[1])])], na.rm = TRUE)
          data.frame(y = m, ymin = m - s, ymax = m + s)
            },
            geom = "errorbar",
            width = 0.2,
            color = "orange"
          ) +
        labs(
          title = "E_pc Comparison by Method",
          x = "Method",
          y = "E_pc",
          fill = "Method"
        ) +
        theme_minimal() +
        scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))



      print((p_shp | p_cc) + plot_layout(guides = "collect"))
      print(p1)

}     

## plotting distribution of pps_mean values
plot_pps_distribution <- function(site, df_shp, df_cc) {

  pdf_filename_dist <- paste0(site, "_emission_dist.pdf")
  pdf(pdf_filename_dist, width = 8, height = 6) 

  # Combine the datasets
  combined_df <- rbind(df_shp, df_cc)
  
  # 1. Histogram with density overlay
  p1 <- ggplot(combined_df, aes(x = pps_mean, fill = method)) +
    geom_histogram(aes(y = ..density..), alpha = 0.7, bins = 30, position = "identity") +
    geom_density(alpha = 0.5, color = "black") +
    facet_wrap(~method, ncol = 2) +
    labs(
      title = "Distribution of PPS Mean Values",
      x = "PPS Mean (ppm)",
      y = "Density",
      fill = "Method"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))
  
  # 2. Violin plot with box plot overlay
  p2 <- ggplot(combined_df, aes(x = method, y = pps_mean, fill = method)) +
    geom_violin(alpha = 0.7, trim = FALSE) +
    geom_boxplot(width = 0.1, alpha = 0.8, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.4, size = 1) +
    labs(
      title = "PPS Mean Distribution by Method",
      x = "Method",
      y = "PPS Mean (ppm)",
      fill = "Method"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))
  
  # 3. Density ridges plot
  library(ggridges)
  p3 <- ggplot(combined_df, aes(x = pps_mean, y = method, fill = method)) +
    geom_density_ridges(alpha = 0.7, scale = 2) +
    labs(
      title = "PPS Mean Distribution Ridges",
      x = "PPS Mean (ppm)",
      y = "Method",
      fill = "Method"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c"))
  
  # 4. Combined overlay histogram
  p4 <- ggplot(combined_df, aes(x = pps_mean, fill = method)) +
    geom_histogram(alpha = 0.6, bins = 30, position = "identity") +
    geom_density(aes(color = method), alpha = 0.8, size = 1, fill = NA) +
    labs(
      title = "PPS Mean Distribution Comparison",
      x = "PPS Mean (ppm)",
      y = "Count",
      fill = "Method",
      color = "Method"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Umich" = "#3498db", "City Clustering" = "#e74c3c")) +
    scale_color_manual(values = c("Umich" = "#2980b9", "City Clustering" = "#c0392b"))
  
  # Print all plots
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  # Return summary statistics
  summary_stats <- combined_df %>%
    group_by(method) %>%
    summarise(
      mean = mean(pps_mean, na.rm = TRUE),
      median = median(pps_mean, na.rm = TRUE),
      sd = sd(pps_mean, na.rm = TRUE),
      min = min(pps_mean, na.rm = TRUE),
      max = max(pps_mean, na.rm = TRUE),
      q25 = quantile(pps_mean, 0.25, na.rm = TRUE),
      q75 = quantile(pps_mean, 0.75, na.rm = TRUE),
      .groups = 'drop'
    )
  
  print("Summary Statistics:")
  print(summary_stats)
  
  return(summary_stats)
}
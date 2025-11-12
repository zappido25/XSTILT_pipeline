library(grid)
library(gridExtra)
library(raster)
library(ncdf4)
library(terra)
library(sf)
library(rslurm)
library(dplyr)
library(pracma)
library(ggplot2)

source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/plotting_XSTILT/get.urban.extent.r")

group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")


# Get all directories starting with "out_"
OCO.DIR = Sys.getenv("OCO2_DIR")
overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log.txt"),
                header = TRUE, sep = "\t")  

city=c("Phoenix")
# for (ss in CONUS$City) {
for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)
 
  
    dir_path <- (paste0(OCO.DIR, "/XSTILT_output/",
                      ss, "/", version))
    base_dir <- dir(dir_path, full.names = TRUE)  
      

    for (ii in matching_indices) {
        site=ss
        city_name=ss
        timestr=overpass_to_model$timestr[ii]
        # print(paste0("CITY::", site))
        # print(paste0("Timestr::", timestr))

        if ((timestr %in% incomplete_runs$timestr)) {
            print("skipping failed runs")
            print(paste0("CITY::", ss))
            print(paste0("Timestr::", timestr))
            next
        } 

      dirs <- base_dir[grepl(paste0("out_",timestr), base_dir)]  # Filter directories starting with "out_"

      print(dirs)

      out.dir=  paste0(out.path,"/dataframe_output/", site)


      plot.folder="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/OVERPASS_OUTPUTS/Plots"

      # Read the CSV file into a data frame
      xco2_data_file <- file.path(out.dir, paste0("xco2_data_region_pop_",timestr,".csv"))
      xco2_data<- read.csv(xco2_data_file,
                            header = TRUE, stringsAsFactors = FALSE)



      urban_extent <- get_urban_extent(city_name)

        
      background_region <- xco2_data$Background == "Yes"
      urban_region <- xco2_data$Urban == "Yes"

        
      # Calculate averages for the background region
      avg_smurf_co2_bg <- mean(xco2_data$smurf[background_region], na.rm = TRUE)
      avg_odiac_bg_shp <- mean(xco2_data$odiac_shp_urban[background_region], na.rm = TRUE)
      avg_odiac_bg_pop <- mean(xco2_data$odiac_pop_urban[background_region], na.rm = TRUE)
      avg_xco2_bg <- mean(xco2_data$XCO2[background_region], na.rm = TRUE)

      # Calculate averages for the urban region
      avg_smurf_co2_urban <- mean(xco2_data$smurf[urban_region], na.rm = TRUE)
      avg_odiac_urban_shp <- mean(xco2_data$odiac_shp_urban[urban_region], na.rm = TRUE)
      avg_odiac_urban_pop <- mean(xco2_data$odiac_pop_urban[urban_region], na.rm = TRUE)
      avg_xco2_urban <- mean(xco2_data$XCO2[urban_region], na.rm = TRUE)

      # AUC calculation for CO2 types

        
      auc_smurf_co2 <- trapz(xco2_data$lat, xco2_data$smurf)
      auc_odiac_conv_shp <- trapz(xco2_data$lat, xco2_data$odiac_shp_urban)
      auc_odiac_conv_pop <- trapz(xco2_data$lat, xco2_data$odiac_pop_urban)
        
        
        # Create the plot
        
      # Subtract background XCO2 from XCO2 values
      xco2_data$XCO2_adj <- xco2_data$XCO2 - avg_xco2_bg
  
        # Define size of latitude bin
      bin_size <- 0.05
        
      # stop("Check the column names in xco2_data")  
        # Aggregate data into 0.05-degree latitude bins
      aggregated_data <- xco2_data %>%
      mutate(lati_bin = floor(lat / bin_size) * bin_size) %>%
      group_by(lati_bin) %>%
      summarise(
        avg_XCO2 = mean(XCO2_adj, na.rm = TRUE)
      ) %>%
      ungroup()


      plot1 <- ggplot(xco2_data, aes(x = lat)) +
      # Regular observations as circles
      geom_point(aes(y = XCO2_adj, color = "Delta CO2 (Obs-bg)", 
                      shape = "Delta CO2 (Obs-bg)"), 
                  size = 3, show.legend = TRUE) +

      # Urban-enhanced points as red circles
      geom_point(data = xco2_data %>% filter(Urban == "Yes"), 
                  aes(x = lat, y = XCO2_adj, color = "Delta CO2 (Obs-bg, urban-enhanced region)", 
                      shape = "Delta CO2 (Obs-bg, urban-enhanced region)"), 
                  size = 3, alpha = 0.4, show.legend = TRUE) +  

      geom_point(data = xco2_data %>% filter(Background == "Yes"), 
                  aes(x = lat, y = XCO2_adj, color = "Delta CO2 (Obs-bg, background region)", 
                      shape = "Delta CO2 (Obs-bg, background region)"), 
                  size = 3, alpha = 0.4, show.legend = TRUE) +

      # Line plots for other CO2 components
      geom_line(aes(y = smurf, color = "Delta CO2 - Bio (SMUrF)"), linewidth = 1) +
      geom_line(aes(y = `odiac_shp_urban`, color = "Delta CO2 - Anthro shp file (ODIAC)"), linewidth = 1) +
      geom_line(aes(y = `odiac_pop_urban`, color = "Delta CO2 - Anthro pop (ODIAC)"), linewidth = 1) +

      # Black triangles for average XCO2 values in each latitude bin
      geom_point(data = aggregated_data, 
                  aes(x = lati_bin, y = avg_XCO2, shape = "Average 0.05 deg bin Delta CO2 (Obs-bg)"), 
                  color = "black", size = 4) +

      # Urban region shaded area
      annotate("rect", xmin = urban_extent$lat[1], xmax = urban_extent$lat[2], 
                ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.2) +

      annotate("text", x = ((urban_extent$lat[1]+urban_extent$lat[2])/2), y = min(xco2_data$XCO2_adj), 
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
      "Average Delta CO2 Anthro using shp file(ppm)" = c(avg_odiac_urban_shp, avg_odiac_bg_shp, (avg_odiac_urban_shp - avg_odiac_bg_shp)),
      "Average Delta CO2 Anthro using pop (ppm)" = c(avg_odiac_urban_pop, avg_odiac_bg_pop, (avg_odiac_urban_pop - avg_odiac_bg_pop)),
      "Average Delta CO2 Bio (ppm)"= c(avg_smurf_co2_urban, avg_smurf_co2_bg, (avg_smurf_co2_urban-avg_smurf_co2_bg)),
      check.names=FALSE
      )

      # New Table: Signal/Noise Ratio
      signal_noise_table <- data.frame(
      Region = "Urban-Background Gradient",
      "Average Delta CO2 Anthro/Bio (Signal/Noise Ratio) using shp file (ppm)" = (avg_odiac_urban_shp - avg_odiac_bg_shp) / (avg_smurf_co2_urban - avg_smurf_co2_bg),
      "Average Delta CO2 Anthro/Bio (Signal/Noise Ratio) using pop (ppm)" = (avg_odiac_urban_pop - avg_odiac_bg_shp) / (avg_smurf_co2_urban - avg_smurf_co2_bg),
      check.names=FALSE
      )


      auc_table <- data.frame(
      "Delta CO2" = c("Anthro", "Bio", "Anthro/Bio (Signal/Noise Ratio)"),
      "Area Under Curve using shp file (ppm*deg)" = c(auc_odiac_conv_shp, auc_smurf_co2, (auc_odiac_conv_shp/auc_smurf_co2)),
      "Area Under Curve using pop (ppm*deg)" = c(auc_odiac_conv_pop, auc_smurf_co2, (auc_odiac_conv_pop/auc_smurf_co2)),

      check.names= FALSE
      )

      # Create the table grobs
      table_grob1 <- tableGrob(avg_table1, rows = NULL, theme = ttheme_default(base_size = 12))
      signal_noise_grob <- tableGrob(signal_noise_table, rows = NULL, theme = ttheme_default(base_size = 12))
      auc_table_grob <- tableGrob(auc_table, rows = NULL, theme = ttheme_default(base_size = 12))

      # Arrange both plots in a grid with a title for the whole plot

      combined_title <- paste(city_name, timestr)

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
      out.fname=paste0("biosphere_plot_", site, "_", timestr, ".pdf")
      output_path <- file.path(plot.folder,ss, out.fname)
      ggsave(output_path, combined_plot, width = 16, height = 13)

      message(paste("Saved plot to:", output_path))

    }
  }

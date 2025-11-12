# -- core processing pipeline --
# Input: emissions from ODIAC, X-STILT metadata, shapefile + city clustering bounds
# Output: Emission comparison plots over Houston for all valid timestrs

# Main workflow and plotting logic goes here (condensed for canvas)
# If needed, you can break this out into modular functions:
# - load_emissions()
# - get_bounds_from_shapefile()
# - get_bounds_from_clustering()
# - plot_emission_diff()

# Relevant sections
# 1. Extract extent from shapefile
# 2. Extract city cluster coordinates and compute extent
# 3. Load ODIAC emission raster and crop to both bounds
# 4. Convert to umol/m2/s
# 5. Resample and compute difference
# 6. Plot difference + bounding boxes over base map (Stadia)

# Make sure `emission_diff()` is correctly defined and accessible
# Ensure pdf() and dev.off() only wrap *outside* the loop to include all pages

# To debug or visualize: try running 1 loop iteration with an example timestr
# Add `print()` statements to inspect emissions_cc, emissions_shp, and bounding boxes

# Ensure required functions like `emission_conversion()` and `get_urban_extent()` are loaded
# Ensure CRS are consistent for shapefile crop and raster crop

# Resulting PDF: Houston_emission_diff.pdf
# Contains one page per timestr, showing Umich vs City Clustering difference overlaid on map

## addition on 08.08.2025
# plot enhancements 



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

args <- commandArgs(trailingOnly = TRUE)

# Set defaults
run_convolution <- FALSE
run_enhancements <- FALSE
stats=FALSE
seasonality=FALSE

if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  run_convolution <- as.logical(args[1])
}
if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  run_enhancements <- as.logical(args[2])
}
if (length(args) >= 3 && !is.na(args[3]) && nzchar(args[3])) {
  stats <- as.logical(args[3])
}
if (length(args) >= 4 && !is.na(args[4]) && nzchar(args[4])) {
  seasonality <- as.logical(args[4])
}

# if (is.na(run_convolution) | is.na(run_enhancements)) {
#   stop("Invalid logical argument. Use TRUE or FALSE.")
# }

if (run_convolution) {
  cat("Convolution is enabled.\n")
} else {
  cat("Convolution is disabled.\n")
}

if (run_enhancements || stats || seasonality) {
  cat("Enhancements or stats or seasonality are enabled.\n")
  # save enhancements in a list
  enh_shp<-c()
  enh_cc<-c()

} else {
  cat("Enhancements are disabled.\n")
}

# stop("Stopping execution for debugging purposes. Check the variables run_convolution and run_enhancements.")
year<-"2023"
emiss.path=file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/ODIAC/",year)

# if timestr is after 2024, use the 2023 emissions
group = Sys.getenv("group")

version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
r_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/get.urban.extent.r')

functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
# print(r_files)
invisible(lapply(r_files, source))
invisible(lapply(functions_files, source))


OCO.DIR = Sys.getenv("OCO2_DIR")
overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_all.txt"),
                header = TRUE, sep = "\t")  

homedir = Sys.getenv("XSTILT_PATH")
oco.ver     = 'V11r'
oco.sensor  = c('OCO-2', 'OCO-3')[1]
input.path  = file.path(homedir, 'OCO2_2021_test/')



urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
# cluster code for this city 

# all clusters - subset to this cluster
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")

half_width <- 1.0   # in degrees
half_height <- 1.0  # in degrees


center_lon <- -112.0
center_lat <- 33.3


bbox <- c(
  left   = center_lon - half_width,
  bottom = center_lat - half_height,
  right  = center_lon + half_width,
  top    = center_lat + half_height
)

city=c("Houston")
api.key = readLines('../../../insert_ggAPI.csv')
register_google(key = api.key)
stadia.api.key = readLines('../../../insert_stadia_key.csv')
register_stadiamaps(key = stadia.api.key)

## get geographic extent and city emissions for city clustering method 
cluster_code<-urban_core$ID[urban_core$city == "Houston"]
cluster<-cluster[cluster$cluster_id == cluster_code, ] 


print(paste0(min(cluster$long)-11, max(cluster$long)+11, min(cluster$lat)-11, max(cluster$lat)+11)) 
print(paste0(min(cluster$long), max(cluster$long), min(cluster$lat), max(cluster$lat)) )


city_points <- SpatialPointsDataFrame(coords = cluster[,1:2], data = cluster[,1:2],
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

if (run_enhancements || stats || seasonality){
  enh_folder=file.path(out.path, "ENHANCEMENTS", city)

  # columns for enhancements
  colnames_readin = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                        "int_sd", "int_epc_mean", "int_epc_sd", "CI", "CI_sd", #"int_epc_gdp_mean" is carbon intensity
                          "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                        "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                        "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")
                           
}


# Houston_map <- get_googlemap(
#   center = "Houston, AZ",
#   zoom = 11,
#   maptype = "roadmap"
# )
emission_conversion<-function(emissions){
  # units conversions for emissions (starts in tonnes C/ grid cell / month)
  emissions<-emissions*1000000 # gC/grid cell/month
  emissions<-emissions/12.011*1000000 #umol/grid cell/month
  area.raster <- raster::area(emissions) * 1E6    # convert km2 to m2
  emissions<-emissions/area.raster #umol/m2/month
  number_of_days<-as.numeric(lubridate::days_in_month(paste0(year,"-",month,"-01")))
  emissions<-emissions/(number_of_days*24*60*60) #umol/m2/s
  return(emissions)
}

# get xmin, ymin, xmax, ymax for city clustering method
city_extent <- extent(city_points)
xmin_cc <- city_extent@xmin
xmax_cc <- city_extent@xmax
ymin_cc <- city_extent@ymin
ymax_cc <- city_extent@ymax

coords_shp_file= get_urban_extent("Houston")

print(coords_shp_file)
#
xmin_shp <- coords_shp_file$lon[1]
xmax_shp <- coords_shp_file$lon[2]
ymin_shp <- coords_shp_file$lat[1]
ymax_shp <- coords_shp_file$lat[2]

rects <- data.frame(
  xmin = c(xmin_cc, xmin_shp),
  xmax = c(xmax_cc, xmax_shp),
  ymin = c(ymin_cc, ymin_shp),
  ymax = c(ymax_cc, ymax_shp),
  method = c("City Clustering", "Umich")
)

Houston_map <- get_stadiamap(
    bbox = bbox,
    zoom = 10,
    maptype = "outdoors"#"stamen_terrain"  # Options: stamen_terrain, stamen_toner, stamen_watercolor
)

m=ggmap(Houston_map)
    plot_name="/houston.png"
    ggsave(plot_name, plot = m, width = 8, height = 6, dpi = 300)

# }
   
# stop("Stopping execution for debugging purposes. Check the variables lons_foot, lats_foot, and footprint.")

for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)


# Print matching indices for verification
    print(ss) 
    print(matching_indices)


    # Houston_map <- get_googlemap(
    #   center = ss,
    #   zoom = 11,
    #   maptype = "roadmap"
    # )
  
    site="Houston"
  
    # one df for all xco2 values
    all_XCO2 <- data.frame()

    # pdf_filename <- paste0(city, "_XCO2_maps.pdf")
    # pdf(pdf_filename, width = 8, height = 6)

    # pdf_filename_emi <- paste0(site, "_emission_map.pdf")
    # pdf(pdf_filename_emi, width = 8, height = 6)
    
    # pdf_filename_emi_diff <- paste0(site, "_emission_diff.pdf")
    # pdf(pdf_filename_emi_diff, width = 8, height = 6)
    
   
    if (run_convolution) {
      pdf_filename_emi_conv <- paste0(site, "_emission_conv.pdf")
      pdf(pdf_filename_emi_conv, width = 8, height = 6)
    }

    if (run_enhancements) {
      pdf_filename_enh <- paste0(site, "_emission_enh.pdf")
      # pdf_filename_enh <- paste0(site, "_Epc_stats.pdf")
      pdf(pdf_filename_enh, width = 8, height = 6)
    }
    if (stats) {
      # pdf_filename_enh <- paste0(site, "_emission_enh.pdf")
      pdf_filename_enh <- paste0(site, "_Epc_stats.pdf")
      pdf(pdf_filename_enh, width = 8, height = 6)
    }
    if (seasonality) {
      # pdf_filename_enh <- paste0(site, "_emission_enh.pdf")
      pdf_filename_enh <- paste0(site, "_Epc_seasonality.pdf")
      pdf(pdf_filename_enh, width = 8, height = 6)
    }


    for (ii in matching_indices) {
   
        site=ss
        timestr=overpass_to_model$timestr[ii]
    
        if (timestr < '2015000000' || timestr %in% incomplete_runs$timestr) {
            print(paste0("skipping runs outside date range or failed runs for Timestr::", timestr))
            # print(paste0("CITY::", ss))
            next
        }
        month<-substr(timestr, 5,6)

        print(timestr)
        store.path  = file.path(OCO.DIR, '/XSTILT_output', site, oco.ver)
        out.path    = list.files(store.path, pattern = paste0('out_', timestr), full.names = T)
        print(out.path)
        # if (length(out.path) == 0) {
        #   warning(paste("No files found for pattern:", paste0("out_", timestr), "in", store.path))
        #   next
        # }

        if (run_convolution) {
          out.path <- out.path[!grepl("\\.png$", out.path)]
          byid.path = file.path(out.path, 'by-id')
          foot.fns  = list.files(byid.path, 'X_foot.nc', full.names = T, recursive = T)

          footprint_list <- list()
          print(paste0("Footprint files found: ", length(foot.fns)))
          
          # loop through the footprint files and read them and add them
          for (i in 1:length(foot.fns)) {
            footprint_file <- nc_open(foot.fns[i])
            lons_foot <- ncvar_get(footprint_file, varid = "lon")
            lats_foot <- ncvar_get(footprint_file, varid = "lat")
            footprint <- ncvar_get(footprint_file, varid = "foot")
            nc_close(footprint_file)
            
            # Sum footprint over the time dimension 
            if (length(dim(footprint)) == 3) {
              # Sum over time (3rd dimension)
              footprint <- apply(footprint, c(1, 2), sum,na.rm = TRUE)
            }
            
            # print(paste0("Footprint file: ", dim(footprint)))
            # Save footprint to list
            footprint_list[[i]] <- footprint
          }

          # Take the mean of all footprints for one overpass
          footprint_mean <- Reduce("+", footprint_list) / length(footprint_list)

          footprint_norm<-footprint_mean/sum(footprint_mean)

          # Create a raster from the footprint mean
          footprint_raster <- raster(footprint_norm, xmn = min(lons_foot), xmx = max(lons_foot), ymn = min(lats_foot), ymx = max(lats_foot), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
        }

        if (run_enhancements || stats || seasonality) {
          # Read in the enhancements data
          enh_file_shp <- file.path(enh_folder, paste0(site, "_", timestr, "_MC_brute_shp.txt"))
          enh_file_cc <- file.path(enh_folder, paste0(site, "_", timestr, "_MC_brute.txt"))
          if (file.exists(enh_file_cc) && file.exists(enh_file_shp)) {

            enh_shp_readin <- t(read.table(enh_file_shp, header = FALSE, sep = ",", skip = 1))
            enh_cc_readin <- t(read.table(enh_file_cc,header = FALSE, sep = ",", skip = 1) ) # transpose the table

            # Add column names to enh_cc_readin and enh_shp_readin
            colnames(enh_cc_readin) <- colnames_readin
            colnames(enh_shp_readin) <- colnames_readin

            # Store enhancements in lists in a loop
            enh_shp[[length(enh_shp) + 1]] <- enh_shp_readin
            enh_cc[[length(enh_cc) + 1]] <- enh_cc_readin
          } else {
            print(paste0("Enhancements file not found for site: ", site, " and timestr: ", timestr))
            next
          }


        }


        ## emissions
        if(timestr < '2024000000') {
          emiss_file = list.files(path = emiss.path, pattern = substr(timestr, 3, 6), 
                            recursive = T, full.names = T)
        }else{
          timestr_23 = "2023123100"

          print(paste0("Using 2023 emissions for timestr: ", timestr))
          emiss_file = list.files(path = emiss.path, pattern = substr(timestr_23, 3, 6), 
                            recursive = T, full.names = T)
        }

        # emissions
        emissions<-raster(emiss_file)

        emissions_cc<-crop(emissions, extent(min(cluster$long), max(cluster$long), min(cluster$lat),
                 max(cluster$lat)))
    

        # get emissions in umol/m2/s
        emissions_city_cc<-emission_conversion(emissions_cc)

        # get emissions for Umich method

# )
        # emissions_city_shp <- mask(crop(emissions, extent(urban_Houston_sp)), urban_Houston_sp)
        emissions_city_shp <- crop(emissions, extent(coords_shp_file$lon[1], coords_shp_file$lon[2], coords_shp_file$lat[1], coords_shp_file$lat[2]))
        emissions_city_shp<-emission_conversion(emissions_city_shp)

        # # Convolve the emissions with the footprint
        # emissions_cc_conv <- resample(emissions_city_cc, footprint_raster, method = "bilinear")
        # emissions_shp_conv <- resample(emissions_city_shp, footprint_raster, method = "bilinear")
        
        # # Convert convolved data to data frames for plotting
        # df_shp_conv <- as.data.frame(rasterToPoints(emissions_shp_conv))
        # colnames(df_shp_conv) <- c("lon", "lat", "value")
        # df_shp_conv$method <- "Umich"

        # df_cc_conv <- as.data.frame(rasterToPoints(emissions_cc_conv))
        # colnames(df_cc_conv) <- c("lon", "lat", "value")
        # df_cc_conv$method <- "City Clustering"

        # df_shp_conv$value[df_shp_conv$value <= 0] <- NA
        # df_cc_conv$value[df_cc_conv$value <= 0] <- NA
        
        #####----#####
        # convert emissions to data frame for plotting
        df_shp <- as.data.frame(rasterToPoints(emissions_city_shp))
        colnames(df_shp) <- c("lon", "lat", "value")
        df_shp$method <- "Umich"

        df_cc <- as.data.frame(rasterToPoints(emissions_city_cc))
        colnames(df_cc) <- c("lon", "lat", "value")
        df_cc$method <- "City Clustering"

        # Replace 0 or negative values with NA for log10 safety
        df_shp$value[df_shp$value <= 0] <- NA
        df_cc$value[df_cc$value <= 0] <- NA
        # Combine to get common color scale limits
        all_emissions <- c(df_shp$value, df_cc$value)
        common_limits <- range(all_emissions, na.rm = TRUE)


        xco2_fname=paste0(site,"_",timestr,".txt")
        XCO2_overpass <- read.table(paste0(file.path(OCO.DIR, "/OCO-2/overpass_obs/",xco2_fname)),
          header = TRUE)  
        all_XCO2 <- rbind(all_XCO2, XCO2_overpass)

        ## Toggle the commenting/uncommenting of the following lines to plot XCO2 maps or emission maps or emission difference maps

        # plot_xco2_maps(Houston_map, XCO2_overpass, rects, timestr)
      
        
        # emission_plot(Houston_map, df_shp, df_cc,
        #               xmin_shp,xmax_shp, ymin_shp, ymax_shp,
        #               xmin_cc,xmax_cc, ymin_cc, ymax_cc,
        #               timestr)
        
        
        # emission_diff(Houston_map, df_shp, df_cc,
        #               xmin_shp,xmax_shp, ymin_shp, ymax_shp,
        #               xmin_cc,xmax_cc, ymin_cc, ymax_cc,
        #               timestr)  

        # if runing run_convolution, plot the convolution
        if (run_convolution) {             
          emission_footprint_convolution(Houston_map, df_shp, df_cc,
                        xmin_shp,xmax_shp, ymin_shp, ymax_shp,
                        xmin_cc,xmax_cc, ymin_cc, ymax_cc,
                        timestr, footprint_raster)
        }

    }
}

# convert in to Dataframe
enh_shp_df <- do.call(rbind, lapply(enh_shp, as.data.frame))
enh_cc_df <- do.call(rbind, lapply(enh_cc, as.data.frame))

enh_shp_df$method <- "Umich"
enh_cc_df$method <- "City Clustering"

# Convert enh_mean to numeric (in case it's character)
enh_shp_df$enh_mean <- as.numeric(enh_shp_df$enh_mean)
enh_cc_df$enh_mean <- as.numeric(enh_cc_df$enh_mean)
enh_shp_df$int_epc_mean <- as.numeric(enh_shp_df$int_epc_mean)
enh_cc_df$int_epc_mean <- as.numeric(enh_cc_df$int_epc_mean)
enh_shp_df$pps_mean <- as.numeric(enh_shp_df$pps_mean)
enh_cc_df$pps_mean <- as.numeric(enh_cc_df$pps_mean)
enh_shp_df$pps_sd <- as.numeric(enh_shp_df$pps_sd)
enh_cc_df$pps_sd <- as.numeric(enh_cc_df$pps_sd)

if (run_enhancements) {
    # Add lon/lat columns to enh_shp_df
    enh_shp_df$lon <- runif(nrow(enh_shp_df), xmin_shp, xmax_shp)
    enh_shp_df$lat <- runif(nrow(enh_shp_df), ymin_shp, ymax_shp)

    enh_cc_df$lon <- runif(nrow(enh_cc_df), xmin_cc, xmax_cc)
    enh_cc_df$lat <- runif(nrow(enh_cc_df), ymin_cc, ymax_cc)
   # Add method labels
  

    # plot_pps_distribution(site, enh_shp_df, enh_cc_df)

    enhancements_plot(Houston_map, enh_shp_df, enh_cc_df,
                        xmin_shp,xmax_shp, ymin_shp, ymax_shp,
                        xmin_cc,xmax_cc, ymin_cc, ymax_cc,
                      rects)

  # --- Compare per capita emissions (int_epc_mean) across methods ---
}

if (stats) {
      # Check assumptions
      diff_epc <- enh_shp_df$int_epc_mean - enh_cc_df$int_epc_mean

      # 1. Shapiro-Wilk normality test on differences, if p > 0.05, data is normal
      shapiro_res <- shapiro.test(diff_epc)
      cat("\n[Shapiro-Wilk Test] p-value:", shapiro_res$p.value, "\n")

      # 2. Paired t-test (assumes normality of differences), if p < 0.05, significant difference
      ttest_res <- t.test(enh_shp_df$int_epc_mean, enh_cc_df$int_epc_mean,
                          paired = TRUE, alternative = "two.sided")
      cat("\n[Paired t-test] p-value:", ttest_res$p.value, "\n")

      # 3. Wilcoxon signed-rank test (non-parametric alternative), if p < 0.05, significant difference
      wilcox_res <- wilcox.test(enh_shp_df$int_epc_mean, enh_cc_df$int_epc_mean,
                                paired = TRUE, alternative = "two.sided")
      cat("\n[Wilcoxon signed-rank test] p-value:", wilcox_res$p.value, "\n")

        compare_df <- data.frame(
        Method = rep(c("Umich", "City Clustering"), each = nrow(enh_cc_df)),
        EPC = c(enh_shp_df$int_epc_mean, enh_cc_df$int_epc_mean)
      )

      p1=ggplot(compare_df, aes(x = Method, y = EPC, fill = Method)) +
        geom_violin(trim = FALSE, alpha = 0.6) +
        geom_boxplot(width = 0.2, outlier.shape = NA) +
        theme_minimal() +
        labs(title = "Per Capita CO₂ Emissions by Method",
            y = "Epc_mean (Tg CO₂ /cap/year)") +
        theme(legend.position = "none")

      diff_df <- data.frame(
        Mean = (enh_shp_df$int_epc_mean + enh_cc_df$int_epc_mean) / 2,
        Diff = enh_shp_df$int_epc_mean - enh_cc_df$int_epc_mean
      )

      p2=ggplot(diff_df, aes(x = Mean, y = Diff)) +
        geom_point(alpha = 0.6) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
        geom_hline(yintercept = mean(diff_df$Diff), color = "blue") +
        geom_hline(yintercept = mean(diff_df$Diff) + 1.96 * sd(diff_df$Diff), linetype = "dotted", color = "red") +
        geom_hline(yintercept = mean(diff_df$Diff) - 1.96 * sd(diff_df$Diff), linetype = "dotted", color = "red") +
        theme_minimal() +
        labs(
          title = "Difference Plot of Per Capita Emissions",
          x = "Mean of Methods",
          y = "Umich - City Clustering (int_epc_mean)"
      )

      print(p1)
      print(p2)
}    


if (seasonality) {

  # Add parsed dates
    enh_shp_df$datetime <- as.POSIXct(enh_shp_df$timestr, format = "%Y%m%d%H", tz = "UTC")
    enh_cc_df$datetime <- as.POSIXct(enh_cc_df$timestr, format = "%Y%m%d%H", tz = "UTC")

    enh_shp_df$month <- format(enh_shp_df$datetime, "%m")
    enh_cc_df$month <- format(enh_cc_df$datetime, "%m")

    enh_shp_df$year <- format(enh_shp_df$datetime, "%Y")
    enh_cc_df$year <- format(enh_cc_df$datetime, "%Y")

    # Combine for comparison
    epc_seasonal <- bind_rows(
      enh_shp_df %>% mutate(method = "Umich"),
      enh_cc_df %>% mutate(method = "City Clustering")
    )

    # Monthly summary
    monthly_summary <- epc_seasonal %>%
      group_by(method, month) %>%
      summarise(
        mean_epc = mean(int_epc_mean, na.rm = TRUE),
        median_epc = median(int_epc_mean, na.rm = TRUE),
        sd_epc = sd(int_epc_mean, na.rm = TRUE),
        n = n()
      )
   
   
    # Plot
    p1=ggplot(monthly_summary, aes(x = as.numeric(month), y = median_epc, color = method)) +
      geom_line(size = 1) +
      geom_point() +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      theme_minimal() +
      labs(
        title = "Seasonality of Per Capita CO₂ Emissions (Houston)",
        x = "Month",
        y = "Median int_epc_mean (Tg CO₂ /cap/year)"
      )


    heatmap_df <- epc_seasonal %>%
    group_by(method, year, month) %>%
    summarise(epc = mean(int_epc_mean, na.rm = TRUE)) %>%
    ungroup()

    p2=ggplot(heatmap_df, aes(x = month, y = year, fill = epc)) +
    geom_tile() +
    facet_wrap(~method) +
    scale_fill_viridis_c() +
    labs(
      title = "Monthly per capita CO₂ emissions (Epc) heatmap - Houston",
      x = "Month",
      y = "Year",
      fill = "Epc (Tg CO₂ /cap/year)"
    ) +
    theme_minimal()


    # Flag seasons for each row in epc_seasonal
    epc_seasonal$month_num <- as.numeric(epc_seasonal$month)
    epc_seasonal$season <- dplyr::case_when(
      epc_seasonal$month_num %in% c(12, 1, 2)  ~ "Winter",
      epc_seasonal$month_num %in% c(3, 4, 5)   ~ "Spring",
      epc_seasonal$month_num %in% c(6, 7, 8)   ~ "Summer",
      epc_seasonal$month_num %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ "Other"
    )
    print(epc_seasonal)
    # Compare distributions for all seasons
    p3 = ggplot(epc_seasonal, aes(x = season, y = int_epc_mean, fill = season)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.7) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
      theme_minimal() +
      labs(
      title = "Per Capita CO₂ Emissions by Season",
      x = "Season",
      y = "Epc (Tg CO₂ /cap/year)"
      ) +
      scale_fill_brewer(palette = "Set2") +
      theme(legend.position = "none")

    # Wilcoxon test: is Summer > Other?
    # Subset to only "Summer" and "Other" for Wilcoxon test
    # epc_seasonal_two <- epc_seasonal %>% filter(season %in% c("Summer", "Other"))
    # epc_seasonal_two$season <- droplevels(factor(epc_seasonal_two$season))
    # wilcox.test(int_epc_mean ~ season, data = epc_seasonal_two, alternative = "greater")
    print(p1)
    print(p2)
    print(p3)
}
dev.off()


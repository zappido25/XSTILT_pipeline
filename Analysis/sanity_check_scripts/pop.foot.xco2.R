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

#
population_density_data_file <-  "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
dens <- raster::brick(population_density_data_file)    
dens<-subset(dens, 1)

## ploting and parsing functions are here
functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
print(functions_files)

group = Sys.getenv("group")
version = Sys.getenv("version")
out.path=Sys.getenv("OUT_DF_DIR")
OCO.DIR = Sys.getenv("OCO2_DIR")


# Read in cities file and prompt user to select city
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_list.txt")
cities <- read.table(cities_file, header = FALSE, skip=1, stringsAsFactors = FALSE)[,1]
cat("Available cities:\n")
for (i in seq_along(cities)) {
  cat(sprintf("%d: %s\n", i, cities[i]))
}

# Force interactive prompt even under Rscript
cat("Enter the number of the city to analyze: ")
city_idx <- as.integer(readLines(file("stdin"), n = 1))

if (is.na(city_idx) || city_idx < 1 || city_idx > length(cities)) {
  stop("Invalid city selection.")
}
city <- cities[city_idx]
cat(paste0("Selected city: ", city, "\n"))


enhancement_cols = c("city", "timestr", "enh_mean", "enh_sd", "int_mean", 
                        "int_sd", "int_epc_mean", "int_epc_sd", "int_epc_gdp_mean", "int_epc_gdp_sd",
                          "int_gdp_epc_mean", "int_gdp_epc_sd","pps_mean", "pps_sd", 
                        "bg_pps_mean", "bg_pps_sd", "gdp_mean", "gdp_sd", "bg_gdp_mean", "bg_gdp_sd", 
                        "consumption_mean", "consumption_sd",
                          "city_fr_mean", "city_fr_sd", "lights_mean", "lights_sd", "na_count")


# save the results to a PDF in the folder "figures"
pdf_filename_emi_conv <- paste0("sanity_check_figures/", city, "_pop_clustering.pdf")
pdf(pdf_filename_emi_conv, width = 8, height = 6)

# get enhancements and sectoral data folders
enh_folder=file.path(out.path, "ENHANCEMENTS", city)
dataframe_folder=file.path(out.path, "DATAFRAME", city)

overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  



OCO.DIR = Sys.getenv("OCO2_DIR")


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

center_lon <- urban_core$lon[urban_core$city == city]
center_lat <- urban_core$lat[urban_core$city == city]

bbox <- c(
  left   = center_lon - half_width,
  bottom = center_lat - half_height,
  right  = center_lon + half_width,
  top    = center_lat + half_height
)

api.key = readLines('../../insert_ggAPI.csv')
register_google(key = api.key)
stadia.api.key = readLines('../../insert_stadia_key.csv')
register_stadiamaps(key = stadia.api.key)

## get geographic extent and city emissions for city clustering method 
cluster_code<-urban_core$ID[urban_core$city == city]
cluster<-cluster[cluster$cluster_id == cluster_code, ] 



city_points <- SpatialPointsDataFrame(coords = cluster[,1:2], data = cluster[,1:2],
                                          proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))




basemap <- get_stadiamap(
    bbox = bbox,
    zoom = 10,
    maptype = "outdoors"#"stamen_terrain"  # Options: stamen_terrain, stamen_toner, stamen_watercolor
)


for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)


# Print matching indices for verification
    print(ss) 
    print(matching_indices)

    # one df for all xco2 values
    all_XCO2 <- data.frame()


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
        if (length(out.path) == 0) {
          warning(paste("No files found for pattern:", paste0("out_", timestr), "in", store.path))
          next
        }

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

        #   footprint_norm<-footprint_mean/sum(footprint_mean)

          # Create a raster from the footprint mean
        footprint_raster <- raster(footprint_mean, xmn = min(lons_foot), xmx = max(lons_foot), ymn = min(lats_foot), ymx = max(lats_foot), crs = CRS("+proj=longlat +datum=WGS84 +no_defs"))
        

        # if (run_enhancements || stats || seasonality) {
        #   # Read in the enhancements data
        enh_file <- file.path(enh_folder, paste0(site, "_", timestr, "_MC_brute.txt"))
        dataframe_file <- file.path(dataframe_folder, paste0(site, "_", timestr, "_MC_brute.txt"))
        #   enh_file_cc <- file.path(enh_folder, paste0(site, "_", timestr, "_MC_brute.txt"))
        #   if (file.exists(enh_file_cc) && file.exists(enh_file_shp)) {

        enh_readin <- t(read.table(enh_file, header = FALSE, sep = ",", skip = 1))
        dataframe_readin <- (read.table(dataframe_file, header = TRUE, sep = "", stringsAsFactors = FALSE))
        #     enh_cc_readin <- t(read.table(enh_file_cc,header = FALSE, sep = ",", skip = 1) ) # transpose the table

        #     # Add column names to enh_cc_readin and enh_shp_readin
        colnames(enh_readin) <- enhancement_cols
        

        dataframe_readin= dataframe_readin %>% drop_na()
        

        xco2_fname=paste0(site,"_",timestr,".txt")
        XCO2_overpass <- read.table(paste0(file.path(OCO.DIR, "/OCO-2/overpass_obs/",xco2_fname)),
          header = TRUE)  
        all_XCO2 <- rbind(all_XCO2, XCO2_overpass)

        plot_pop=cluster.pop.density(dens,cluster, basemap, center_lat, center_lon, city_points, footprint_raster,
                        XCO2_overpass, timestr, site, dataframe_readin)    
        ## Toggle the commenting/uncommenting of the following lines to plot XCO2 maps or emission maps or emission difference maps
        print(plot_pop)      
        
    }
}



dev.off()


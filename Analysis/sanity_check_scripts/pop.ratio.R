library(ggplot2)
library(sp)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidygeocoder)
library(sf)
library(maps)
library(raster)

functions_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Analysis/functions/', full.names = TRUE, pattern = "\\.r$")
invisible(lapply(functions_files, source))
cities_file <- file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/simulated_cities.txt")

cities <- read.table(cities_file, header = FALSE, skip = 0, stringsAsFactors = FALSE)[, 1]

pdf_filename <- paste0("sanity_check_figures/", "population_ratio.pdf")
pdf(pdf_filename, width = 8, height = 6)
print(pdf_filename)

pop_file=read.csv("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/City_Population_By_Year.csv",
                    header = TRUE,stringsAsFactors = FALSE)
urban_core<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
cluster<-readRDS("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/possible_clusters_07212022.rds")
path_pop="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/"
population_density_data_file<-paste0(path_pop,"gpw_v4_population_density_rev11_2020_30_sec.tif")

# population_density_data_file <-  "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif"
gpw_dens <- raster::brick(population_density_data_file)    
gpw_dens<-subset(gpw_dens, 1)




for (city in cities) {
    message("Processing city: ", city)

  # Extract data for this city
    city_df <- pop_file[pop_file$city == city, ]
    gpw_pop <- urban_core$core_pop[urban_core$city == city]

    cluster_code<-urban_core$ID[urban_core$city == city]
    cluster_city<-cluster[cluster$cluster_id == cluster_code, ] 
    cluster_pop=urban_core$core_pop[urban_core$city == city]
    print(paste0("core pop ", cluster_pop," with gpw pop ",gpw_pop))
    

    city_points <- SpatialPointsDataFrame(coords = cluster_city[,1:2], data = cluster_city[,1:2],
                                            proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))

    #gpw city density
    gpw_dens_city<-pop.density(gpw_dens, cluster_city, city_points)
    gpw_total_pop <- sum(gpw_dens_city[[3]], na.rm = TRUE)
    print(paste0("GPW Total Population: ", gpw_total_pop))
  # Build plot
    p <- ggplot(city_df, aes(x = year, y = population)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_point(color = "red", size = 2) +
        # Add GPW (2020) reference point
        # geom_point(aes(x = 2020, y = gpw_pop), color = "black", size = 3, shape = 17) +
        geom_point(aes(x = 2020, y = gpw_total_pop), color = "black", size = 3, shape = 17) +
        # geom_text(aes(x = 2020, y = gpw_pop, label = "GPW 2020"),
        #         vjust = -1, color = "black", size = 3) +
        geom_text(aes(x = 2020, y = gpw_total_pop, label = "GPW"),
                vjust = -1, color = "black", size = 3) +
        ggtitle(paste("Population Trend for", city)) +
        xlab("Year") +
        ylab("Population") +
        scale_x_continuous(
        breaks = seq(min(city_df$year), max(city_df$year), by = 1),  # integer ticks
        labels = as.integer
        ) +
        theme_minimal(base_size = 13) +
        theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
        )

    print(p)
}

dev.off()
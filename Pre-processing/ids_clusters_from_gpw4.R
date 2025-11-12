# Step1: uses city clustering algorithm (cca) R pacakage to find the 3 km radius core with high density
# Step2: selects cities with total population > 1 million
# Step3 : uses k nearest neighbours to find the densest cluster for each city
# Step4: write the output to a csv file

library(raster)
library(osc)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(sf)
library(tidygeocoder)
library(FNN)

### Read in and subset data
dens <- raster::brick("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_density_rev11_2020_30_sec.tif")
dens<-subset(dens, 1)
pop <- raster::brick("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/gpw_v4_population_count_rev11_2020_30_sec.tif")
pop<-subset(pop, 1)

OCO.DIR = Sys.getenv("OCO2_DIR")
data <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
unique_cities <- data %>%
  select(site, lat, lon) %>%
  # mutate(lat = round(lat, 5), lon = round(lon, 5)) %>%
  mutate(lat = lat, lon = lon) %>%
  distinct()

# dens<-crop(dens, extent(-120,-65,20,50))
# pop<-crop(pop, extent(-120,-65,20,50))

### Perform city clustering and attach total population data
# Step 1
density_cluster<-cca(dens, s=3000, unit ="m", compare="g",cell.class = 1500)
clusters<-density_cluster$cluster
# extract values from population raster
ras<-extract(pop, cbind(clusters$long, clusters$lat))
clusters$total_population<-ras


# get cluster level population
clusters<- clusters %>% group_by(cluster_id) %>% mutate(cluster_pop = sum(total_population))

clusters<-as.data.frame(clusters)
# Step 2
big_clusters<-clusters[clusters$cluster_pop > 1000000, ] #threshold total pop

distinct_clusters=distinct(big_clusters[,colnames(clusters) %in% c("long", "lat","cluster_id", "cluster_pop")])

# Step 3 : nearest neighbor search
cluster_coords <- as.matrix(distinct_clusters[, c("lat", "long")])
city_coords <- as.matrix(unique_cities[, c("lat", "lon")])

# Find nearest cluster for each city
nn <- get.knnx(cluster_coords, city_coords, k = 1)

nearest_idx <- nn$nn.index[, 1]

matching_clusters <- cbind(unique_cities, distinct_clusters[nearest_idx, ])

# Remove duplicated columns if any
matching_clusters <- matching_clusters[, !duplicated(colnames(matching_clusters))]

# Optionally, add nearest_lat and nearest_lon columns if needed
matching_clusters$nearest_lat <- distinct_clusters$lat[nearest_idx]
matching_clusters$nearest_lon <- distinct_clusters$long[nearest_idx]
  


# test with 5 k-nearest neighbours 

# nn <- get.knnx(cluster_coords, city_coords, k = 5)
# nearest_indices <- nn$nn.index[1, ]  # indices of the 5 nearest clusters
# nearest_distances <- nn$nn.dist[1, ] # distances to those clusters

# # To create a mapping of each city to its 5 nearest clusters:
# city_cluster_pairs <- data.frame(
#   city = rep(unique_cities$site, each = 5),
#   city_lat = rep(unique_cities$lat, each = 5),
#   city_lon = rep(unique_cities$lon, each = 5),
#   cluster_id = distinct_clusters$cluster_id[as.vector(nn$nn.index)],
#   cluster_lat = distinct_clusters$lat[as.vector(nn$nn.index)],
#   cluster_long = distinct_clusters$long[as.vector(nn$nn.index)],
#   cluster_pop = distinct_clusters$cluster_pop[as.vector(nn$nn.index)],
#   distance = as.vector(nn$nn.dist)
# )

# Step 4: Write the output to a CSV file
output_file1 <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_population_updated_v2.csv"
write.csv(matching_clusters, output_file1, row.names = FALSE, quote = FALSE)
# output_file2 <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/cities_nnk5.csv"
# write.csv(city_cluster_pairs, output_file2, row.names = FALSE, quote = FALSE)

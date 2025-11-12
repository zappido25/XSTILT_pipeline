##### challenges
# getting city names from geocoding
# not getting colloquial city names (e.g., Millcreek instead of Salt Lake City)

# going to just go with coords and name later

library(gdalUtils)
library(rgdal)
library(raster)
library(osc)
library(dplyr)
library(ggplot2)
library(maps)
library(ggmap)
library(tidygeocoder)
register_google(key = "AIzaSyBVpnuLVJlQkU6_85b5BPcirG3Snu43C2M")
Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyBVpnuLVJlQkU6_85b5BPcirG3Snu43C2M")

### Read in and subset data
population_count_data_file <- file.path(base_dir, "gpw_v4_population_count_rev11_2020_30_sec.tif")
population_density_data_file <- file.path(base_dir, "gpw_v4_population_density_rev11_2020_30_sec.tif")


dens <- raster::brick(population_density_data_file) 
dens<-subset(dens, 1)

pop <- raster::brick(population_count_data_file)    
pop<-subset(pop, 1)


# may need different thresholds by continent/geographic region to get good spread
# could be challenging for cities with multiple urban cores
# do I need my cities isolated from nearby urban influences? could implement some sort of distancing? 


### Perform city clustering and attach total population data
density_cluster<-cca(dens, s=3000, unit ="m", compare="g",cell.class = 1500)
clusters<-density_cluster$cluster

# extract values from population raster
ras<-extract(pop, cbind(clusters$long, clusters$lat))
clusters$total_population<-ras

# get cluster level population
clusters<- clusters %>% group_by(cluster_id) %>% mutate(cluster_pop = sum(total_population))

clusters<-as.data.frame(clusters)

### look at city totals
# keep in mind these clusters aren't going to be the whole city, just meant to identify major urban cores
# think I need variable values for different regions to try to keep a good spread for downstream analyses
# also tough because we probably want a range of densities/sizes within an urban system
# 1 million give 5 cities in Oceania - Australia seems important given climate change rhetoric
# 1.5 million gives 22 in North America
# 2 million gives us 19 in North America, 25 in Europe, 33 South America
# 3 million gives us 20 in South America, 29 for Africa
# 5 million give us 50 in Asia

big_clusters<-clusters[clusters$cluster_pop > 50000, ] #threshold total pop

# check out totals
cities<-distinct(big_clusters[,colnames(clusters) %in% c("cluster_id", "cluster_pop")])
cities<-cities[order(cities$cluster_pop),]
cities<-cities[length(cities$cluster_id):1, ]


# get some general characteristics of each cluster
locations<-NULL
for(i in unique(big_clusters$cluster_id)){
  i_clust<-big_clusters[big_clusters$cluster_id == i, ]
  vect<-c(i, mean(i_clust$long), mean(i_clust$lat), diff(range(i_clust$long)), diff(range(i_clust$lat)))
  locations<-rbind(locations, vect)
}

locations<-as.data.frame(locations)
colnames(locations)<-c("cluster","mean_lon","mean_lat","range_lon","range_lat")
# add a component for distance to closest other cluster

# get location continent and country
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}
spdf <- SpatialPointsDataFrame(coords = big_clusters[,1:2], data = big_clusters,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

cont<-coords2continent(spdf)
big_clusters$continent<-cont


### country - continent counts
continent<-distinct(big_clusters[,c(3, 6)])
continent<-na.omit(continent)

table(continent$continent)

# ### create vectors for continent based thresholds
# asia<-continent$cluster_id[continent$continent == "Asia"] #5M

# africa<-continent$cluster_id[continent$continent == "Africa"] #3M

# Samerica<-continent$cluster_id[continent$continent == "South America"] #2M

# europe<-continent$cluster_id[continent$continent == "Europe"] #1.5M
Nam-continent$cluster_id[continent$continent == "North America"]

# Australia<-continent$cluster_id[continent$continent == "Australia"] #1M

# IDs<-c(asia, africa, Samerica, europe, Namerica, Australia)
IDs<-c( Nam)

saveRDS(IDs, file="possible_IDS_2025.rds")
saveRDS(clusters, file="possible_clusters_2025.rds")

sub_locations<-locations[locations$cluster %in% Nam, ]
### plot each location
library(ggplot2)
library(ggmap)
library(rworldmap)
dens_context<-readRDS("pop_dens_500.rds")
count=0
for(i in sub_locations$cluster){
  count=count+1
  print(count)
  #if(count == 40){break()}
  i_cluster<-big_clusters[big_clusters$cluster_id == i,   ]
  
  p<-ggmap(get_googlemap(center = c(lon = mean(i_cluster$long), lat = mean(i_cluster$lat)),
                zoom = 8))

  context<-dens_context[abs(dens_context$x -  mean(i_cluster$long)) < 2 & abs(dens_context$y -  mean(i_cluster$lat)) < 2, ]
  context$gpw_v4_population_density_rev11_2020_30_sec[context$gpw_v4_population_density_rev11_2020_30_sec > 1500]<-1500
  colnames(context)[3]<-"dens"
  
  p<-p+geom_tile(aes(x = x, y=y, fill = dens), data=context, height = 0.00833, width = 0.00833, alpha=0.6)+
    scale_fill_gradientn(limits = c(500, 1500), colors=c("blue", "orange"))
  
  
  
  q<-p+geom_tile(aes(x = long, y=lat), data=i_cluster, height = 0.00833, width = 0.00833, color = "darkred", fill = "red", alpha=0.4)
  
  ggsave(file = paste0("cluster_figs/cluster_",i,".png"))
}

# locations to be removed if:
# a) No real hope for a "background" - perhaps subset to some edge defined by a geographic barrier? 
# b) Plot is too uniform and obviously calls into question the GPW4 data
# c) Not enough overpasses
  # c2) backward trajs do not hit urban center for enough overpasses - probably just filter based on where has the most
# Could perform some type of sub clustering to identify the densest core of each urban area to site the "urban core"


# plot all locations

# plotting 
world<-map_data("world")

p<-ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  )

q<-p+geom_point(aes(x = mean_lon, y=mean_lat), color="red", data = locations)


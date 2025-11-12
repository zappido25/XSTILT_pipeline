
library(raster)
library(tabularaster)
library(sp)

# for plotting
library(ggplot2)
library(ggmap)
library(rworldmap)
# api.key = readLines('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/insert_ggAPI.csv')
register_google(key = "AIzaSyBVpnuLVJlQkU6_85b5BPcirG3Snu43C2M")
Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyBVpnuLVJlQkU6_85b5BPcirG3Snu43C2M")

## directory paths 
base_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data"
plot_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GPW_V4_population_data/plots_update/"
pre_processing_outfiles="/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files"

## The possible_clusters_07212022.rds file is a list of all the clusters that were identified in the GPW4 data
## which is created using cca.R script in "/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/city_selection/city_clustering"
## possible_clusters_07212022.rds contains total population of the cluster and sum(total_pop) for cluster id
cluster_data_file <- file.path(base_dir, "possible_clusters_07212022.rds")
population_count_data_file <- file.path(base_dir, "gpw_v4_population_count_rev11_2020_30_sec.tif")
population_density_data_file <- file.path(base_dir, "gpw_v4_population_density_rev11_2020_30_sec.tif")


Conus_data <- read.csv(file.path("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/CONUS_cities_population_updated_v2.csv"), stringsAsFactors = FALSE)


## save outfile urban_core_info.txt
urban_core_outfile=file.path(pre_processing_outfiles, "urban_core_info_check.txt")

# read in cluster data for our cities
data<-readRDS(cluster_data_file)

# all population data
pop <- raster::brick(population_count_data_file)    
pop<-subset(pop, 1)

dens <- raster::brick(population_density_data_file)    
dens<-subset(dens, 1)


print(pop)


# cluster IDs and city names
# For cities that passed visual inspection of GPW4 clustering
  # tossed out if too uniform or very large administrative boundaries with nearly even values
  # Also if administrative boundary is nearly constant

# Australia
# australia<-data.frame(city = c("Melbourne", "Auckland", "Sydney", "Perth", "Brisbane"), 
#                       ID = c(77, 108, 211, 417, 913))
# australia$continent<-"aus"

# North America - only USA and Canada for some reason
north_america<-data.frame(city = Conus_data$site, 
                          ID = Conus_data$cluster_id)
# print(north_america)

# north_america<-data.frame(city = c("East_FL", "SanAntonio", "Houston", "ELPaso_CiudadJuarez", "SanDiego_Tiujana", "Dallas_FortWorth", "Phoenix",
#                                   "LosAngeles" , "LasVegas", "SanFrancisco_SanJose", "Sacramento", "WashingtonDC", "Denver", "Philadelphia", 
#                                   "NewYork", "Chicago", "Detroit", "Boston", "Toronto", "Montreal", "Seattle", "Vancouver", "SaltLakeCity"), 
#                           ID = c(9828, 10566, 10599, 11089, 11259, 11332, 11524, 11648, 12443, 12845, 13368, 13524, 13944, 14023, 14384,
#                                  15307, 15720, 15747, 16301, 17516, 18204, 18512, 14615))

north_america$continent<-"Nam"

# South America
# south_america<-data.frame(city = c("BuenosAires", "Santiago", "PortoAlegre", "Curitiba", "Asuncion", "SaoPaulo", "Rio", "BeloHorizonte", "Goiania",
#                                    "Salvador", "Lima", "Recife", "Fortaleza", "Manaus", "Belem", "Quito", "Medellin", "Barranquilla", "Guatemala",
#                                    "SantoDomingo", "Port_au_Prince", "MexicoCity", "Guadalajara", "Monterrey"), 
#                           ID = c(143, 265, 566, 1289, 1319, 1595, 1877, 2809, 3211, 3707, 3808, 4469, 5366, 5531, 5727, 5850, 6158, 6791, 7270, 
#                                  8111, 8185, 8450, 8993, 9860))

# south_america$continent<-"Sam_Lam"

# # Africa 
# # Maybe break Cairo into pieces? 
# # Really suffered from suspect data quality in GPW4
# africa<-data.frame(city = c("CapeTown", "Johannesburg", "Antananarivo", "Dar_es_Salaam", "Nairobi", "Kampala", "Kano", "Alexandria", "Casablanca",
#                             "Algiers", "Onitsha", "Lagos", "Addis Ababa", "Luxor_Qena", "Cairo"), 
#                    ID = c(206, 1086, 2983, 4764, 5741, 5894, 6970, 11009, 11621, 12604, 6126, 6189, 6520, 9855, 9948))

# africa$continent<-"afr"

# # Europe
# # Maybe break cologne into pieces? 
# # Maybe break up Leeds and Sheffield? 
# # Maybe break up Liverpool and Manchester? - only keep 1 of this and Leeds Sheffield
# europe<- data.frame(city = c("Athens", "Lisbon", "Valencia", "Madrid", "Baku", "Naples", "Barcelona", "Rome", "Bucharest", "Milan",
#                              "Budapest", "Munich", "Vienna", "Paris", "Cologne", "London", "Rotterdam", "Warsaw", "Birmingham", "Leeds_Sheffield",
#                              "Liverpool_Manchester", "Moscow", "StPetersburg", "TelAviv_Yafo", "GazaStrip"), 
#                     ID = c(13124, 13441, 13818, 14408, 14477, 14768, 15191, 15462, 16874, 17385, 18257, 18363, 18365, 18449, 
#                            18786, 18916, 19133, 19244, 19330, 19607, 19647, 20030, 20335, 11131, 11035))

# europe$continent<-"eur"

# # Asia
# # maybe break up Kochi
# #Careful breaking up Dhaka
# # Maybe break up Hong Kong and Guangzhou
# # Break up of Patna
# asia<- data.frame(city = c("Jakarta", "Singapore", "KualaLumpur_PetalingJaya", "Kochi", "HoChiMinhCity", "Chennai", "Bangkok",
#                            "Manila", "Yangon", "Hyderbad", "Mumbai", "Hanoi", "Kolkata", "Dhaka", "Chattogram", "HongKong_Guangzhou",
#                            "Ahmedabad", "Shantou", "Xiamen", "Taipei", "Patna", "Wenzhou", "Kathmandu", "NewDelhi", "Wuhan", 
#                            "Chengdu", "Shanghai", "Nanjing", "Baghdad", "XiAn", "Osaka", "Zhengzhou", "Tokyo", "Seoul", 
#                            "Beijing", "Shenyang"), 
#                   ID = c(4819, 5964, 6010, 6473, 6753, 7078, 7135, 7179, 7663, 7787, 8319, 9152, 9429, 9514, 9446, 9473, 9565, 
#                          9587, 9701, 9718, 9773, 10205, 10237, 10381, 10802, 10869, 10964, 11142, 11516, 11824, 11904, 11991,
#                          12186, 12763, 14046, 15433))

# asia$continent<-"asi"

# 128 cities as it stands - possibly a few more if we break up a few of the large conglomerates
# cities<-rbind(australia, north_america, south_america, europe, africa, asia)
## for these runs only consider NAM cities
cities<-rbind( north_america)


# perform sub-clustering for each city to identify the densest urban core - will be release point for forward trajs
  # For the conglomerates we may identify a few to break them up a bit
  # Ultimately want the center coordinates of the densest 0.3x0.3 degree point

  cities$lon<-NA
  cities$lat<-NA
  cities$core_pop<-NA

  for(c in cities$ID){
    print(cities$city)
    print(cities$city[cities$ID == c])
    # get city cluster data
    cluster<-data[data$cluster_id == c, ]
    
    # get population data in rectangle surrounding city
    # may need to make this larger so it doesn't die in the loop below for very small cities? 
    # Something is pulling Cairo super hard toward NE
    i_pop<-crop(pop, extent(min(cluster$long)-0.15, max(cluster$long)+0.15, min(cluster$lat)-0.15, max(cluster$lat)+0.15))
    i_pop[is.na(i_pop[])]<-0 # this keeps all our coords
    
    # this is where we are losing coords
    coords<-rasterToPoints(i_pop)
    coords<-as.data.frame(coords)
    lons<-unique(coords$x)
    lats<-unique(coords$y)
    
    city_points <- SpatialPointsDataFrame(coords = cluster[,1:2], data = cluster[,1:2],
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
    
    # get raster cells with a city grid point in them
    city<- extract(i_pop, city_points, cellnumbers=TRUE)[,"cells"]
    all<-1:(dim(i_pop)[1]*dim(i_pop)[2])
    not_city<-setdiff(all, city)
    
    # set places not in clity cluster equal to 0
    # Don't want a tiny pocket of super high density near but not in the city to be flagged as the densest urban core
    i_pop[not_city] <- 0
    
    # want to find densest 36x36 pixel region of raster essentially
    # assuming all pixels are the same area with this small window - tiny effects pf meridians converging
    
    
    i_pop<-as.matrix(i_pop) 
    values<-NULL
    for(i in 36:dim(i_pop)[1]){
      print(i)
      for(j in 36:dim(i_pop)[2]){
        # assuming area per grid cell constant - or close enough
        sub_pop<-sum(i_pop[(i-35):i, (j-35):j])
        values<-rbind(values, c(i, j, sub_pop))
      }
    }
    values<-as.data.frame(values)
    colnames(values)<-c("i","j", "pop")
    
    # this is where I could pick out multiples for the conglomerate clusters later
    # get value and location of the max - location here is the lower right corner of this box, get center
    values<-values[values$pop == max(values$pop), ]
    
    # want middle value
    center_lon<-mean(lons[(values$j[1]-18): (values$j[1]-17)])
    center_lat<-mean(lats[(values$i[1]-18): (values$i[1]-17)])
    
    # record info
    cities$lon[cities$ID == c]<-center_lon
    cities$lat[cities$ID == c]<-center_lat
    cities$core_pop[cities$ID == c]<-values$pop[1]
    
    # create some plots
    p<-ggmap(get_googlemap(center = c(lon = center_lon, lat = center_lat),
                             zoom = 8))
    
    
    # to provide some context for density here
    i_dens<-crop(dens, extent(min(cluster$long)-0.15, max(cluster$long)+0.15, min(cluster$lat)-0.15, max(cluster$lat)+0.15))
    
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
    q<-p+geom_tile(aes(x = x, y=y, fill = dens), data=context[context$city == FALSE,], height = 0.00833, width = 0.00833, alpha = 0.3)+
      scale_fill_gradientn(limits = c(min(context$dens), max(context$dens)),colors=c("blue", "orange"))
      
    q<-q+geom_tile(aes(x = x, y=y, fill = dens), data=context[context$city == TRUE,], height = 0.00833, width = 0.00833, alpha = 0.7)+
      scale_fill_gradientn(limits = c(min(context$dens), max(context$dens)),colors=c("blue", "orange"))
    
    # add box for urban core
    center<-data.frame(x = center_lon, y = center_lat)
    r<-q+geom_tile(aes(x = x, y = y), data = center, color = "black", linewidth = 1.1, fill = NA, height = 0.3, width = 0.3)
      
      
    ggsave(file = file.path(plot_dir, "cluster_figs/urban_core", paste0(cities$city[cities$ID == c], ".png")), width = 5, height = 4)
    
    
  }
  
 write.table(cities, file= urban_core_outfile, quote=F, row.names = F)
pop.density=function(dens, cluster, city_points){
    #crop pop density down to cluster city lat lon
    threshold=0.25#0.15
    i_dens<-crop(dens, extent(min(cluster$long)-threshold, max(cluster$long)+threshold, min(cluster$lat)-threshold, max(cluster$lat)+threshold))
    
    city<- raster::extract(i_dens, city_points, cellnumbers=TRUE)[,"cells"]
    all<-1:(dim(i_dens)[1]*dim(i_dens)[2])
    not_city<-setdiff(all, city)
    
    city_dens<-i_dens
    city_dens[not_city]<-NA
    city_dens<-rasterToPoints(city_dens)
    city_dens<-as.data.frame(city_dens)
    city_dens<-na.omit(city_dens)
        
    return(city_dens)
}
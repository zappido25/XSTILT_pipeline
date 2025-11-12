# This is the version Carlton modified based on Kai's overpass_values_func.R

###
# perhaps use standard deviations in bins to capture uncertainty for various variables
# conversions on consumption


overpass_values<-function(city, timestr, dir, out_path){
  library(stringr);library(dplyr);library(rioja);library(stats)
  
  #### get latitude range of interest for the city
  lat_range<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
  lat_range<-lat_range$lat[lat_range$city == city]
  lat_range<-seq(lat_range - 1.55, lat_range + 1.55, 0.05)
  lat_seq<-lat_range[1:(length(lat_range) -1)]+0.025 # midpoints of intended 0.05 degree width bins
  lat_df<-data.frame(bin = 1:length(lat_seq), lat_mid = lat_seq)
 
  #### get observations
  obs_file<-paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/OCO-2/overpass_obs/",city,"_", timestr,".txt")
  obs<-read.table(obs_file, header=T, stringsAsFactors = F)
  obs$lat_bin<-NA
  for(i in 1:length(obs$lat)){
    obs$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - obs$lat[i]) <= 0.025]
  }
  
  # colnames(obs)[c(3,4,5)]<-c("XCO2","XCO2_uncert", "time_utc")
  
 
  # some overpasses have huge ranges within a given latitude bin ~10 ppm
  # deal with outliers 
  # straight up remove obs that are 3 SD above/below median for whole overpass
  obs_sd<-sd(obs$XCO2)*3
  obs_med<-median(obs$XCO2)
 
  obs<-obs[obs$XCO2 > (obs_med - obs_sd) & obs$XCO2 < (obs_med + obs_sd), ]
  obs_adj<-NULL
  

  for(i in unique(obs$lat_bin)){
    i_obs<-obs[obs$lat_bin == i, ]

    # latitude bins with 4 or more soundings
    if(length(i_obs$XCO2) > 3){
      sd<-sd(i_obs$XCO2)
      median<-median(i_obs$XCO2) # so not swayed by outlier
      i_obs$XCO2[i_obs$XCO2 > (median + sd*2) | i_obs$XCO2 < (median - sd*2)]<-median
    }
    # latitude bins with 3 or less soundings
    if(length(i_obs$XCO2) < 4){
      sd<-sd(obs$XCO2[abs(obs$lat_bin - i) < 2])
      median<-median(obs$XCO2[abs(obs$lat_bin - i) < 2]) # so not swayed by outlier
      i_obs$XCO2[i_obs$XCO2 > (median + sd*2) | i_obs$XCO2 < (median - sd*2)]<-median
    }
    
    obs_adj<-rbind(obs_adj, i_obs)
  }

  obs<-obs_adj
  # take means by latitude bins
  obs<- obs %>% group_by(lat_bin) %>% mutate(mean_XCO2 = mean(XCO2), mean_XCO2.uncert = mean(XCO2_uncert))
  obs<-as.data.frame(obs)
  obs<-obs[,c(6,7,8)]
  obs<-distinct(obs)
  
  
  #### get nhrs
  nhrs<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/Phoenix/V11r/out_2021120320_hrrr_OCO-2/nhours_results.txt", 
              header=TRUE, quote="")
  nhrs<-nhrs$nhrs_thresh[nhrs$city == city & nhrs$timestr == timestr]


  #### get ODIAC enhancements
  
  # city for nhrs
  ODIAC_city_files<-dir(dir,recursive =T, pattern = "city", full.names=T)
  ODIAC_city_lon<-as.numeric(unlist(str_split(basename(dirname(ODIAC_city_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(ODIAC_city_files)),"_"))), 4)])
  ODIAC_city_lat<-as.numeric(unlist(str_split(basename(dirname(ODIAC_city_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(ODIAC_city_files)),"_"))), 4)])

  vect<-c()
  for(i in ODIAC_city_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    
    # get the hours we are interested - deal with missing hours? 
    i_data<-i_data[i_data$hour %in% (24-nhrs+1):24, ]
    i_data[is.na(i_data)]<-0
    val<-sum(i_data$conv)
    vect<-c(vect, val)
  }
 
  ODIAC_city_df<-data.frame(lon = ODIAC_city_lon, lat = ODIAC_city_lat, ppm = vect)
  ODIAC_city_df$lat_bin<-NA
  for(i in 1:length(ODIAC_city_df$lat)){
    ODIAC_city_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - ODIAC_city_df$lat[i]) <= 0.025]
  }
  # take means by latitude bins
  ODIAC_city_df<- ODIAC_city_df %>% group_by(lat_bin) %>% mutate(city_ppm = mean(ppm), city_sd = sd(ppm))
  ODIAC_city_df<-as.data.frame(ODIAC_city_df)
  ODIAC_city_df<-ODIAC_city_df[,c(4,5,6)]
  ODIAC_city_df$city_sd[is.na(ODIAC_city_df$city_sd)]<-0
  ODIAC_city_df<-distinct(ODIAC_city_df)
  
 
  # total for nhrs and total for 24 hours
  ODIAC_enhancement_files<-dir(dir,recursive =T, pattern = "urban", full.names=T)
  ODIAC_enhancement_lon<-as.numeric(unlist(str_split(basename(dirname(ODIAC_enhancement_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(ODIAC_enhancement_files)),"_"))), 4)])
  ODIAC_enhancement_lat<-as.numeric(unlist(str_split(basename(dirname(ODIAC_enhancement_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(ODIAC_enhancement_files)),"_"))), 4)])
  vect<-c()
  vect_total<-c()
  
  for(i in ODIAC_enhancement_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    
    # total 
    vect_total<-c(vect_total, sum(i_data$conv, na.rm=T)) # base background on last 24 hours worth of odiac enhancements
    
    # get the hours we are interested - deal with missing hours? 
    i_data<-i_data[i_data$hour %in% (24-nhrs+1):24, ]
    i_data[is.na(i_data)]<-0
    val<-sum(i_data$conv)
    vect<-c(vect, val)
  }
  ODIAC_enhancement_df<-data.frame(lon = ODIAC_enhancement_lon, lat = ODIAC_enhancement_lat, ppm = vect, ppm_total = vect_total)
  ODIAC_enhancement_df$lat_bin<-NA
  for(i in 1:length(ODIAC_enhancement_df$lat)){
    ODIAC_enhancement_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - ODIAC_enhancement_df$lat[i]) <= 0.025]
  }
  # take means by latitude bins
  ODIAC_enhancement_df<- ODIAC_enhancement_df %>% group_by(lat_bin) %>% mutate(odiac_ppm = mean(ppm), odiac_sd = sd(ppm), odiac_total = mean(ppm_total), odiac_tot_sd = sd(ppm_total), recp_num = length(lat_bin))
  ODIAC_enhancement_df<-as.data.frame(ODIAC_enhancement_df)
  ODIAC_enhancement_df<-ODIAC_enhancement_df[,c(5:10)]
  ODIAC_enhancement_df$odiac_sd[is.na(ODIAC_enhancement_df$odiac_sd)]<-0
  ODIAC_enhancement_df$odiac_tot_sd[is.na(ODIAC_enhancement_df$odiac_tot_sd)]<-0
  ODIAC_enhancement_df<-distinct(ODIAC_enhancement_df)
  
  #### get bio enhancements
  bio_files<-dir(dir, recursive =T, pattern = "bio", full.names=T)
  bio_lon<-as.numeric(unlist(str_split(basename(dirname(bio_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(bio_files)),"_"))), 4)])
  bio_lat<-as.numeric(unlist(str_split(basename(dirname(bio_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(bio_files)),"_"))), 4)])
  vect<-c()
  vect_uncert<-c()
  for(i in bio_files){
    i_data<-try(read.table(i, header=T, stringsAsFactors = F), silent=T)
    #print(i_data)
    mean<-NA
    sd<-NA
    
    if(length(i_data$x) > 1){
      mean<-i_data$x[1]
      if(is.na(mean)){mean <- i_data$x[3]}
      sd<-i_data$x[2]
    }
    
    vect<-c(vect, mean)
    vect_uncert<-c(vect_uncert, sd)
  }
  bio_df<-data.frame(lon = bio_lon, lat = bio_lat, ppm = vect, uncert = vect_uncert)
  bio_df$lat_bin<-NA
  for(i in 1:length(bio_df$lat)){
    bio_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - bio_df$lat[i]) <= 0.025]
  }
  bio_df<- bio_df %>% group_by(lat_bin) %>% mutate(bio_ppm = mean(ppm), bio_uncert = mean(uncert))
  bio_df<-as.data.frame(bio_df)
  bio_df<-bio_df[,c(5,6,7)]
  bio_df<-distinct(bio_df)
  

  
  #### get EDGAR sectoral breakdown of enhancements
  edgar_files<-dir(dir, recursive =T, pattern = "edgar", full.names=T)
  edgar_lon<-as.numeric(unlist(str_split(basename(dirname(edgar_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(edgar_files)),"_"))), 4)])
  edgar_lat<-as.numeric(unlist(str_split(basename(dirname(edgar_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(edgar_files)),"_"))), 4)])
  
  # get EDGAR total enhancements
  vect<-c()
  for(i in edgar_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-sum(i_data$ppm)/(dim(i_data)[1]/17)
    vect<-c(vect, val)
  }
  edgar_df<-data.frame(lon = edgar_lon, lat = edgar_lat, ppm = vect)
  edgar_df$lat_bin<-NA
  for(i in 1:length(edgar_df$lat)){
    edgar_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - edgar_df$lat[i]) <= 0.025]
  }
  edgar_df<- edgar_df %>% group_by(lat_bin) %>% mutate(edgar_ppm = mean(ppm), sd_edgar = sd(ppm))
  edgar_df<-as.data.frame(edgar_df)
  edgar_df<-edgar_df[,c(4,5,6)]
  edgar_df$sd_edgar[is.na(edgar_df$sd_edgar)]<-0
  edgar_df<-distinct(edgar_df)
  
  # get dataframe of average sectoral breakdown for latitude bins
  sect<-NULL
  for(i in edgar_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    i_data<- i_data %>% group_by(sect) %>% mutate(ppm = mean(ppm))
    i_data<-distinct(as.data.frame(i_data))
    
    sect<-rbind(sect, i_data$ppm)
  }
  sect<-as.data.frame(sect)
  colnames(sect)<-i_data$sect
  sect<-cbind(edgar_lon, edgar_lat, sect)
  colnames(sect)[1:2]<-c("lon","lat")
  sect$lat_bin<-NA
  for(i in 1:length(sect$lat)){
    sect$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - sect$lat[i]) <= 0.025]
  }
  
  
  sect<- sect %>% group_by(lat_bin) %>% mutate(CHE = mean(CHE), ENE = mean(ENE), 
                                               IND = mean(IND), IRO = mean(IRO),
                                               NEU = mean(NEU), NFE = mean(NFE), 
                                               NMM = mean(NMM), PRO = mean(PRO), 
                                               PRU_SOL = mean(PRU_SOL), RCO = mean(RCO),
                                               REF_TRF = mean(REF_TRF), TNR_Aviation_CDS = mean(TNR_Aviation_CDS),
                                               TNR_Aviation_CRS = mean(TNR_Aviation_CRS), TNR_Aviation_LTO = mean(TNR_Aviation_LTO),
                                               TNR_Other = mean(TNR_Other), TNR_Ship = mean(TNR_Ship), 
                                               TRO_noRES = mean(TRO_noRES))
  
  sect<-as.data.frame(sect)
  sect<-sect[,3:20]
  sect<-distinct(sect)
  
  #### get effective population densities
  pps_files<-dir(dir, recursive =T, pattern = "pps", full.names=T)
  pps_lon<-as.numeric(unlist(str_split(basename(dirname(pps_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(pps_files)),"_"))), 4)])
  pps_lat<-as.numeric(unlist(str_split(basename(dirname(pps_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(pps_files)),"_"))), 4)])
  vect<-c()
  for(i in pps_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1]
    vect<-c(vect, val)
  }
  pps_df<-data.frame(lon = pps_lon, lat = pps_lat, pps = vect)
  pps_df$lat_bin<-NA
  for(i in 1:length(pps_df$lat)){
    pps_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - pps_df$lat[i]) <= 0.025]
  }
  pps_df<- pps_df %>% group_by(lat_bin) %>% mutate(mean_pps = mean(pps), sd_pps =sd(pps))
  pps_df<-as.data.frame(pps_df)
  pps_df<-pps_df[,c(4,5,6)]
  pps_df$sd_pps[is.na(pps_df$sd_pps)]<-0
  pps_df<-distinct(pps_df)
  
  #### get effective gdp
  gdp_files<-dir(dir, recursive =T, pattern = "gdp", full.names=T)
  gdp_lon<-as.numeric(unlist(str_split(basename(dirname(gdp_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(gdp_files)),"_"))), 4)])
  gdp_lat<-as.numeric(unlist(str_split(basename(dirname(gdp_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(gdp_files)),"_"))), 4)])
  vect<-c()
  for(i in gdp_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1]
    vect<-c(vect, val)
  }
  gdp_df<-data.frame(lon = gdp_lon, lat = gdp_lat, gdp = vect)
  gdp_df$lat_bin<-NA
  for(i in 1:length(gdp_df$lat)){
    gdp_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - gdp_df$lat[i]) <= 0.025]
  }
  gdp_df<- gdp_df %>% group_by(lat_bin) %>% mutate(mean_gdp = mean(gdp))
  gdp_df<-as.data.frame(gdp_df)
  gdp_df<-gdp_df[,c(4,5)]
  gdp_df<-distinct(gdp_df)
  
  #### get consumption level foots
  consumption_files<-dir(dir, recursive =T, pattern = "consumption", full.names=T)
  consumption_lon<-as.numeric(unlist(str_split(basename(dirname(consumption_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(consumption_files)),"_"))), 4)])
  consumption_lat<-as.numeric(unlist(str_split(basename(dirname(consumption_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(consumption_files)),"_"))), 4)])
  vect<-c()
  for(i in consumption_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1] # may need to do a conversion here
    vect<-c(vect, val)
  }
  consumption_df<-data.frame(lon = consumption_lon, lat = consumption_lat, consumption = vect)
  consumption_df$lat_bin<-NA
  for(i in 1:length(consumption_df$lat)){
    consumption_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - consumption_df$lat[i]) <= 0.025]
  }
  consumption_df<- consumption_df %>% group_by(lat_bin) %>% mutate(mean_consumption = mean(consumption))
  consumption_df<-as.data.frame(consumption_df)
  consumption_df<-consumption_df[,c(4,5)]
  consumption_df<-distinct(consumption_df)
  
  #### get lights
  lights_files<-dir(dir, recursive =T, pattern = "light", full.names=T)
  lights_lon<-as.numeric(unlist(str_split(basename(dirname(lights_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(lights_files)),"_"))), 4)])
  lights_lat<-as.numeric(unlist(str_split(basename(dirname(lights_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(lights_files)),"_"))), 4)])
  vect<-c()
  for(i in lights_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1] # may need to do a conversion here
    vect<-c(vect, val)
  }
  lights_df<-data.frame(lon = lights_lon, lat = lights_lat, lights = vect)
  lights_df$lat_bin<-NA
  for(i in 1:length(lights_df$lat)){
    lights_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - lights_df$lat[i]) <= 0.025]
  }
  lights_df<- lights_df %>% group_by(lat_bin) %>% mutate(mean_lights = mean(lights))
  lights_df<-as.data.frame(lights_df)
  lights_df<-lights_df[,c(4,5)]
  lights_df<-distinct(lights_df)
  
  
  # get summed footprint values
  # consumption based emisssions per capita
  foot_files<-dir(dir, recursive =T, pattern = "eff_pop", full.names=T)
  
  foot_lon<-as.numeric(unlist(str_split(basename(dirname(foot_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(foot_files)),"_"))), 4)])
  foot_lat<-as.numeric(unlist(str_split(basename(dirname(foot_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(foot_files)),"_"))), 4)])
  vect<-c()

  for(i in foot_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1] # may need to do a conversion here # using only pps value
    vect<-c(vect, val)
  }
  foot_df<-data.frame(lon = foot_lon, lat = foot_lat, foot_pps = vect)
  foot_df$lat_bin<-NA
  for(i in 1:length(foot_df$lat)){
    foot_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - foot_df$lat[i]) <= 0.025]
  }
  foot_df<- foot_df %>% group_by(lat_bin) %>% mutate(mean_foot_pps = mean(foot_pps))
  foot_df<-as.data.frame(foot_df)
  foot_df<-foot_df[,c(4,5)]
  foot_df<-distinct(foot_df)
  
  # should be in umol CO2/person*s
  foot_df$mean_foot_pps<-foot_df$mean_foot_pps/1000000 # conversion for km2 to m2
  
  # convert to tCO2/person*yr
  foot_df$mean_foot_pps<-foot_df$mean_foot_pps*60*60*24*365
  foot_df$mean_foot_pps<-foot_df$mean_foot_pps*(1/1000000)*(44.01)*(1/1000000)
  
 
  # get horizontal transport error
  
  # CX currently I am storing the horr err in out_20 folder 
  err_files<-dir(out_path, recursive =T, pattern = "trans", full.names=T)
  err_lon<-as.numeric(unlist(str_split(basename(dirname(err_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(err_files)),"_"))), 4)])
  err_lat<-as.numeric(unlist(str_split(basename(dirname(err_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(err_files)),"_"))), 4)])
  
  vect<-c()
  for(i in err_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1] # may need to do a conversion here
    vect<-c(vect, val)
  }
  err_df<-data.frame(lon = err_lon, lat = err_lat, err_pps = vect)
  err_df$lat_bin<-NA
  for(i in 1:length(err_df$lat)){
    err_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - err_df$lat[i]) <= 0.025]
  }
  err_df<- err_df %>% group_by(lat_bin) %>% mutate(mean_err_pps = mean(err_pps))
  err_df<-as.data.frame(err_df)
  err_df<-err_df[,c(4,5)]
  err_df<-distinct(err_df)
 
  #
  
  # combine into a single dataset
  data<-data.frame(lat_bin = lat_df$bin)
  data<-left_join(data, obs)
  data<-left_join(data, bio_df)
  data<-left_join(data, ODIAC_city_df)
  data<-left_join(data, ODIAC_enhancement_df)
  data<-left_join(data, edgar_df)
  data<-left_join(data, pps_df)
  data<-left_join(data, gdp_df)
  data<-left_join(data, consumption_df)
  data<-left_join(data, foot_df)
  data<-left_join(data, err_df)
  data<-left_join(data, lights_df)
  
  data<-cbind(lat_df$lat_mid, data)
  colnames(data)[1]<-"lat_mid"
  data <- data %>% rowwise() %>% mutate(na_count = sum(is.na(data[data$lat_bin == lat_bin, ])))
  data<-as.data.frame(data)
  print(dim(data))
  # data <- na.omit(data)
  write.table(data, file=paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/dataframe_output/", city, "_", timestr, "_data.txt"), quote=F, row.names = F)
  
  # stop("stop here")
  #data<-data[data$na_count < 9, ] # throw out rows with all NAs
  
  # estimate fraction of enhancement from city as clustered
  data$city_frac<-data$city_ppm/data$odiac_ppm
  
  ### classify bins as background, city, nothing
  data$category<-0
  
  # identify city enhanced region
  # use quantiles instead of absolute values to let satellite data drive the show, not
  # emissions inventories
  # broader city definitions than Dien used - so cut out smallest vals
  # a lot of the city points tht don't make it will be in between the ones that do and will be filled in
  # quantiles have strange effects with limited data here
  
  # generate variations in city_ppm based on SD in latitude bins
  data_save<-data
  sect_save<-left_join(data[,colnames(data) %in% c("lat_bin", "category")], sect)
  
  enhancement_vect<-c()
  pps_vect<-c()
  gdp_vect<-c()
  consumption_vect<-c()
  lights_vect<-c()
  city_fr_vect<-c()
  city_count<-c()
  quantiles<-c()
  integrated_enh<-c()
  integrated_Epc<-c()
  background_gdp<-c()
  background_pps<-c()
  iterated_sects<-NULL
  
  plot_flag<-T
  for(j in 1:100000){
    print(paste0("iteration:",j))
  #print(j)
    data<-data_save
    sect<-sect_save
    
    quant<-runif(1,0,0.2)
    
    # draw and add a random value from the standard deviations
    # rand_int<-runif(62, -1, 1)
    rand_int<-runif(nrow(data), -1, 1)
    data$city_ppm<-data$city_ppm+(rand_int*data$city_sd)
    data$category[data$city_ppm > quantile(data$city_ppm[data$city_ppm > 0], quant, na.rm=T)]<-"city"
    
  
  
  # expand to city enhanced latitudes
    data$category[data$lat_bin > min(data$lat_bin[data$category == "city"]) &
                    data$lat_bin < max(data$lat_bin[data$category == "city"])]<-"city"
    
    # iterate until at least 5 receptors are part of the background
    iterate_val<-sum(data$recp_num, na.rm=T)*0.2
    if(iterate_val < 10){iterate_val<-10} # minimum number of background receptors
    iterate_val<-round(iterate_val, digits =0)
  
  
  # get latitude bins that are low in all categories - best chance at "clean" bg
  
  # first we add in some random variability
  # if each gets own random vector - trying to capture noise
  # doing modeled CO2 and pps together because assumed to be correlated
    data$odiac_ppm<-data$odiac_ppm+(rand_int*data$odiac_sd)
    data$odiac_total<-data$odiac_total+(rand_int*data$odiac_tot_sd)
    data$edgar_ppm<-data$edgar_ppm+(rand_int*data$sd_edgar)
    data$mean_pps<-data$mean_pps+(rand_int*data$sd_pps)
  
  # add an additional alteration for pps error from trajs
  # rand_int<-runif(62, -1, 1)
    rand_int<-runif(nrow(data), -1, 1)
    data$mean_pps<-data$mean_pps+(rand_int*data$mean_err_pps)
    
    # obs get their own vector
    # rand_int<-runif(62, -1, 1)
    rand_int<-runif(nrow(data), -1, 1)
    data$mean_XCO2<-data$mean_XCO2+(rand_int*data$mean_XCO2.uncert)
    
    # bio gets it's own vector - all the same value on assumption regional scale effects dominant
    rand_int<-runif(1, -1, 1)
    # rand_int<-rep(rand_int, 62)
    rand_int<-rep(rand_int, nrow(data))
    # rand_int<-runif(nrow(data), -1, 1)
    data$bio_uncert[is.na(data$bio_uncert)]<-0
    data$bio_ppm<-data$bio_ppm+(rand_int*data$bio_uncert)
    
    vect<-data$lat_bin[data$category != "city"]
    scores_odiac<-data$odiac_ppm[data$category != "city"]/sum(data$odiac_ppm[data$category != "city"], na.rm =T)
    scores_edgar<-data$edgar_ppm[data$category != "city"]/sum(data$edgar_ppm[data$category != "city"], na.rm =T)
    scores_odiac_tot<-data$odiac_total[data$category != "city"]/sum(data$odiac_total[data$category != "city"], na.rm =T)
    scores_pps<-data$mean_pps[data$category != "city"]/sum(data$mean_pps[data$category != "city"], na.rm =T)
    
    #XCO2,bio−adj=XCO2−ΔCO2,bio
    temp_enh<-data$mean_XCO2 - data$bio_ppm

    # get background scores
    scores_obs<-temp_enh[data$category != "city"]/sum(temp_enh[data$category != "city"], na.rm =T)
    
    # background scores
    scores<-scores_odiac+scores_edgar+scores_odiac_tot+scores_pps+scores_obs
    bg_vect<-vect[order(scores)]
    bg_recp_count<-c()

    # cumulative sum of the number of backgroundreceptors
    #STEP 3
    for(i in 1:length(bg_vect)){
      if(i == 1){
        bg_recp_count<-c(bg_recp_count, data$recp_num[data$lat_bin == bg_vect[i]])
      }
      if(i > 1){
        bg_recp_count<-c(bg_recp_count, data$recp_num[data$lat_bin == bg_vect[i]] + bg_recp_count[i-1])
      }
    }

    print(length(bg_recp_count))
    print(length(scores))

     #The indices stored in bg_ind correspond to latitude bins that collectively meet the receptor count threshold (iterate_val).
    # These bins are likely used in subsequent steps to classify background regions.
    # index of background bins
    bg_ind<-which(bg_recp_count < iterate_val)
    if(is.na(bg_recp_count[max(bg_ind)+1]) == F){bg_ind<-c(bg_ind, max(bg_ind) +1)}
    
    bg_bins<-bg_vect[bg_ind]
    data$category[data$lat_bin %in% bg_bins]<-"bg"
  
  
    #data$category[data$category == "bg" & data$odiac_total > 0.2*max(data$odiac_total, na.rm=T)]<-0
    
    
    #if(city == "LasVegas" & timestr == "2016071020"){data$bio_ppm = 0}
    # calculate enhancements with bio gradient included

 
    data$enhancement<-(data$mean_XCO2 - data$bio_ppm)
   
    # remove background enhancement values
    # STEP 5 a and b
    data$enhancement<-data$enhancement - mean(data$enhancement[data$category == "bg"], na.rm=T)

  
  
    if(sum(data$category == "bg") > 0){
      # interpolate enhancements for city n lat bins without data
      city_bins_na<-data$lat_bin[data$category == "city" & is.na(data$enhancement) == T]
      enh_df<-data[, colnames(data) %in% c("lat_bin", "enhancement")]
      enh_df<-na.omit(enh_df)
      enh_df<-interp.dataset(enh_df, enh_df$lat_bin, city_bins_na)
      enh_df<-as.data.frame(enh_df)
      data$enhancement[data$lat_bin %in% enh_df$lat_bin]<-enh_df$enhancement
      
      ### calculate overpass level enhancments and write text files
      enh<-mean(data$enhancement[data$category == "city"], na.rm=T)
    }
  
  # bin level emissions per capita
    data$Epc<-data$enhancement/data$mean_foot_pps
    data$Epc<-data$Epc/1000000 # seems we are off by a factor of 1 million
    
    # require 10 city enhanced receptors
    if(sum(data$recp_num[data$category == "city"], na.rm=T) < 10){enh<-NA}
    if(sum(data$recp_num[data$category == "bg"], na.rm=T) < 5){enh<-NA}
    
    # Does the enhanced region based on observations correspond to the model determined city enhanced region? 
    # setting to 1ppm seems to be too strict, but haveing no criteria lets a lot 
    # of weirdness through
    obs_enhanced_bins<-data$lat_bin[data$enhancement > 0.5 & is.na(data$enhancement) ==F]
    if(sum(obs_enhanced_bins %in% data$lat_bin[data$category == "city"] == T) == 0){enh<-NA}
    
    obs_enhanced_bins<-data$lat_bin[data$enhancement > quantile(data$enhancement, 0.8, na.rm=T) & is.na(data$enhancement) ==F]
    if(sum(obs_enhanced_bins %in% data$lat_bin[data$category == "city"] == T) == 0){enh<-NA}
  
  # t-test for city versus background
    test<-try(t.test(data$enhancement[data$category == "city"], data$enhancement[data$category == "bg"]), silent =T)
  
    if(any(grepl("Error", test))){enh <- NA}
    
    if(any(grepl("Error", test)) == FALSE){
      if(test$p.value > 0.1){enh <- NA}
      if(test$estimate[1] < test$estimate[2]){enh <- NA}
    }
    
  
    int_enh<-NA
    # step 7 a
    if(is.na(enh) ==F){
      int_enh<-sum(data$enhancement[data$category == "city"]*0.05, na.rm=T)
    }
    int_Epc<-NA
    if(is.na(enh) ==F){
      int_Epc<-sum(data$mean_foot_pps[data$category == "city"]*0.05, na.rm=T)
      int_Epc<-int_enh/int_Epc/1000000
    }
    
    # get more overpass aggregated values
    # gdp, consumption, pps, city_fraction
    
    gdp<-NA
    if(is.na(enh) ==F){
      gdp<-mean(data$mean_gdp[data$category == "city"], na.rm=T)
    }
    bg_gdp<-NA
    if(is.na(enh) ==F){
      bg_gdp<-mean(data$mean_gdp[data$category == "bg"], na.rm=T)
    }
    
    pps<-NA
    if(is.na(enh) ==F){
      pps<-mean(data$mean_pps[data$category == "city"], na.rm=T)
    }
    bg_pps<-NA
    if(is.na(enh) ==F){
      bg_pps<-mean(data$mean_pps[data$category == "bg"], na.rm=T)
    }
  
    consumption<-NA
    if(is.na(enh) ==F){
      consumption<-mean(data$mean_consumption[data$category == "city"], na.rm=T)
    }
    lights_val<-NA
    if(is.na(enh) ==F){
      lights_val<-mean(data$mean_lights[data$category == "city"], na.rm=T)
    }
    city_fr<-NA
    if(is.na(enh) ==F){
      city_fr<-mean(data$city_frac[data$category == "city"]*data$enhancement[data$category == "city"], na.rm=T)/enh
    }
  
  #### sectoral breakdown
    if(is.na(enh) ==F){
    sect<-cbind(sect_save[,1:2], data$enhancement, sect_save[,3:19])
    sect_city<-sect[sect$category == "city", ]
    sect_city[,4:20]<-sect_city[,4:20]*as.numeric(sect_city$enhancement) # basically weighting by enh
    sect_tots<-apply(sect[,4:20], 2, sum, na.rm=T)
    sect_tots<-round(sect_tots, digits=2)
    sect_tots<-sect_tots/sum(sect_tots)*100
    
    iterated_sects<-rbind(iterated_sects, sect_tots)
    }
  
    integrated_Epc<-c(integrated_Epc, int_Epc)
    enhancement_vect<-c(enhancement_vect, enh)
    integrated_enh<-c(integrated_enh, int_enh)
    pps_vect<-c(pps_vect, pps)
    gdp_vect<-c(gdp_vect, gdp)
    lights_vect<-c(lights_vect, lights_val)
    consumption_vect<-c(consumption_vect, consumption)
    city_fr_vect<-c(city_fr_vect, city_fr)
    city_count<-c(city_count, sum(data$category == "city"))
    quantiles<-c(quantiles, quant)
    background_gdp<-c(background_gdp, bg_gdp)
    background_pps<-c(background_pps, bg_pps)
    
  # plotting w/ plot flag - just one of them
  
    if(is.na(enh) == F & plot_flag == T){
      plot_flag<-F
      png(paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/plots/",city,"_",timestr,".png"))
      plot(data$lat_mid, data$enhancement)
      lines(data$lat_mid[data$category == "city"], rep(0, sum(data$category == "city")), col ="green")
      lines(data$lat_mid[data$category == "city"], data$enhancement[data$category == "city"], col = "purple")
      points(data$lat_mid[data$category == "city"], data$enhancement[data$category == "city"], col = "green")
      
      points(data$lat_mid[data$category == "bg"], data$enhancement[data$category == "bg"] , col ="orange")
      title(paste0(city,", ",timestr,", ",enh))
      dev.off()
    }
    # print(paste0("enhancement:",j))
  
  }
  
  enh_mean<-mean(enhancement_vect, na.rm=T)
  enh_sd<-sd(enhancement_vect, na.rm=T)
  
  int_mean<-mean(integrated_enh, na.rm=T)
  int_sd<-sd(integrated_enh, na.rm=T)
  
  int_epc_mean<-mean(integrated_Epc, na.rm=T)
  int_epc_sd<-sd(integrated_Epc, na.rm=T)
  
  pps_mean<-mean(pps_vect, na.rm=T)
  pps_sd<-sd(pps_vect, na.rm =T)
  
  gdp_mean<-mean(gdp_vect, na.rm =T)
  gdp_sd<-sd(gdp_vect, na.rm =T)
  
  consumption_mean<-mean(consumption_vect, na.rm =T)
  consumption_sd<-sd(consumption_vect, na.rm =T)
  
  lights_mean<-mean(lights_vect, na.rm =T)
  lights_sd<-sd(lights_vect, na.rm =T)
  
  city_fr_mean<-mean(city_fr_vect, na.rm=T)
  city_fr_sd<-sd(city_fr_vect, na.rm=T)
  
  bg_gdp_mean<-mean(background_gdp, na.rm=T)
  bg_gdp_sd<-sd(background_gdp, na.rm=T)
  
  bg_pps_mean<-mean(background_pps, na.rm=T)
  bg_pps_sd<-sd(background_pps, na.rm=T)
  
  na_count<-sum(is.na(enhancement_vect))
  
  # sectoral again
  if(length(iterated_sects[,1]) < 2){
    final_sects<-iterated_sects
  }
  if(length(iterated_sects[,1]) > 1){
    final_sects<-apply(iterated_sects, 2, mean)
  }
  
  enh<-c(city, timestr, enh_mean, enh_sd, int_mean, int_sd,  int_epc_mean, int_epc_sd, pps_mean, pps_sd, bg_pps_mean, bg_pps_sd, gdp_mean, gdp_sd, bg_gdp_mean, bg_gdp_sd, consumption_mean, consumption_sd, city_fr_mean, city_fr_sd, lights_mean, lights_sd, na_count)
  write.table(enh, file=paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/enhancements_lights/",city,"_",timestr,".txt"), quote=F, row.names = F)
  write.table(final_sects, file=paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/sectoral_lights/",city,"_",timestr,".txt"), quote=F, row.names = F)
  
}

# parallel configuration
# library('rslurm')
# library("raster")
# library("parallel")
# library("ncdf4")
# library("dplyr")

# timeout = 12 * 60 * 60              # time allowed before terminations in sec
# job_time = '3:00:00'               # total job time
# slurm_account = 'lin-np'
# slurm_partition = 'lin-np'

# slurm_options = list(time = '3:00:00', 
#                      account = 'owner-guest', 
#                      partition = 'notchpeak-guest',
#                      mem = '380000')

# df<-read.table("/uufs/chpc.utah.edu/common/home/lin-group14/KaiW/XSTILT_OCO/nhrs_info_plusVisual.txt", header=T, stringsAsFactors = F, skip=3)
# df<-df[,1:2]
# df$timestr<-substr(df$overpass,5,14)
# df<-df[,c(1,3)]

# df$file<-paste0(df$city,"_",df$timestr,".txt")
# done<-dir("enhancements_lights/",pattern=".txt")
# df<-df[!(df$file %in% done),]
# vect<-sample(1:length(df$file))
# df<-df[vect, ]
# df<-df[,1:2]
# dim(df)

# slurm_apply(overpass_values, df, nodes = 11, cpus_per_node = 1, slurm_options = slurm_options, jobname="overpass_aggregate_lights")

group<-"lin-group20"
version="V11r"
city="Phoenix"
  #### get file location info
dir <- paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/", city, "/", version)

# the covolutions are stored in outerr folder and the horr err is stored in outerr_folder
out.path    = list.files(dir, pattern = 'out_20', full.names = T)
outerr.path    = list.files(dir, pattern = 'outerr_20', full.names = T)
# print(out.path)
out.path <- out.path[!grepl("\\.png$", out.path)]
outerr.path <- outerr.path[!grepl("\\.png$", outerr.path)]
byid.path = file.path(outerr.path, 'by-id')


base_folder<-basename((out.path))
timestr=substr(base_folder, 5, 14)
print(timestr)

overpass_values(city, timestr, byid.path, out.path)
# serial testing


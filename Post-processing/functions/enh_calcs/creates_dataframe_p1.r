# This is the version Carlton modified based on Kai's overpass_values_func.R

###
# perhaps use standard deviations in bins to capture uncertainty for various variables
# conversions on consumption


overpass_dataframe<-function(city, timestr, dir, out_path, nhrs,urban_core_info, obs, shp_file){
  library(stringr);library(dplyr);library(rioja);library(stats)
  library(fmcmc)

  print(shp_file)
  print(timestr)
  
  year=as.integer(substr(timestr, 1, 4))

  if (shp_file){
    bio_file="^bio_nhrs_shp\\.txt$"
    city_file="^odiac_city_influence\\.txt$"
    urban_file="^odiac_urban_influence\\.txt$"
    edgar_file="edgar_shp"
  
    # gdp_file="^gdp_shp\\.txt$"
    gdp_file="^gdp_shp_v2022\\.txt$"
    consumption_file="^consumption_shp\\.txt$"
    lights_file="^lights_shp\\.txt$"
    gdp_conv_file="^GDP_conv_shp\\.txt$"
    consumption_conv_file="^consumption_conv_shp\\.txt$"
    dataframe_out_file="_dataframe_shp.txt"
    sect_edgar_out_file="_sect_edgar_shp.txt"
    # vulcan_out_file="^vulcan_conv_shp\\.txt$"
    sect_gra2pes_out_file="_sect_gra2pes_shp.txt"
    gra2pes_file="^GRA2PES_conv_shp\\.txt$"
    urban_extent <- get_urban_extent(city)
    lat1 <- urban_extent$lat[1]
    lat2 <- urban_extent$lat[2]
    lat_range <- seq(lat1 - 1.55, lat2 + 1.55, 0.05)
    # lat_range <- seq(lat1 - 3.5, lat2 + 3.5, 0.05)
    # if (year < 2021) {
    pps_file="^pps_shp\\.txt$"
    eff_pop_density_file="^eff_pop_density_shp\\.txt$"
    # } else {
    #   pps_file="^pps_shp_yearly\\.txt$"
    #   eff_pop_density_file="^eff_pop_density_shp_yearly\\.txt$"
    # }
  }  else{
    bio_file="^bio_nhrs\\.txt$"
    city_file="^city_influence\\.txt$"
    urban_file="^urban_influence\\.txt$"
    edgar_file="edgar"
   
    # pps_file="^pps\\.txt$"
    # gdp_file="^gdp\\.txt$"
    gdp_file="^gdp_v2022\\.txt$"
    consumption_file="^consumption\\.txt$"
    lights_file="^lights\\.txt$"
    gdp_conv_file="^GDP_conv\\.txt$"
    consumption_conv_file="^consumption_conv\\.txt$"
    dataframe_out_file="_dataframe.txt"
    sect_edgar_out_file="_sect_edgar.txt"
    sect_gra2pes_out_file="_sect_gra2pes.txt"
    # vulcan_out_file="^vulcan_conv\\.txt$"
    gra2pes_file="^GRA2PES_conv\\.txt$"
    lat_range<-urban_core_info
    lat_range<-lat_range$lat[lat_range$city == city]
    lat_range<-seq(lat_range - 1.55, lat_range + 1.55, 0.05)
    # lat_range<-seq(lat_range - 3.5, lat_range + 3.5, 0.05)
    # if (year < 2021) {
      pps_file="^pps\\.txt$"
      eff_pop_density_file="^eff_pop_density\\.txt$"
    # } else {
    #   pps_file="^pps_yearly\\.txt$"
    #   eff_pop_density_file="^eff_pop_density_yearly\\.txt$"
    # }
  }
  #### get latitude range of interest for the city
#   lat_range<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/post_process_outfiles/urban_core_info.txt", header=T, stringsAsFactors = F)
  lat_seq<-lat_range[1:(length(lat_range) -1)]+0.025 # midpoints of intended 0.05 degree width bins
  lat_df<-data.frame(bin = 1:length(lat_seq), lat_mid = lat_seq)


  #### get observations
#   obs_file<-paste0("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/OCO-2/overpass_obs/",city,"_", timestr,".txt")
  # obs_file=observations_file
  # obs<-read.table(obs_file, header=T, stringsAsFactors = F)
  print(paste0("length of obs lat: ", length(obs$lat)))
  # print(obs$lat)
  obs$lat_bin<-NA
  for(i in 1:length(obs$lat)){
    obs$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - obs$lat[i]) <= 0.025]
  }
  
  # }
#   for(i in seq_len(nrow(obs))) {
#   idx <- which.min(abs(lat_df$lat_mid - obs$lat[i]))
#   obs$lat_bin[i] <- lat_df$bin[idx]
# }
  
  # colnames(obs)[c(3,4,5)]<-c("XCO2","XCO2_uncert", "time_utc")
  
 
  # some overpasses have huge ranges within a given latitude bin ~10 ppm
  # deal with outliers 
  # straight up remove obs that are 3 SD above/below median for whole overpass

  # print(dim(obs))
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
  # print(dim(obs))
  
  #### get nhrs
#   nhrs<-read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/Phoenix/V11r/out_2021120320_hrrr_OCO-2/nhours_results.txt", 
#               header=TRUE, quote="")
  nhrs<-nhrs$nhrs_thresh[nhrs$city == city & nhrs$timestr == timestr]
  cat("nhrs is ", nhrs, "\n")
   if(nhrs=="-Inf") {
      nhrs=24
    } 
  
  #### get ODIAC enhancements
  
  # city for nhrs
  # ODIAC_city_files<-dir(dir,recursive =T, pattern = "city_influence.txt", full.names=T)
  ODIAC_city_files <- dir(dir, recursive = TRUE, pattern = city_file, full.names = TRUE)

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
  # ODIAC_enhancement_files<-dir(dir,recursive =T, pattern = "urban_influence.txt", full.names=T)
  ODIAC_enhancement_files<-dir(dir,recursive =T, pattern = urban_file, full.names=T)
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
  
  
  # # total for nhrs and total for 24 hours
  # # vulcan 
  # Vulcan_enhancement_files<-dir(dir,recursive =T, pattern = vulcan_out_file, full.names=T)
  # Vulcan_enhancement_lon<-as.numeric(unlist(str_split(basename(dirname(Vulcan_enhancement_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(Vulcan_enhancement_files)),"_"))), 4)])
  # Vulcan_enhancement_lat<-as.numeric(unlist(str_split(basename(dirname(Vulcan_enhancement_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(Vulcan_enhancement_files)),"_"))), 4)])
  # vect<-c()
  # vect_total<-c()
  
  # for(i in Vulcan_enhancement_files){
  #   i_data<-read.table(i, header=T, stringsAsFactors = F)

  #   val<-i_data$x[1]
  #   vect<-c(vect, val)
  # }
  # vulcan_df<-data.frame(lon = Vulcan_enhancement_lon, lat = Vulcan_enhancement_lat, vulcan_ppm = vect)
  # vulcan_df$lat_bin<-NA
  # for(i in 1:length(vulcan_df$lat)){
  #   vulcan_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - vulcan_df$lat[i]) <= 0.025]
  # }
  # vulcan_df<- vulcan_df %>% group_by(lat_bin) %>% mutate(mean_vulcan_ppm = mean(vulcan_ppm), sd_vulcan =sd(vulcan_ppm))
  # vulcan_df<-as.data.frame(vulcan_df)
  # vulcan_df<-vulcan_df[,c(4,5,6)]
  # vulcan_df$sd_vulcan[is.na(vulcan_df$sd_vulcan)]<-0
  # vulcan_df<-distinct(vulcan_df)

  #### get bio enhancements
  # bio_files<-dir(dir, recursive =T, pattern = "bio", full.names=T)
  bio_files<-dir(dir, recursive =T, pattern = bio_file, full.names=T)
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
  edgar_files<-dir(dir, recursive =T, pattern = edgar_file, full.names=T)
  # print(edgar_files)
  edgar_lon<-as.numeric(unlist(str_split(basename(dirname(edgar_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(edgar_files)),"_"))), 4)])
  edgar_lat<-as.numeric(unlist(str_split(basename(dirname(edgar_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(edgar_files)),"_"))), 4)])
  
  # print(paste0("length of edgar_lon ", length(edgar_lon)))
  # print(paste0("length of edgar_files ", length(edgar_files)))

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
  sect_edgar<-NULL
  for(i in edgar_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    i_data<- i_data %>% group_by(sect) %>% mutate(ppm = mean(ppm))
    i_data<-distinct(as.data.frame(i_data))
    
    sect_edgar<-rbind(sect_edgar, i_data$ppm)
  }
  sect_edgar<-as.data.frame(sect_edgar)
  colnames(sect_edgar)<-i_data$sect_edgar
  sect_edgar<-cbind(edgar_lon, edgar_lat, sect_edgar)
  print(dim(sect_edgar))
  colnames(sect_edgar)[1:2]<-c("lon","lat")
  colnames(sect_edgar)[3:19]<-c(
  "CHE","ENE","IND","IRO","NEU","NFE","NMM","PRO","PRU_SOL","RCO",
  "REF_TRF","TNR_Aviation_CDS","TNR_Aviation_CRS","TNR_Aviation_LTO",
  "TNR_Other","TNR_Ship","TRO_noRES"
)
  sect_edgar$lat_bin<-NA
  for(i in 1:length(sect_edgar$lat)){
    sect_edgar$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - sect_edgar$lat[i]) <= 0.025]
  }
  
  # print(sect_edgar)
  sect_edgar<- sect_edgar %>% group_by(lat_bin) %>% mutate(CHE = mean(CHE), ENE = mean(ENE), 
                                               IND = mean(IND), IRO = mean(IRO),
                                               NEU = mean(NEU), NFE = mean(NFE), 
                                               NMM = mean(NMM), PRO = mean(PRO), 
                                               PRU_SOL = mean(PRU_SOL), RCO = mean(RCO),
                                               REF_TRF = mean(REF_TRF), TNR_Aviation_CDS = mean(TNR_Aviation_CDS),
                                               TNR_Aviation_CRS = mean(TNR_Aviation_CRS), TNR_Aviation_LTO = mean(TNR_Aviation_LTO),
                                               TNR_Other = mean(TNR_Other), TNR_Ship = mean(TNR_Ship), 
                                               TRO_noRES = mean(TRO_noRES))
  
  print(paste0("dim sect_edgar before: ", dim(sect_edgar)))
   print(paste0("dim egdar_df : ", dim(edgar_df)))
  sect_edgar<-as.data.frame(sect_edgar)
  sect_edgar<-sect_edgar[,3:20]
  sect_edgar<-distinct(sect_edgar)
  print(paste0("dim sect_edgar after: ", dim(sect_edgar)))
  
  #### get EDGAR sectoral breakdown of enhancements
  gra2pes_files<-dir(dir, recursive =T, pattern = gra2pes_file, full.names=T)
  # print(gra2pes_files)
  gra2pes_lon<-as.numeric(unlist(str_split(basename(dirname(gra2pes_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(gra2pes_files)),"_"))), 4)])
  gra2pes_lat<-as.numeric(unlist(str_split(basename(dirname(gra2pes_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(gra2pes_files)),"_"))), 4)])
  # print(paste0("length of gra2pes_lon ", length(gra2pes_lon)))
  #  print(paste0("length of gra2pes_files ", length(gra2pes_files)))
  # get EDGAR total enhancements
  vect<-c()
  for(i in gra2pes_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-sum(i_data$ppm)/(dim(i_data)[1]/19)
    vect<-c(vect, val)
  }
  gra2pes_df<-data.frame(lon = gra2pes_lon, lat = gra2pes_lat, ppm = vect)
  gra2pes_df$lat_bin<-NA
  for(i in 1:length(gra2pes_df$lat)){
    gra2pes_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - gra2pes_df$lat[i]) <= 0.025]
  }
  print(paste0("dim gra2pes_df here: ", dim(gra2pes_df)))
  gra2pes_df_intact=gra2pes_df
  gra2pes_df<- gra2pes_df %>% group_by(lat_bin) %>% mutate(gra2pes_ppm = mean(ppm), sd_gra2pes = sd(ppm))
  gra2pes_df<-as.data.frame(gra2pes_df)
  gra2pes_df<-gra2pes_df[,c(4,5,6)]
  gra2pes_df$sd_gra2pes[is.na(gra2pes_df$sd_gra2pes)]<-0
  gra2pes_df<-distinct(gra2pes_df)
  
  # get dataframe of average sectoral breakdown for latitude bins
  sect_gra2pes<-NULL
  for(i in gra2pes_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    i_data<- i_data %>% group_by(sect) %>% mutate(ppm = mean(ppm))
    i_data<-distinct(as.data.frame(i_data))
    
    sect_gra2pes<-rbind(sect_gra2pes, i_data$ppm)
  }
  print(paste0("dim sect_gra2pes here: ", dim(sect_gra2pes)))
    # print(i_data$sect)
  sect_gra2pes_colnames=i_data$sect
  # print(sect_gra2pes_colnames)
  sect_gra2pes<-as.data.frame(sect_gra2pes)
  colnames(sect_gra2pes)<-i_data$sect_gra2pes
  sect_gra2pes<-cbind(gra2pes_lon, gra2pes_lat, sect_gra2pes)
  colnames(sect_gra2pes)[1:2]<-c("lon","lat")
  colnames(sect_gra2pes)[3:21]<-sect_gra2pes_colnames

  # sect_gra2pes$lat_bin<-NA
  # for(i in 1:length(sect_gra2pes$lat)){
  #   sect_gra2pes$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - sect_gra2pes$lat[i]) <= 0.025]
  # }
  sect_gra2pes$lat_bin=gra2pes_df_intact$lat_bin
  print(paste0("dim sect_gra2pes here2: ", dim(sect_gra2pes)))
  # print(sect_gra2pes$lat_bin)
  # sect_gra2pes<- sect_gra2pes %>% group_by(gra2pes_df_intact$lat_bin) %>% mutate(AG = mean(AG), AVIATION = mean(AVIATION), 
  #                                              COMM = mean(COMM), COOKING = mean(COOKING),
  #                                              EGU = mean(EGU), FUG = mean(FUG), 
  #                                              INDF = mean(INDF), INDP = mean(INDP), 
  #                                              INTERNATIONAL = mean(INTERNATIONAL), OFFROAD = mean(OFFROAD),
  #                                              OG = mean(OG), ONROAD_DSL = mean(ONROAD_DSL),
  #                                              ONROAD_GAS = mean(ONROAD_GAS), RAIL = mean(RAIL),
  #                                              RES = mean(RES), SHIPPING = mean(SHIPPING), 
  #                                              VCP = mean(VCP),WASTE = mean(WASTE),TOTAL = mean(total))


# Attach the lat_bin vector as a column first
# Attach the lat_bin vector as a column first
sect_gra2pes <- sect_gra2pes %>%
  mutate(lat_bin = lat_bin) %>%
  group_by(lat_bin) %>%
  summarise(
    across(
      c("AG", "AVIATION", "COMM", "COOKING", "EGU", "FUG", "INDF", "INDP", "INTERNATIONAL",
        "OFFROAD", "OG", "ONROAD_DSL", "ONROAD_GAS", "RAIL", "RES", "SHIPPING", "VCP", "WASTE", "total"),
      ~ mean(.x, na.rm = TRUE),
      .names = "{.col}"
    )
  ) %>%
  ungroup()

  print(paste0("dim sect_gra2pes before: ", dim(sect_gra2pes)))
  print(paste0("dim gra2pes_df : ", dim(gra2pes_df)))
  print(colnames(sect_gra2pes))
  sect_gra2pes<-as.data.frame(sect_gra2pes)
  sect_gra2pes<-sect_gra2pes[,2:20]
  # sect_gra2pes<-distinct(sect_gra2pes)
  print(paste0("dim sect_gra2pes after: ", dim(sect_gra2pes)))
  
  #### get effective population densities
  pps_files<-dir(dir, recursive =T, pattern = pps_file, full.names=T)
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
  gdp_files<-dir(dir, recursive =T, pattern = gdp_file, full.names=T)
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
  
  #### get gdp convolution with footprint for E_GDP
  gdp_conv_files<-dir(dir, recursive =T, pattern = gdp_conv_file, full.names=T)
  gdp_lon<-as.numeric(unlist(str_split(basename(dirname(gdp_conv_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(gdp_conv_files)),"_"))), 4)])
  gdp_lat<-as.numeric(unlist(str_split(basename(dirname(gdp_conv_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(gdp_conv_files)),"_"))), 4)])
  vect<-c()
  for(i in gdp_conv_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1]
    vect<-c(vect, val)
  }
  gdp_conv_df<-data.frame(lon = gdp_lon, lat = gdp_lat, gdp_conv = vect)
  gdp_conv_df$lat_bin<-NA
  for(i in 1:length(gdp_conv_df$lat)){
    gdp_conv_df$lat_bin[i]<-lat_df$bin[abs(lat_df$lat_mid - gdp_conv_df$lat[i]) <= 0.025]
  }
  gdp_conv_df<- gdp_conv_df %>% group_by(lat_bin) %>% mutate(mean_gdp_footprint = mean(gdp_conv))
  gdp_conv_df<-as.data.frame(gdp_conv_df)
  gdp_conv_df<-gdp_conv_df[,c(4,5)]
  gdp_conv_df<-distinct(gdp_conv_df)

  #### get consumption level foots
  consumption_files<-dir(dir, recursive =T, pattern = consumption_file, full.names=T)
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
  lights_files<-dir(dir, recursive =T, pattern = lights_file, full.names=T)
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
  foot_files<-dir(dir, recursive =T, pattern = eff_pop_density_file, full.names=T)
  
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
  err_files<-dir(out_path, recursive =T, pattern = "horr_trans", full.names=T)
  # err_files<-dir(dir, recursive =T, pattern = "^horr_trans_err\\.txt$", full.names=T)
  print(timestr)
  err_lon<-as.numeric(unlist(str_split(basename(dirname(err_files)),"_"))[seq(2,length(unlist(str_split(basename(dirname(err_files)),"_"))), 4)])
  err_lat<-as.numeric(unlist(str_split(basename(dirname(err_files)),"_"))[seq(3,length(unlist(str_split(basename(dirname(err_files)),"_"))), 4)])
  
  vect<-c()
  for(i in err_files){
    i_data<-read.table(i, header=T, stringsAsFactors = F)
    val<-i_data$x[1] # may need to do a conversion here
    vect<-c(vect, val)
  }
  print(length(err_lon))
  print(length(err_lat))
  print(length(vect))
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
  data<-left_join(data, gra2pes_df)
  data<-left_join(data, pps_df)
  data<-left_join(data, gdp_df)
  data<-left_join(data, gdp_conv_df)
  # data<-left_join(data, vulcan_df)
  data<-left_join(data, foot_df)
  data<-left_join(data, err_df)
  data<-left_join(data, lights_df)
  
  data<-cbind(lat_df$lat_mid, data)
  colnames(data)[1]<-"lat_mid"
  data <- data %>% rowwise() %>% mutate(na_count = sum(is.na(data[data$lat_bin == lat_bin, ])))
  data<-as.data.frame(data)
  print(dim(data))
  print(length(lat_df$bin))
  # data <- na.omit(data)
  
  # Check if the folder exists, if not, create a folder for each CONUS city 
  # Need to source Login_init.sh to get paths 
  out.path=Sys.getenv("OUT_DF_DIR")
  output_folder <- paste0(out.path,"/dataframe_output_gdp2022/", city)
  if (!dir.exists(output_folder)) {
    dir.create(output_folder, recursive = TRUE)
  }
  write.table(data, file=paste0(output_folder,"/", city, "_", timestr, dataframe_out_file), quote=F, row.names = F) #dataframe
  write.table(sect_edgar, file=paste0(output_folder,"/", city, "_", timestr, sect_edgar_out_file), quote=F, row.names = F) #sectoral breakdown
  write.table(sect_gra2pes, file=paste0(output_folder,"/", city, "_", timestr, sect_gra2pes_out_file), quote=F, row.names = F) #sectoral breakdown
} # end function  
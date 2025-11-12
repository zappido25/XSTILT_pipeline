
# This is the original code for the Monte Carlo brute force method developed by Kai W

MC_brute_force = function(data, edgar_sect,gra2pes_sect,city, timestr,shp_file, vulcan_epc){
    library(stringr);library(dplyr);library(rioja);library(stats)
    
    if (shp_file){
         suffix="_MC_brute_UScensus.txt"   
    }else{
         suffix="_MC_brute_CC.txt"
    }
    data$city_frac<-data$city_ppm/data$odiac_ppm

    ### classify bins as background, city, nothing
    data$category<-0

    cities_without_smurf=c("Atlanta","Portland","Baltimore", "Austin","Orlando","Indianapolis","Columbus","Cleveland","Minneapolis")
    
    # identify city enhanced region
    # use quantiles instead of absolute values to let satellite data drive the show, not
    # emissions inventories
    # broader city definitions than Dien used - so cut out smallest vals
    # a lot of the city points tht don't make it will be in between the ones that do and will be filled in
    # quantiles have strange effects with limited data here

    # generate variations in city_ppm based on SD in latitude bins
    data_save<-data
    # print(dim(edgar_sect))
    # print(dim(gra2pes_sect))
    gra2pes_sect$lat_bin=edgar_sect$lat_bin
#     cat("data columns:\n"); print(colnames(data))
# cat("gra2pes_sect columns:\n"); print(colnames(gra2pes_sect))
# cat("edgar_sect columns:\n"); print(colnames(edgar_sect))
    edgar_sect_save<-left_join(data[,colnames(data) %in% c("lat_bin", "category")], edgar_sect)
    gra2pes_sect_save<-left_join(data[,colnames(data) %in% c("lat_bin", "category")], gra2pes_sect)

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
    integrated_Epc_gdp<-c()
    integrated_gdp_Epc<-c()
    background_gdp<-c()
    background_pps<-c()
    iterated_sects_edgar<-NULL
    iterated_sects_city_edgar<-NULL
    iterated_sects_ppm_edgar<-NULL
    iterated_sects_gra2pes<-NULL
    iterated_sects_city_gra2pes<-NULL
    iterated_sects_ppm_gra2pes<-NULL

    plot_flag<-T

    # monte carlo brute force iterations starts here
    # add a random value to city_ppm, odiac_ppm, edgar_ppm, vulcan_ppm,bio_ppm, mean_ppc, odiac_total for each iteration value jj
    for(jj in 1:100000){
        print(paste0("iteration:",jj))
       
        data<-data_save
        sect_edgar<-edgar_sect_save
        sect_gra2pes<-gra2pes_sect_save

        quant<-runif(1,0,0.2)

        # draw and add a random value from the standard deviations
        # rand_int<-runif(62, -1, 1)
        rand_int<-runif(nrow(data), -1, 1)
        data$city_ppm<-data$city_ppm+(rand_int*data$city_sd)
        data$category[data$city_ppm > quantile(data$city_ppm[data$city_ppm > 0], quant, na.rm=T)]<-"city"



        # expand to city enhanced latitudes. all lats between bins labeled "city" are now categorized as "city"
        data$category[data$lat_bin > min(data$lat_bin[data$category == "city"]) &
                        data$lat_bin < max(data$lat_bin[data$category == "city"])]<-"city"

        # cat("nrows in data which are labelled as city: ", sum(data$category == "city", na.rm = TRUE), "\n")
        # cat("nrows in data which are labelled as not city: ", sum(data$category != "city", na.rm = TRUE), "\n")
        # iterate until at least 5 receptors are part of the background
        iterate_val<-sum(data$recp_num, na.rm=T)*0.2
        # if(iterate_val < 10){iterate_val<-10} # minimum number of background receptors
        if(iterate_val < 5){iterate_val<-5} # minimum number of background receptors
        iterate_val<-round(iterate_val, digits =0)


        # get latitude bins that are low in all categories - best chance at "clean" bg

        # first we add in some random variability
        # if each gets own random vector - trying to capture noise
        # doing modeled CO2 and pps together because assumed to be correlated
        data$odiac_ppm<-data$odiac_ppm+(rand_int*data$odiac_sd) # city 
        data$odiac_total<-data$odiac_total+(rand_int*data$odiac_tot_sd) # urban
        data$mean_pps<-data$mean_pps+(rand_int*data$sd_pps)
        if (vulcan_epc){
            print("using vulcan scores instead of edgar")
            data$mean_vulcan_ppm<-data$mean_vulcan_ppm+(rand_int*data$sd_vulcan)
        } else {
        #  print("use edgar ppm")
            data$edgar_ppm<-data$edgar_ppm+(rand_int*data$sd_edgar)
        }
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
        # point 3a for bins which are not city
        scores_odiac<-data$odiac_ppm[data$category != "city"]/sum(data$odiac_ppm[data$category != "city"], na.rm =T)
        scores_odiac_tot<-data$odiac_total[data$category != "city"]/sum(data$odiac_total[data$category != "city"], na.rm =T)
        scores_pps<-data$mean_pps[data$category != "city"]/sum(data$mean_pps[data$category != "city"], na.rm =T)

        # use vulcan inventory    
        if (vulcan_epc) {
            print("using vulcan inventory")
            scores_inventory<-data$mean_vulcan_ppm[data$category != "city"]/sum(data$mean_vulcan_ppm[data$category != "city"], na.rm =T)
        }
        else {
            # print("using edgar inventory")
            scores_inventory<-data$edgar_ppm[data$category != "city"]/sum(data$edgar_ppm[data$category != "city"], na.rm =T)
        }
        # for testing
        if (city %in% cities_without_smurf ) {
            # print("here")
            data$bio_ppm = 0.1+ rand_int*0.01
        }
        #XCO2,bio−adj=XCO2−ΔCO2,bio , bio adjusted
        temp_enh<-data$mean_XCO2 - data$bio_ppm
      
        # get background scores
        scores_obs<-temp_enh[data$category != "city"]/sum(temp_enh[data$category != "city"], na.rm =T)

        # print(data$bio_ppm)
        # background scores old, original method 
        # scores_old<-scores_odiac+scores_inventory+scores_odiac_tot+scores_pps+scores_obs
        scores<-scores_odiac+scores_inventory+scores_odiac_tot+scores_pps+scores_obs
       
# background score new
        # scores <- rowSums(
        # cbind(scores_odiac, scores_inventory, scores_odiac_tot, scores_pps, scores_obs),
        # na.rm = TRUE
        # )
        # scores[scores == 0] <- NA
       
        
    #    print(scores_odiac)
    #    print(scores_inventory)
    #    print(scores_odiac_tot)
    #    print(scores_pps)
    #    print(scores)
    #    cat(sprintf(
    #         "ODIAC=%.4f, EDGAR=%.4f, ODIAC_TOTAL=%.4f, PPS=%.4f, OBS=%.4f --> TOTAL=%.4f\n",
    #         sum(scores_odiac,na.rm=TRUE),
    #         sum(scores_inventory,na.rm=TRUE),
    #         sum(scores_odiac_tot,na.rm=TRUE),
    #         sum(scores_pps,na.rm=TRUE),
    #         sum(scores_obs,na.rm=TRUE),
    #         sum(scores,na.rm=TRUE)
    #         ))
        
        bg_vect<-vect[order(scores)]
        bg_recp_count<-c()
        
      
        # cumulative sum of the number of backgroundreceptors
        #STEP 3 b
        for(i in 1:length(bg_vect)){
            if(i == 1){
                bg_recp_count<-c(bg_recp_count, data$recp_num[data$lat_bin == bg_vect[i]])
            }
            if(i > 1){
                bg_recp_count<-c(bg_recp_count, data$recp_num[data$lat_bin == bg_vect[i]] + bg_recp_count[i-1])
            }
        }
      
       
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

        ## XCO2,bio-adj=XCO2-deltaCO2,bio    , step 1 and 5a
        data$enhancement<-(data$mean_XCO2 - data$bio_ppm)

        # STEP 5b to calculate CO2 enhancements i.e  ∆CO2,OCO−2 <- XCO2,bio−adj− XCO2,BG
        # remove background enhancement values
        # first remove biogenic ppm and then remove the background values to get enhancements
        data$enhancement<-data$enhancement - mean(data$enhancement[data$category == "bg"], na.rm=T)
       
        
        # cat("number of bg bins ", sum(data$category == "bg"))
        if(sum(data$category == "bg") > 0){
            # interpolate enhancements for city n lat bins without data
            city_bins_na<-data$lat_bin[data$category == "city" & is.na(data$enhancement) == T]
            
            enh_df<-data[, colnames(data) %in% c("lat_bin", "enhancement")]
            enh_df<-na.omit(enh_df)
            # print(enh_df)
            if (nrow(enh_df) > 0){ # happens when all enh values are na
                enh_df<-interp.dataset(enh_df, enh_df$lat_bin, city_bins_na)
                enh_df<-as.data.frame(enh_df)
                data$enhancement[data$lat_bin %in% enh_df$lat_bin]<-enh_df$enhancement
            }
            ### calculate overpass level enhancments and write text files
            enh<-mean(data$enhancement[data$category == "city"], na.rm=T)
        }

        # Step 7a: bin level emissions per capita 
        data$Epc<-data$enhancement/data$mean_foot_pps
        data$Epc<-data$Epc/1000000 # seems we are off by a factor of 1 million
        
        # # bin level emissions per gdp 
        # data$Egdp<-data$enhancement/data$mean_gdp
        # data$Egdp<-data$Egdp/1000000 # seems we are off by a factor of 1 million

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

        # t-test for city versus background.. curently a 2 sides test, and the original method used in Kai W
        test<-try(t.test(data$enhancement[data$category == "city"], data$enhancement[data$category == "bg"]), silent =T)

        if(any(grepl("Error", test))){enh <- NA}

        if(any(grepl("Error", test)) == FALSE){
            if(test$p.value > 0.1){enh <- NA}
            if(test$estimate[1] < test$estimate[2]){enh <- NA}
        }

        # mann-whitney test if t-test fails
        # test <- try(wilcox.test(data$enhancement[data$category == "city"], data$enhancement[data$category == "bg"], alternative = "greater"), silent = TRUE)
        # if(any(grepl("Error", test))){enh <- NA}

        # if(any(grepl("Error", test)) == FALSE){
        #     if(test$p.value > 0.1){enh <- NA}
        #     # if(test$estimate[1] < test$estimate[2]){enh <- NA}
        # }
        
        # integrated enhancements within the city for each overpass
        # bin width accountability by multiplying with 0.05
        int_enh<-NA
     
        if(is.na(enh) ==F){
            int_enh<-sum(data$enhancement[data$category == "city"]*0.05, na.rm=T)
        }
        int_Epc<-NA
        # CO2 intensity i.e CO2 emissions per USD
        int_CI<-NA # carbon intensity
        int_PC<-NA # PRODUCTIVITY CAPACITY i.e how MUCH usd is generated per tco2 emitted
        
        if(is.na(enh) ==F){
            int_Epc<-sum(data$mean_foot_pps[data$category == "city"]*0.05, na.rm=T)
            int_Epc<-int_enh/int_Epc/1000000
            
            # mean effective gdp per capita of the city enhanced region
            # normalized using population weighting to get a more intutive measure?
            # eff_gdp_cap<-mean(data$mean_gdp[data$category == "city"], na.rm=T)
           
            # gdp_city=data$mean_gdp[data$category == "city"]
            # pps_city=data$mean_pps[data$category == "city"]
            # city_data <- data[data$category == "city", ]
            # eff_gdp_cap<-with(city_data, sum(mean_gdp * pps_city, na.rm = TRUE) / sum(pps_city, na.rm = TRUE))
            eff_gdp_cap<-sum(data$mean_gdp[data$category == "city"], na.rm=T)

            int_CI<-int_Epc/eff_gdp_cap #tco2 per usd
            
            int_PC<-eff_gdp_cap/int_Epc #usd/tco2
        
        }

        # get more overpass aggregated values
        # gdp, consumption, pps, city_fraction for city enhanced region

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
            sect_edgar<-cbind(edgar_sect_save[,1:2], data$enhancement, edgar_sect_save[,3:19])
            sect_city_edgar<-sect_edgar[sect_edgar$category == "city", ]
            sect_city_edgar[,4:20]<-sect_city_edgar[,4:20]*as.numeric(sect_city_edgar$enhancement) # basically weighting by enh
            sect_city_tots_edgar<-apply(sect_city_edgar[,4:20], 2, sum, na.rm=T) # total enhacement weighted ppm
            sect_tots_edgar<-apply(sect_edgar[,4:20], 2, sum, na.rm=T) # total sector ppm in the overpass
            sect_tots_edgar<-round(sect_tots_edgar, digits=2)
            sect_tots_ppm_edgar=sect_tots_edgar
            sect_tots_edgar<-sect_tots_edgar/sum(sect_tots_edgar)*100

            iterated_sects_edgar<-rbind(iterated_sects_edgar, sect_tots_edgar) ## is the percentage contribution of each sector to the total enhancement
            iterated_sects_ppm_edgar<-rbind(iterated_sects_ppm_edgar, sect_tots_ppm_edgar) ## is the ppm contribution of each sector to the total enhancement
            iterated_sects_city_edgar<-rbind(iterated_sects_city_edgar, sect_city_tots_edgar) ## absolute enhancement contribution from each sector in the city region
            
            # print(paste0("grapes_dim: ",dim(gra2pes_sect_save)))
            sect_gra2pes<-cbind(gra2pes_sect_save[,1:2], data$enhancement, gra2pes_sect_save[,3:21])
            sect_city_gra2pes<-sect_gra2pes[sect_gra2pes$category == "city", ]
            sect_city_gra2pes[,4:22]<-sect_city_gra2pes[,4:22]*as.numeric(sect_city_gra2pes$enhancement) # basically weighting by enh
            sect_city_tots_gra2pes<-apply(sect_city_gra2pes[,4:22], 2, sum, na.rm=T) # total enhacement weighted ppm
            sect_tots_gra2pes<-apply(sect_gra2pes[,4:22], 2, sum, na.rm=T) # total sector ppm in the overpass
            sect_tots_gra2pes<-round(sect_tots_gra2pes, digits=2)
            sect_tots_ppm_gra2pes=sect_tots_gra2pes
            sect_tots_gra2pes<-sect_tots_gra2pes/sum(sect_tots_gra2pes)*100

            iterated_sects_gra2pes<-rbind(iterated_sects_gra2pes, sect_tots_gra2pes) ## is the percentage contribution of each sector to the total enhancement
            iterated_sects_ppm_gra2pes<-rbind(iterated_sects_ppm_gra2pes, sect_tots_ppm_gra2pes) ## is the ppm contribution of each sector to the total enhancement
            iterated_sects_city_gra2pes<-rbind(iterated_sects_city_gra2pes, sect_city_tots_gra2pes) ## absolute enhancement contribution from each sector in the city region
        }

        # if (is.null(iterated_sects)== T) {
        #     iterated_sects=NA
        # }

        integrated_Epc<-c(integrated_Epc, int_Epc)
        integrated_Epc_gdp<-c(integrated_Epc_gdp, int_CI)
        integrated_gdp_Epc<-c(integrated_gdp_Epc, int_PC)
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

        #svae score for sanity check
        # scores_vect <- c(as.numeric(scores_odiac),
        #      as.numeric(scores_inventory),
        #      as.numeric(scores_odiac_tot),
        #      as.numeric(scores_pps),
        #      as.numeric(scores_obs),
        #     as.numeric(scores)
        #     )    
        # plotting w/ plot flag - just one of them

        if(is.na(enh) == F & plot_flag == T){
            plot_flag<-F

            out.path=Sys.getenv("OUT_DF_DIR")
            png_folder <- paste0(out.path,"/Plots/", city)
            if (!dir.exists(png_folder)) {
                dir.create(png_folder, recursive = TRUE)
            }
           
            png_filename=paste0(city, "_", timestr, ".png")
            png(paste0(png_folder, "/", png_filename), width=800, height=600)
            
            
            plot(data$lat_mid, data$enhancement)
            lines(data$lat_mid[data$category == "city"], rep(0, sum(data$category == "city")), col ="green")
            lines(data$lat_mid[data$category == "city"], data$enhancement[data$category == "city"], col = "purple")
            points(data$lat_mid[data$category == "city"], data$enhancement[data$category == "city"], col = "green")
            
            points(data$lat_mid[data$category == "bg"], data$enhancement[data$category == "bg"] , col ="orange")
            title(paste0(city,", ",timestr,", ",enh))
            dev.off()
        }
    # print(paste0("enhancement:",j))

    } # end of monte carlo iterations

    enh_mean<-mean(enhancement_vect, na.rm=T)
    enh_sd<-sd(enhancement_vect, na.rm=T)

    int_mean<-mean(integrated_enh, na.rm=T)
    int_sd<-sd(integrated_enh, na.rm=T)

    int_epc_mean<-mean(integrated_Epc, na.rm=T)
    int_epc_sd<-sd(integrated_Epc, na.rm=T)
    
    int_epc_gdp_mean<-mean(integrated_Epc_gdp, na.rm=T)
    int_epc_gdp_sd<-sd(integrated_Epc_gdp, na.rm=T)
    
    int_gdp_epc_mean<-mean(integrated_gdp_Epc, na.rm=T)
    int_gdp_epc_sd<-sd(integrated_gdp_Epc, na.rm=T)

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
    if(length(iterated_sects_edgar[,1]) < 2){
        final_sects_edgar<-iterated_sects_edgar
        final_sects_city_edgar<-iterated_sects_city_edgar
        final_sects_ppm_edgar<-iterated_sects_ppm_edgar
    }
    if(length(iterated_sects_edgar[,1]) > 1){
        final_sects_edgar<-apply(iterated_sects_edgar, 2, mean)
        final_sects_city_edgar<-apply(iterated_sects_city_edgar, 2, mean)
        final_sects_ppm_edgar<-apply(iterated_sects_ppm_edgar, 2, mean)
    }
    if(length(iterated_sects_gra2pes[,1]) < 2){
        final_sects_gra2pes<-iterated_sects_gra2pes
        final_sects_city_gra2pes<-iterated_sects_city_gra2pes
        final_sects_ppm_gra2pes<-iterated_sects_ppm_gra2pes
    }
    if(length(iterated_sects_gra2pes[,1]) > 1){
        final_sects_gra2pes<-apply(iterated_sects_gra2pes, 2, mean)
        final_sects_city_gra2pes<-apply(iterated_sects_city_gra2pes, 2, mean)
        final_sects_ppm_gra2pes<-apply(iterated_sects_ppm_gra2pes, 2, mean)
    }

    enh<-c(city, timestr, enh_mean, enh_sd, int_mean, int_sd,  int_epc_mean, int_epc_sd, int_epc_gdp_mean, int_epc_gdp_sd,
        int_gdp_epc_mean, int_gdp_epc_sd,
     pps_mean, pps_sd, bg_pps_mean, bg_pps_sd, gdp_mean, gdp_sd, 
     bg_gdp_mean, bg_gdp_sd, consumption_mean, 
     consumption_sd, city_fr_mean, city_fr_sd, lights_mean, lights_sd, na_count)

    # save to txt files
    out.path=Sys.getenv("OUT_DF_DIR")
    # enh.out.folder <- paste0(out.path,"/ENHANCEMENTS_pop/", city)
    # sect.out.folder <- paste0(out.path,"/SECTORAL_pop/", city)
    # data.out.folder <- paste0(out.path,"/DATAFRAME_pop/", city)
    # if (vulcan_epc){
    #     enh.out.folder <- paste0(out.path,"/ENHANCEMENTS_vulcan/", city)
    #     data.out.folder <- paste0(out.path,"/DATAFRAME_vulcan/", city)
    #     scores.out.folder <- paste0(out.path,"/SCORES_vulcan/", city)
    #     sect.out.folder <- paste0(out.path,"/SECTORAL_vulcan/", city)
    # }else {
        enh.out.folder <- paste0(out.path,"/ENHANCEMENTS/", city)
        data.out.folder <- paste0(out.path,"/DATAFRAME/", city)
        # scores.out.folder <- paste0(out.path,"/SCORES/", city)
        edgar_sect.out.folder <- paste0(out.path,"/SECTORAL_edgar/", city)
        gra2pes_sect.out.folder <- paste0(out.path,"/SECTORAL_gra2pes/", city)
    # }

    if (!dir.exists(enh.out.folder)) {
        dir.create(enh.out.folder, recursive = TRUE)
    }

    if (!dir.exists(edgar_sect.out.folder)) {
        dir.create(edgar_sect.out.folder, recursive = TRUE)
    }
    if (!dir.exists(gra2pes_sect.out.folder)) {
        dir.create(gra2pes_sect.out.folder, recursive = TRUE)
    }

    if (!dir.exists(data.out.folder)) {
        dir.create(data.out.folder, recursive = TRUE)
    }

    # if (!dir.exists(scores.out.folder)) {
    #     dir.create(scores.out.folder, recursive = TRUE)
    # }
    write.table(enh, file=paste0(enh.out.folder,"/",city,"_",timestr,suffix), quote=F, row.names = F)
    
    write.table(final_sects_edgar, file=paste0(edgar_sect.out.folder,"/",city,"_",timestr,suffix), quote=F, row.names = F)
    write.table(final_sects_city_edgar, file=paste0(edgar_sect.out.folder,"/",city,"_",timestr,"_city",suffix), quote=F, row.names = F)
    write.table(final_sects_ppm_edgar, file=paste0(edgar_sect.out.folder,"/",city,"_",timestr,"_ppm",suffix), quote=F, row.names = F)
    
    write.table(final_sects_gra2pes, file=paste0(gra2pes_sect.out.folder,"/",city,"_",timestr,suffix), quote=F, row.names = F)
    write.table(final_sects_city_gra2pes, file=paste0(gra2pes_sect.out.folder,"/",city,"_",timestr,"_city",suffix), quote=F, row.names = F)
    write.table(final_sects_ppm_gra2pes, file=paste0(gra2pes_sect.out.folder,"/",city,"_",timestr,"_ppm",suffix), quote=F, row.names = F)
    
    write.table(data, file=paste0(data.out.folder,"/",city,"_",timestr,suffix), quote=F, row.names = F)
    # write.table(scores, file=paste0(scores.out.folder,"/",city,"_",timestr,suffix), quote=F, row.names = F)
    # write.table(scores, file=paste0(scores.out.folder,"/",city,"_",timestr,suffix), quote=F, row.names = F)
    # print(paste0(data.out.folder,"/",city,"_",timestr,suffix))

}


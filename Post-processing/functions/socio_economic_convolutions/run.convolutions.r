
##comment out functions that are not needed


run.convolutions=function(foot, foot_byid, nhrs, city, shp_file) {
  # Load necessary libraries
    r_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/functions/socio_economic_convolutions/', pattern = '\\.r$', full.names = TRUE)
    # print(r_files)
    invisible(lapply(r_files, source))

    # print("vulcan conv")
    # calc.vulcan.convolution(foot, foot_byid, nhrs, city, shp_file) 
    print("GRAPES conv")
    calc.grapes.convolution(foot, foot_byid, nhrs, city, shp_file)

        
    ### effective population density from the script calc_pps.r
    print("Calculating effective population density...")
    eff_PPS_func_nhrs(foot, foot_byid, nhrs, city, shp_file) 
    # eff_PPS_func_nhrs_yearly(foot, foot_byid, nhrs, city, shp_file) 
    
    print("Calculating effective GDP USING 2022 data...")
    calc_eff_gdp_2022_nhrs(foot, foot_byid, nhrs, city, shp_file)
    
    # ### effective gdp convolution from the script calc_gdp.r. footprint is not normalized
    print("Calculating effective GDP...")
    calc_eff_gdp_nhrs(foot, foot_byid, nhrs, city, shp_file)
    
    # ### gdp convolution of footprint*gdp
    print("Calculating GDP convolution...")
    calc_gdp_conv_nhrs(foot, foot_byid, nhrs, city, shp_file)
  
    # ### consumption convolution from the script calc_consumption.r
    print("Calculating effective consumption...")
    eff_consump_func_nhrs(foot, foot_byid, nhrs, city, shp_file)

    # ### bio convolution from the script calc_bio.r 
    print("Calculating bio convolution...")
    bio_func_nhrs(foot, foot_byid, nhrs, city, shp_file)

    # ### effective lights convolution from the script calc_lights.r
    print("Calculating effective lights convolution...")
    eff_lights_func_nhrs(foot, foot_byid, nhrs, city, shp_file)
    
    # ### edgar convolution from the script calc_edgar.r
    print("Calculating EDGAR convolution...")
    edgar_func_nhrs_v7_yearly(foot, foot_byid, nhrs, city, shp_file)
    
    ### P_conv, pop count and pop density convolution. needed for the population weighted emission per capita
    print("Calculating population weighted emission per capita...")
    footprint_pop_density_nhrs(foot, foot_byid, nhrs, city, shp_file)
    # footprint_pop_density_nhrs_yearly(foot, foot_byid, nhrs, city, shp_file)
}
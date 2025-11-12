
### function to create the ouput gdp, pps, horr_err, consumption, and bio fluxes.
### need to add edgar data
### bio is based on smurf data. It is extraplolated from 2014-2018 to whatevere year one is running after 2019
### need to find a better way to do this. check the interpolation methodolgy


generate_outputtxtfile=function( foot_fn,trajerr_fn, traj_fn,pop_data, results, results_err,
                            site, fresh_start=TRUE) {
  # Load necessary libraries
    r_files <- list.files('/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/functions/socio_economic_convolutions/', pattern = '\\.r$', full.names = TRUE)
    print(r_files)
    invisible(lapply(r_files, source))
    print(paste0("fresh_start is set to ", fresh_start))
    # fresh_start=FALSE # set to TRUE if you want to run only the odiac convolution functions
# get urban extent shapefile
 
    
    # fresh_start=TRUE # set to TRUE if you want to run only the odiac convolution functions
    for (i in seq_along(foot_fn)) {
    # for (i in seq_along(results)) {
      # Print the current file being processed
      
      foot_exist<-file.exists(foot_fn[i])
      footerr_exist<-file.exists(trajerr_fn[i])
      # if(!foot_exist && !footerr_exist){
      if(results[i] != TRUE  && results_err[i] != TRUE){
        print(paste("Footprint file does not exist:", foot_fn[i], "- Skipping."))
        next
      }else if(results[i] == TRUE  && results_err[i] == TRUE){
        print(paste("Both footprint and trajectory error files exist for:", foot_fn[i], " ",trajerr_fn[i]))
        # print(paste("Footprint file exists:", foot_fn[i]))
        # print(paste("Processing file:", foot_fn[i]))
      
        # Example: Call a function (e.g., PPS_func) to process the file
        # Save the output for each file
        pps_output_file <- paste0(dirname(foot_fn[i]), "/pps_beforenhrs.txt")
        gdp_output_file <- paste0(dirname(foot_fn[i]), "/gdp_beforenhrs.txt")
        consumption_output_file <- paste0(dirname(foot_fn[i]), "/consumption_beforenhrs.txt")
        horr_err_output_file <- paste0(dirname(foot_fn[i]), "/horr_trans_err.txt")
        bio_output_file <- paste0(dirname(foot_fn[i]), "/bio_beforenhrs.txt")
        intersect_file <- paste0(dirname(foot_fn[i]), "/intersect_beforenhrs.txt")
        urban_influence_file <- paste0(dirname(foot_fn[i]), "/urban_influence.txt")
        city_influence_file <- paste0(dirname(foot_fn[i]), "/city_influence.txt")
        odiac_city_conv_file <- paste0(dirname(foot_fn[i]), "/odiac_city_influence_.txt") #shapefile based
        odiac_urban_conv_file <- paste0(dirname(foot_fn[i]), "/odiac_urban_influence.txt") #shapefile based
      
        if (fresh_start) {
          # print("running only odiac shappefile convolution functions")
          # print("running only city convolution and odiac shpapefile convolution functions")
          # if (file.exists(city_influence_file) && file.exists(urban_influence_file) &&
          #     file.exists(odiac_urban_conv_file) && file.exists(odiac_city_conv_file) && file.exists(horr_err_output_file)) {
          #     print(paste("All odiac conv files already exist for:", foot_fn[i], "- Skipping."))
          #     next  # Skip to the next iteration
          # } else {
              urban_extent <- get_urban_extent(site)
              # print(urban_extent)
              # print(paste("in fs:", foot_fn[i]))
              sim_conv = cc_city_convolution(foot_fn[i],site) # city clustering method
              horr_err = horr_pop_update(trajerr_fn[i],foot_fn[i], pop_data)
              odiac_city_conv = shp_city_convolution(foot_fn[i],site,urban_extent) # this is using shapefiles, UMichigan and Loren Brinks method
              write.table(horr_err, file = horr_err_output_file, quote = FALSE, row.names = FALSE)
              print(paste("Saved horr_err output to:", horr_err_output_file))

              # horr_err_output_file <- paste0(dirname(foot.fns[i]), "/horr_trans_err.txt")
              # odiac_city_conv = shp_city_convolution(foot.fn[i],site,urban_extent) # this is using shapefiles, UMichigan and Loren Brinks method
              # sim_conv = cc_city_convolution(foot.fn[i],site) # city clustering method
              # horr_err = horr_pop_update(trajerr.fn[i],foot.fns[i], pop_data)
              # write.table(horr_err, file = horr_err_output_file, quote = FALSE, row.names = FALSE)
              # print(paste("Saved horr_err output to:", horr_err_output_file))
          # }

        } else {
          print("running functions to create *_before_nhrs_files for comparison. NOTE: does not use the nhrs data and not needed for most runs, unless one wants to 
          compare before and after nhrs results")
          if (file.exists(pps_output_file) && file.exists(gdp_output_file) &&
          file.exists(consumption_output_file) && file.exists(horr_err_output_file) &&
          file.exists(bio_output_file) && file.exists(intersect_file) &&
          file.exists(city_influence_file) && file.exists(urban_influence_file) &&
          file.exists(odiac_city_conv_files) && file.exists(odiac_urban_conv_file)) {
            print(paste("All output files already exist for:", foot_fn[i], "- Skipping."))
            next  # Skip to the next iteration
          } else {
        
              print(paste("Processing file:", foot_fn[i]))
              
              pps <- PPS_func(foot_fn[i], pop_data)
              gdp<-calc_gdp(foot_fn[i])
              consumption<-consump_func(foot_fn[i])
              bio<-bio_func(foot_fn[i],site)

              # use traj rds
              sim_intersect<-city_intersect(traj_fn[i],site)
              ### use trajerr as it contiains the footprints with and without errors
              # sim_conv<-city_convolution(foot_fn[i],site)
              # odiac_city_conv=odiac_city_convolution(foot_fn[i],site,urban_extent) # this is using shapefiles, Michigan and Loren Brinks method
              
              write.table(pps, file = pps_output_file, quote = FALSE, row.names = FALSE)
              write.table(gdp, file = gdp_output_file, quote = FALSE, row.names = FALSE)
              write.table(consumption, file = consumption_output_file, quote = FALSE, row.names = FALSE)
              
              write.table(bio, file = bio_output_file, quote = FALSE, row.names = FALSE)
              write.table(sim_intersect, file = intersect_file, quote = FALSE, row.names = FALSE)
        
        # Optional: Print a message after processing
              print(paste("Saved pps output to:", pps_output_file))
              print(paste("Saved gdp output to:", gdp_output_file))
              print(paste("Saved consumption output to:", consumption_output_file))
            
              print(paste("Saved SMuRF bio output to:", bio_output_file))
              print(paste("Saved city intersect population output to:", intersect_file))
              print(paste("Saved city influence output to:", city_influence_file))
              print(paste("Saved urban influence output to:", urban_influence_file))
              print(paste("Saved ODIAC city convolution output to:", odiac_urban_conv_file))
            }
        }
    }
  }
}
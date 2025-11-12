# simple function
generate_outputtxtfile_simple=function( byid.path,byiderr.path, pop_data, results, results_err,
                            site) {
                              
    # source( paste0( Sys.getenv("PP_DIR"), "/functions/socio_economic_convolutions/shp_city_convolution.r") )                        
    urban_extent = get_urban_extent(site)
    foot_files <- list.files(byid.path, full.names = TRUE, recursive = FALSE)
    trajerr_files <- list.files(byiderr.path, full.names = TRUE, recursive = FALSE)

    print("in the function generate_outputtxtfile_simple")
    print(site)
    print(byid.path)
    print(byiderr.path)
    print(urban_extent)     
    print(length(foot_files))
    print(length(trajerr_files))
    print(length(results))

    for (i in seq_along(results)) {
        foot_file_i <- list.files(foot_files[i],'X_foot.nc', full.names = TRUE, recursive = FALSE)
        trajerr_file_i <- list.files(trajerr_files[i],'traj.rds', full.names = TRUE, recursive = FALSE)
          
            
        if (length(foot_file_i) > 0 && length(trajerr_file_i) > 0) {
               
            # horr_err_output_file <- paste0(dirname(foot_files[i]), "/horr_trans_err.txt")
            # odiac_city_conv = shp_city_convolution(foot_file_i,site,urban_extent) # this is using shapefiles, UMichigan and Loren Brinks method
            sim_conv = cc_city_convolution(foot_file_i,site) # city clustering method
            # horr_err = horr_pop_update(trajerr_file_i,foot_file_i, pop_data)
            # write.table(horr_err, file = horr_err_output_file, quote = FALSE, row.names = FALSE)
            # print(paste("Saved horr_err output to:", horr_err_output_file))
          }
      }
}
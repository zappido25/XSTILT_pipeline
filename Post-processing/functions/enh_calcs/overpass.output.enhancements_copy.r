
# read the path from the environment variable
# source Login.init.sh for setting the environment variables
overpass.output.enhancements_copy=function(run_failed_jobs, city) {

    library(dplyr)

    city=city[1]
    run_failed_jobs=run_failed_jobs[1]
    lin26_dir=FALSE
    lin20_dir=TRUE
    scratch_dir=FALSE
    vulcan_epc=FALSE

    PP.DIR = Sys.getenv("PP_DIR")
    func.dir=paste0(PP.DIR,"/functions/enh_calcs/")
    version="V11r"
    scratch = "/scratch/general/vast/u6065567/"
    r_files <- list.files(func.dir, pattern = '\\.r$', full.names = TRUE)

    invisible(lapply(r_files, function(file) {
        # print(paste0("Sourcing: ", file))
        source(file)
    }))

    df.path=Sys.getenv("OUT_DF_DIR")
    print(df.path)
    # 
    # df_dir <- paste0(df.path,"dataframe_output_new_pop")
    df_dir <- paste0(df.path,"dataframe_output_gdp2022")
    print(df_dir)
    city.df.path    = list.files(df_dir, full.names = T)
    print(city.df.path)

    OCO.DIR = Sys.getenv("OCO2_DIR")
    lin26 = Sys.getenv("lin26")
        
    if (run_failed_jobs) {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                    header = TRUE, sep = "\t")  
            
    } else {
            overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                    header = TRUE, sep = ",")  # Replace "data.txt" with your file name
            incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                    header = TRUE, sep = "\t") 
    }

    files_types <- c("CC","Umich")
    # files_types <- c("Umich")
   if (lin26_dir) {
    dir_path <- paste0(lin26, "/XSTILT_output/",
                    city, "/", version)
  } else if (lin20_dir) {
     dir_path <- paste0(OCO.DIR, "/XSTILT_output/",
                  city, "/", version)
  } else if (scratch_dir) {
    dir_path <- paste0(scratch, "/XSTILT_output/",
                    city, "/", version)
  }
    
    print(dir_path)
    for (ss in city) {
        matching_indices <- which(overpass_to_model$site %in% ss)

    # Print matching indices for verification
        print(ss)
        print(matching_indices)

        for (ff in files_types) {
            print(paste0("Processing file type: ", ff))
            # output resulting nhrs list
            # nhours_results<-NULL
            
            if(ff=="CC") {
                shp_file=FALSE
                dataframe_file= "_dataframe.txt"
                edgar_sect_file= "_sect_edgar.txt"
                gra2pes_sect_file= "_sect_gra2pes.txt"
                nhrs_file=(paste0(dir_path,"/../nhours_output_",ss,".txt"))
            } else {
                shp_file=TRUE
                dataframe_file= "_dataframe_shp.txt"
                edgar_sect_file= "_sect_edgar_shp.txt"
                gra2pes_sect_file= "_sect_gra2pes_shp.txt"
                nhrs_file=(paste0(dir_path,"/../nhours_output_shp_",ss,".txt"))
            }

            print(dir_path)
            print(nhrs_file)
            nhrs<-read.table(nhrs_file, header=T, stringsAsFactors = F)
            nhrs$timestr<-as.character(nhrs$timestr)

            for (ii in matching_indices) {
            # for (ii in 1:1) {
            
                site=ss
                timestr=overpass_to_model$timestr[ii]
                # timestr="2018101817"
                # print(paste0("CITY::", site))
                # print(paste0("Timestr::", timestr))
                if (timestr < "2015000000") {
                # if (timestr < "2021100000") {
                        print("skipping old runs")
                        print(paste0("CITY::", ss))
                        print(paste0("Timestr::", timestr))
                        next    
                }
            
                if (!run_failed_jobs) {
                    if ((timestr %in% incomplete_runs$timestr)) {
                        print("skipping failed runs")
                        print(paste0("CITY::", ss))
                        print(paste0("Timestr::", timestr))
                        next
                    }
                }else { # run_failed_jobs is TRUE and timestring is not in nhrs updated file, skip the iteration
                    if (!(timestr %in% nhrs$timestr)) {
                        cat("The timestr:", timestr, "is not found in the nhrs_file. Skipping this iteration.\n")
                        next
                    }
                }

                print(paste0("City: ", ss,", Timestr: ", timestr))

                data.files = dir(city.df.path, recursive = TRUE, 
                                pattern = paste0(ss, "_", timestr, dataframe_file), 
                                full.names = TRUE)
                edgar_sect.files=dir(city.df.path, recursive = TRUE, 
                                pattern = paste0(ss, "_", timestr, edgar_sect_file), 
                                full.names = TRUE)
                gra2pes_sect.files=dir(city.df.path, recursive = TRUE, 
                                pattern = paste0(ss, "_", timestr, gra2pes_sect_file), 
                                full.names = TRUE)

                print(paste0("Processing: ",data.files))
                print(paste0("Processing: ",edgar_sect.files))
                print(paste0("Processing: ",gra2pes_sect.files))
                
                #read the data
                data <- read.table(data.files, header = TRUE, sep = " ", stringsAsFactors = FALSE)
                edgar_sect <- read.table(edgar_sect.files, header = TRUE, sep = " ", stringsAsFactors = FALSE)
                gra2pes_sect <- read.table(gra2pes_sect.files, header = TRUE, sep = " ", stringsAsFactors = FALSE)
                data <- as.data.frame(data) 
                edgar_sect <- as.data.frame(edgar_sect) 
                gra2pes_sect <- as.data.frame(gra2pes_sect) 
                MC_brute_force(data,edgar_sect,gra2pes_sect, city, timestr,shp_file=shp_file, vulcan_epc=vulcan_epc)
            }
        }
    }
}



  


    # future work touse adaptive metropolis hastings algorithm
    # data_out=initial_bg_xco2_mean_guess(data)
    # output <- estimate_enhancement_params(data_out, nsteps = 100000, burnin = 10000,
    #                                     prior_means = list(city_ppm = 0.1, odiac_ppm = 0.1, odiac_total = 0.1,
    #                edgar_ppm = 0.1, mean_pps = 100, bio_ppm = NULL, mean_XCO2 = NULL),
    #                                     prior_sds = list(city_ppm = 0.1, odiac_ppm = 0.1, odiac_total = 0.1,
    #                edgar_ppm = 0.1, mean_pps = 50, bio_ppm = NULL, mean_XCO2 = NULL))



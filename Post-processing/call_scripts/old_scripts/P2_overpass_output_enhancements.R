
# read the path from the environment variable
# source Login.init.sh for setting the environment variables
library(dplyr)

timeout = 12 * 60 * 60              # time allowed before terminations in sec
job_time = '4:00:00'               # total job time
slurm_account = 'lin-np'
slurm_partition = 'lin-shared-np'

slurm_options = list(time = '4:00:00', 
                     account = 'lin-np', 
                     partition = 'lin-shared-np',
                     mem = '380000')

PP.DIR = Sys.getenv("PP_DIR")
func.dir=paste0(PP.DIR,"/functions")

args <- commandArgs(trailingOnly = TRUE)

# DEFAULTS

args <- commandArgs(trailingOnly = TRUE)

run_failed_jobs=FALSE
slurm_run=FALSE

if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  run_failed_jobs <- as.logical(args[1])
  print(paste0("Run failed jobs mode is ", ifelse(run_failed_jobs, "ON", "OFF")))
}
if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  slurm_run <- as.logical(args[2])
  print(paste0("Slurm run mode is ", ifelse(slurm_run, "ON", "OFF")))
}
if (length(args) >= 3 && !is.na(args[3]) && nzchar(args[3])) {
  city <- as.character(args[3])
  print(paste0("Selected city is ", city))
}
version="V11r"


r_files <- list.files(func.dir, pattern = '\\.r$', full.names = TRUE)

invisible(lapply(r_files, function(file) {
    # print(paste0("Sourcing: ", file))
    source(file)
}))

# if (shp_file) {
#     dataframe_file= "_dataframe_shp.txt"
#     sect_file= "_sect_shp.txt"
#     # read the shapefile    
# }else {
#     dataframe_file= "_dataframe.txt"
#     sect_file= "_sect.txt"
# }

df.path=Sys.getenv("OUT_DF_DIR")
print(df.path)
# 
df_dir <- paste0(df.path,"dataframe_output")
print(df_dir)
city.df.path    = list.files(df_dir, full.names = T)
print(city.df.path)

OCO.DIR = Sys.getenv("OCO2_DIR")
    
if (run_failed_jobs) {
       overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_all.txt"),
                header = TRUE, sep = "\t")  
        
} else {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
        incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_update.txt"),
                header = TRUE, sep = "\t") 
}

# files_types <- c("CC","Umich")
files_types <- c("CC")
dir_path <- paste0(OCO.DIR, "/XSTILT_output/",
                city, "/", version)

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
            sect_file= "_sect.txt"
            nhrs_file=(paste0(dir_path,"/../nhours_output_",ss,".txt"))
        } else {
            shp_file=TRUE
            dataframe_file= "_dataframe_shp.txt"
            sect_file= "_sect_shp.txt" 
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
            # timestr="2015111720"
            # print(paste0("CITY::", site))
            # print(paste0("Timestr::", timestr))
            if ( timestr < '2015000000') {
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
            sect.files=dir(city.df.path, recursive = TRUE, 
                            pattern = paste0(ss, "_", timestr, sect_file), 
                            full.names = TRUE)

            print(paste0("Processing: ",data.files))
            print(paste0("Processing: ",sect.files))


            params <- data.frame(
                foot_fn = foot.fns,          # Footprint file paths
                trajerr_fn = trajerr.fns,    # Trajectory error file paths
                traj_fn = traj.fns,          # Trajectory file paths
                pop_data = rep(pop_data, length(foot.fns)),  # Repeat for each row
                site = rep(site, length(foot.fns)),
                fresh_start = fresh_start
            )
            #read the data
            data <- read.table(data.files, header = TRUE, sep = " ", stringsAsFactors = FALSE)
            sect <- read.table(sect.files, header = TRUE, sep = " ", stringsAsFactors = FALSE)
            data <- as.data.frame(data) 
            sect <- as.data.frame(sect) 
            MC_brute_force(data,sect, city, timestr,shp_file=shp_file)
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



# create dataframes for enhancements calculations
 
 
homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)
setwd(xstilt_wd)

r_files <- list.files('Post-processing/functions', pattern = '\\.r$', full.names = TRUE)
# print(r_files)
invisible(lapply(r_files, source))

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



# DEFAULTS
# shp_file=FALSE

# if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
#   shp_file <- as.logical(args[1])
# }

# if (shp_file) {
#   cat("Umich method.\n")
# } else {
#   cat("City clustering method.\n")
# }

# stop("Stopping execution for debugging purposes. Check the variables shp_file and city.")
# variables for convolution
OCO.DIR = Sys.getenv("OCO2_DIR")
group = Sys.getenv("group")
version = Sys.getenv("version")
print(version)
print(group)

if (run_failed_jobs) {
       overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_all.txt"),
                header = TRUE, sep = "\t")  
        
} else {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
        incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log_update.txt"),
                header = TRUE, sep = "\t") 
}

urban_core_info=read.table("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Cities_input_files/urban_core_info.txt", 
              header=TRUE, quote="")

files_types <- c("CC","Umich")
# files_types <- c("Umich")

dir_path <- paste0(OCO.DIR, "/XSTILT_output/",
                city, "/", version)
# city=c("Phoenix")
# for (ss in CONUS$City) {
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
        nhrs_file=(paste0(dir_path,"/../nhours_output_",ss,".txt"))
      } else {
        shp_file=TRUE
        nhrs_file=(paste0(dir_path,"/../nhours_output_shp_",ss,".txt"))  
      }

      nhrs<-read.table(nhrs_file, header=T, stringsAsFactors = F)
      nhrs$timestr<-as.character(nhrs$timestr)
    
      for (ii in matching_indices) {
        site=ss
        timestr=overpass_to_model$timestr[ii]
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
        } else {
          if (!(timestr %in% nhrs$timestr)) {
            cat("The timestr:", timestr, "is not found in the nhrs_file. Skipping this iteration.\n")
            next
          }
        }

      
        # dir_path <- paste0(OCO.DIR, "/XSTILT_output/",
        #               ss, "/", version)
        dat <- dir(dir_path, full.names = TRUE)      
      
        xco2_fname=paste0(site,"_",timestr,".txt")
        XCO2_overpass <- read.table(paste0(file.path(OCO.DIR, "/OCO-2/overpass_obs/",xco2_fname)),
          header = TRUE)  # Replace "data.txt" with your file name
            
          
        # the covolutions are stored in out folder and the horr err is stored in out_folder
   
          out.path    = dat[grepl(paste0("out_",timestr), basename(dat))]
          out.path <- out.path[!grepl("\\.png$", out.path)]
          byid.path = file.path(out.path, 'by-id')
        # outerr.path    = dat[grepl(paste0("outerr_",timestr), basename(dat))]
        # outerr.path <- outerr.path[!grepl("\\.png$", outerr.path)]
        # print(out.path)

          print(paste0("File type: ", ff))
          overpass_dataframe(city, timestr, byid.path, out.path, nhrs, urban_core_info, 
                            XCO2_overpass, shp_file=shp_file)
      } 
    }
}
# serial testing


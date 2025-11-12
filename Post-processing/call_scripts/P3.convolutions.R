# this is the convolution script Carlton modified based on Kai's parallel_convolution_0.5.R
# it convolves the footprints with different data meterics based on the nhrs data

# Load the librarires
library('rslurm')
library("raster")
library("parallel")
library("ncdf4")
library("dplyr")


args <- commandArgs(trailingOnly = TRUE)

run_failed_jobs=FALSE
parallel=TRUE

lin26_dir=FALSE
lin20_dir=TRUE
scratch_dir=FALSE

if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  run_failed_jobs <- as.logical(args[1])
  print(paste0("Run failed jobs mode is ", ifelse(run_failed_jobs, "ON", "OFF")))
}
if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  city <- as.character(args[2])
  print(paste0("Selected city is ", city))
}


# source function
homedir = Sys.getenv("XSTILT_PATH")
xstilt_wd = file.path(homedir)
setwd(xstilt_wd)

r_files <- list.files('Post-processing/functions/socio_economic_convolutions/', pattern = '\\.r$', full.names = TRUE)
# print(r_files)
invisible(lapply(r_files, source))



# SLURM settings
timeout = 24 * 60 * 60              # time allowed before terminations in sec
job_time = '48:00:00'               # total job time
slurm_account = 'lin-np'
slurm_partition = 'lin-shared-np'
# slurm_partition = 'lin-np'

slurm_options = list(time = '24:00:00', 
                     account = slurm_account, 
                     partition = slurm_partition,
                     mem = '380000')


# going to run this for all directories
# will overwrite, but just seems like less of a headache

# variables for directory paths
group<-"lin-group20"
version="V11r"


#epd is effective_pop_density convolved with footprint
# type<-c("pps", "gdp", "consumption","gdp_conv", "bio","lights","edgar","epd")
# type<-c("pps","lights","epd")
# type<-c("consumption_conv")



# variables for convolution
OCO.DIR = Sys.getenv("OCO2_DIR")
lin26 = Sys.getenv("lin26")
scratch = "/scratch/general/vast/u6065567/"

if (run_failed_jobs) {
       overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")  
        
} else {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
        incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t") 
}
 
#  files_types <- c("CC","Umich")
files_types <- c("Umich")

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
    
check_time=c("2022081319", "2022111019","2024041219","2024081819")
for (ss in city) {
  matching_indices <- which(overpass_to_model$site %in% ss)

  # Print matching indices for verification
  print(ss) 
  print(matching_indices)

  for (ff in files_types) {
    print(paste0("Processing file type: ", ff, " for city: ",city))
    # output resulting nhrs list
    # nhours_results<-NULL
    
    if(ff=="CC") {
      shp_file=FALSE
      nhrs_file=(paste0(dir_path,"/../nhours_output_",ss,".txt"))
    } else {
      shp_file=TRUE
      nhrs_file=(paste0(dir_path,"/../nhours_output_shp_",ss,".txt"))  
    }

    print(nhrs_file)
    for (ii in matching_indices) {
      # for (ii in 1:4) {
          site=ss
          # timestr="2024041418"
          timestr=overpass_to_model$timestr[ii]
          # timestr=check_time[ii]
          # print(paste0("CITY::", site))
          print(paste0("Timestr::", timestr) )
          # if ( timestr < '2015000000' || timestr == '2022051418' ) {
          if ( timestr < '2015000000' ) {
                  print("skipping old runs or unprocessed run")
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
        } 
      
       # the nhrs file is generated in the out_ directory 
      # if (shp_file) {
      #   nhrs_file=(paste0(dir_path,"/../nhours_output_shp_",site,".txt"))  
      # }else{
      #   nhrs_file=(paste0(dir_path,"/../nhours_output_",site,".txt"))               
      # }         
      # make sure this is the correct nhrs file
        if (file.exists(nhrs_file)) {
          # Use system command to count lines
          num_lines <- as.numeric(system(paste("wc -l", nhrs_file, "| awk '{print $1}'"), intern = TRUE))
          if (num_lines == 0) {
            stop("The nhrs_file is empty.")
          }
          else if (num_lines > 0) {
            cat("The nhrs_file exists and has", num_lines, "lines.\n")
          }
          
        } else {
          stop("The nhrs_file does not exist.")
        }

      # Need to add nhrs for data
        nhrs<-read.table(nhrs_file, header=T, stringsAsFactors = F)
        nhrs$timestr<-as.character(nhrs$timestr)
        nhrs$nhrs_ecdf[nhrs$nhrs_ecdf == -Inf] <- 24
        # run_failed_jobs is TRUE and timestring is not in nhrs updated file, skip the iteration
        if (run_failed_jobs)  {
          if (!(timestr %in% nhrs$timestr)) { 
            cat("The timestr:", timestr, "is not found in the nhrs_file. Skipping this iteration.\n")
            next
          }
        }
        if (!dir.exists(dir_path)) {
          stop(paste("Directory does not exist:", dir_path))
        }

        # omit files with .R, .txt, and .png extensions
        dat <- dir(dir_path, full.names = TRUE)
        dat<-dat[substr(dat, nchar(dat)-1, nchar(dat)) != ".R"]
        dat<-dat[substr(dat, nchar(dat)-3, nchar(dat)) != ".txt"]
        dat<-dat[substr(dat, nchar(dat)-3, nchar(dat)) != ".png"]

        # get directories for out_ and outerr_
        dir_out <- dat[grepl(paste0("out_",timestr), basename(dat))]
        dir_out_err <- dat[grepl(paste0("outerr_",timestr), basename(dat))]
        print(dir_out)
      

        # get all the receptor folders
        receptors<-c()
        for(j in dir_out){
          j_dat <- dir(paste0(j, "/by-id"), full.names = TRUE)
      
        # if convolved footprints exisit, skip the folders
        # complete<-file.exists(paste0(j_dat,"/",type,".txt"))
        # j_dat<-j_dat[complete == FALSE]
          receptors<-c(receptors, j_dat)
        }
        print(paste0("Number of receptors: ", length(receptors)))

      # get city and time string from the receptor file name
        recp_city<-basename(dirname(dirname(dirname(dirname(receptors)))))
        recp_timestr<-substr(basename(dirname(dirname(receptors))),5,14)


        # create a data frame with the city, time string, and count and add nhrs for each receptor
        count<-1:length(receptors)
        recp_df<-data.frame(city = recp_city, timestr = recp_timestr, count = count)
        recp_df<-inner_join(recp_df, nhrs)
        receptors<-receptors[recp_df$count]


        foot_byid<-receptors
        cities_not_in_lin20=c("Detroit","Phoenix","Chicago")
        # if (city=="Detroit") {
        if (city %in% cities_not_in_lin20) {
          recp_time_overpass=basename(receptors)
          foot<-paste0(receptors,'/',recp_time_overpass,"_foot.nc")
          # city<-basename(dirname(dirname(dirname(dirname(foot)))))
        }else {# 
          foot<-gsub("by-id","footprints", receptors)
          foot<-paste0(foot,"_foot.nc")
          # city<-basename(dirname(dirname(dirname(dirname(foot)))))
        }
        print(city)
   

      # foot<-paste0(foot,"_0.05x0.05_hrly_foot.nc")
      
        print(paste0("length of foot:", length(foot)))
        print(paste0("length of foot_byid:", length(foot_byid)))
        # print(paste0("length of foot:", length(recp_df$nhrs_ecdf)))
        # print(paste0("length of foot:", length(city)))
        # print(paste0("length of foot:", length(shp_file)))
        # print(dim())
      
        
        df<-data.frame(foot, foot_byid, nhrs = recp_df$nhrs_ecdf, city=city,shp_file=shp_file)
        # Ensure all vectors have the same length before creating the data frame
        # min_len <- min(length(foot), length(foot_byid), length(recp_df$nhrs_ecdf), length(city), length(shp_file))
        # df <- data.frame(
        #   foot = foot[1:min_len],
        #   foot_byid = foot_byid[1:min_len],
        #   nhrs = recp_df$nhrs_ecdf[1:min_len],
        #   city = city[1:min_len],
        #   shp_file = rep(shp_file, min_len),
        #   stringsAsFactors = FALSE
        # )
        colnames(df) <- c("foot", "foot_byid", "nhrs","city","shp_file")
        vect<-sample(1:length(df$foot))

        df<-df[vect, ]

      # run in parallel

    
        if (parallel) {
          
            slurm_job = slurm_apply(
            f=run.convolutions,
            params = df,
            nodes = 2,
            cpus_per_node = 40,
            slurm_options = slurm_options,
            jobname = paste0(ff,"_", timestr)
            )
            jobname <- slurm_job$jobname
            cat("Submitted job with name:", jobname, "\n")
            job_status <- system(paste("squeue --name", jobname, "--format=%i"), intern = TRUE)
            cat("Job ID(s):\n", job_status, "\n")
          
        } else {
          # for (ii in names(par_fun_list)) {
          #   cat("Running serial for type:", ii, "\n")
            
            print("GRAPES conv")
            calc.grapes.convolution(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

            # print("Calculating effective population density...")
            # eff_PPS_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file) 
            # # eff_PPS_func_nhrs_yearly(df$foot, df$foot_byid, df$nhrs, df$city, shp_file) 

            # # # effective gdp convolution from the script calc_gdp.r. footprint is not normalized
            # print("Calculating effective GDP...")
            # calc_eff_gdp_2022_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

            # # # gdp convolution of footprint*gdp
            # print("Calculating GDP convolution...")
            # calc_gdp_conv_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

            # # # consumption convolution from the script calc_consumption.r
            # print("Calculating effective consumption...")
            # eff_consump_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

            # # ### bio convolution from the script calc_bio.r 
            # print("Calculating bio convolution...")
            # bio_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

            # ### effective lights convolution from the script calc_lights.r
            # print("Calculating effective lights convolution...")
            # eff_lights_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, df$shp_file)

            # # ## edgar convolution from the script calc_edgar.r
            # print("Calculating EDGAR convolution...")
            # edgar_func_nhrs_v7_yearly(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

            # # # P_conv, pop count and pop density convolution. needed for the population weighted emission per capita
            # print("Calculating population weighted emission per capita...")
            # footprint_pop_density_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)
            # # footprint_pop_density_nhrs_yearly(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)
        }
        # }
    }
    }
  
  }
# }
  # 


    # consump_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city) # for some reason this is crashes after 1st iteration
    # lights_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city) # for some reason this is crashes after 1st iteration
    # bio_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city) # for some reason this is crashes after 1st iteration
    # bio_func(df$foot, df$city) # for some reason this is crashes after 1st iteration
    
# try in serial to find errors
# lights_func_nhrs(df$foot, df$foot_byid, df$nhrs, df$city) # for some reason this is crashes after 1st iteration




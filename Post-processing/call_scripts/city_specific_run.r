# this is the convolution script Carlton modified based on Kai's parallel_convolution_0.5.R
# it convolves the footprints with different data meterics based on the nhrs data

# Load the librarires
run_city_specific_conv=function(city_df){

  library('rslurm')
  library("raster")
  library("parallel")
  library("ncdf4")
  library("dplyr")


  lin26_dir=FALSE
  lin20_dir=TRUE
  scratch_dir=FALSE

  # cities=c("Orlando")


  # source function
  homedir = Sys.getenv("XSTILT_PATH")
  xstilt_wd = file.path(homedir)
  setwd(xstilt_wd)

  source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/functions/socio_economic_convolutions/calc.GRA2PES.convolution.r")
  source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/functions/socio_economic_convolutions/calc_gdp_2022.r")
  source("/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/X-STILT/Post-processing/functions/socio_economic_convolutions/shpfiles.city.convolution.r")


   # variables for directory paths
  group<-"lin-group20"
  version="V11r"

  city=city_df[1]
  print(paste0("in the function:", city))


  # variables for convolution
  OCO.DIR = Sys.getenv("OCO2_DIR")
  lin26 = Sys.getenv("lin26")
  scratch = "/scratch/general/vast/u6065567/"

          
  overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
          header = TRUE, sep = ",")  # Replace "data.txt" with your file name
  incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
          header = TRUE, sep = "\t") 

  
  files_types <- c("CC","Umich")
 

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
  matching_indices <- which(overpass_to_model$site %in% city)

  # Print matching indices for verification
  print(city) 
  print(matching_indices)

  for (ff in files_types) {
    print(paste0("Processing file type: ", ff, " for city: ",city))
    # output resulting nhrs list
    # nhours_results<-NULL
    
    if(ff=="CC") {
      shp_file=FALSE
      nhrs_file=(paste0(dir_path,"/../nhours_output_",city,".txt"))
    } else {
      shp_file=TRUE
      nhrs_file=(paste0(dir_path,"/../nhours_output_shp_",city,".txt"))  
    }

      print(nhrs_file)
      for (ii in matching_indices) {

            site=city
            
            timestr=overpass_to_model$timestr[ii]
       
            print(paste0("Timestr::", timestr) )
           
            if ( timestr < '2015050000' || timestr %in% incomplete_runs$timestr ) {
                    print("skipping old runs or unprocessed run")
                    print(paste0("CITY::", city))
                    print(paste0("Timestr::", timestr))
                    next
                
            }
    
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
      
        
          
          df<-data.frame(foot, foot_byid, nhrs = recp_df$nhrs_ecdf, city=city,shp_file=shp_file)
         
          colnames(df) <- c("foot", "foot_byid", "nhrs","city","shp_file")
          vect<-sample(1:length(df$foot))

          df<-df[vect, ]

         
          print("GRAPES conv")
          calc.grapes.convolution(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)

          print("Calculating effective GDP USING 2022 data...")
          calc_eff_gdp_2022_nhrs(df$foot, df$foot_byid, df$nhrs, df$city, shp_file)
          
        # }
    }
    }
  
  }

#
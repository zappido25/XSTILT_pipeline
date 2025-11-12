# this script calculates the nhrs after which the urban enhanced and background enhancement sare indistinughable
library(ggplot2)
library(dplyr)

args <- commandArgs(trailingOnly = TRUE)

run_failed_jobs=FALSE


if (length(args) >= 1 && !is.na(args[1]) && nzchar(args[1])) {
  run_failed_jobs <- as.logical(args[1])
  print(paste0("Run failed jobs mode is ", ifelse(run_failed_jobs, "ON", "OFF")))
}
if (length(args) >= 2 && !is.na(args[2]) && nzchar(args[2])) {
  city <- as.character(args[2])
  print(paste0("Selected city is ", city))
}


group<-"lin-group20"
version="V11r"
# site="Phoenix"
# get cities in group folder
# dir_path <- paste0("/uufs/chpc.utah.edu/common/home/", group, "/CarlXav/X-STILT/OCO2_2021_test/XSTILT_output/",
#                     site, "/", version)

OCO.DIR = Sys.getenv("OCO2_DIR")
lin26 = Sys.getenv("lin26")
scratch = "/scratch/general/vast/u6065567/"


lin26_dir=FALSE
lin20_dir=TRUE
scratch_dir=FALSE

if (run_failed_jobs) {
       overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")   
      
} else {
        overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
                header = TRUE, sep = ",")  # Replace "data.txt" with your file name
        incomplete_runs<- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/failed_runs.txt"),
                header = TRUE, sep = "\t")   
}

# overpass_to_model <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/overpass_to_model.txt"),
#                 header = TRUE, sep = ",")  # Replace "data.txt" with your file name
# incomplete_runs <- read.table(file.path(OCO.DIR, "/OCO-2/overpass_city/incomplete_log.txt"),
#                 header = TRUE, sep = "\t")  

# city=c("SanDiego")
# for (ss in CONUS$City) {
files_types <- c("Umich")
# files_types <- c("CC","Umich")

check_time=c("2022081319", "2022111019","2024041219","2024081819")
for (ss in city) {
    matching_indices <- which(overpass_to_model$site %in% ss)

# Print matching indices for verification
  print(ss)
  print(matching_indices)

  for (ff in files_types) {
    print(paste0("Processing file type: ", ff))
    # output resulting nhrs list
    nhours_results<-NULL
    
    if(ff=="CC") {
      all_fname_pattern="urban"
      city_fname_pattern="city"
      thresh_plot_fname="_thresh_test.png"
      nhrs_plot_fname="_nhrs.png"
      outfile_fname="/nhours_output_"
    } else {
      all_fname_pattern="odiac_urban"
      city_fname_pattern="odiac_city"
      thresh_plot_fname="_thresh_test_shp.png"
      nhrs_plot_fname="_nhrs_shp.png"
      outfile_fname="/nhours_output_shp_"
    }

    for (ii in matching_indices) {
    # for (ii in 1:4) {
      site=ss
      timestr=overpass_to_model$timestr[ii]
      # timestr=check_time[ii]
      # timestr=c("2021083118", "2021112818")
      # timestr="2022051418"
      # print(paste0("CITY::", site))
      # print(paste0("Timestr::", timestr))
      if (!run_failed_jobs){
        if ((timestr %in% incomplete_runs$timestr)) {
            print("skipping failed runs")
            print(paste0("CITY::", ss))
            print(paste0("Timestr::", timestr))
            next
        } 
      }

      if (lin26_dir) {
        dir_path <- paste0(lin26, "/XSTILT_output/",
                          ss, "/", version)
        nhrs_output_path <- paste0(lin26, "/XSTILT_output/",
                      ss)
      } else if (lin20_dir) {
        dir_path <- paste0(OCO.DIR, "/XSTILT_output/",
                      ss, "/", version)
        nhrs_output_path <- paste0(OCO.DIR, "/XSTILT_output/",
                      ss)
      } else if (scratch_dir) {
        dir_path <- paste0(scratch, "/XSTILT_output/",
                        ss, "/", version)
        nhrs_output_path <- paste0(scratch, "/XSTILT_output/",
                      ss)
      }

      print(paste0("dir_path: ", dir_path))

      # Check if plotting folder exists, if not, create the folder
      plot_dir <- paste0(OCO.DIR, "/XSTILT_output/", ss, "/plots")
      # plot_dir <- paste0(lin26, "/XSTILT_output/", ss, "/plots")
      if (!dir.exists(plot_dir)) {
        print(paste0("Creating directory: ", plot_dir))
        dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
      }
    

      dat <- dir(dir_path, full.names = TRUE)

      ## remove the .R, .png and .txt files
      dat<-dat[substr(dat, nchar(dat)-1, nchar(dat)) != ".R"]
      dat<-dat[substr(dat, nchar(dat)-3, nchar(dat)) != ".txt"]
      dat<-dat[substr(dat, nchar(dat)-3, nchar(dat)) != ".png"]

      # get only the folder with out_ in the name 
      dir_out <- dat[grepl(paste0("out_",timestr), basename(dat))]

      # get the time string from the directory name
      # timestr=substr(basename(dir_out), 5,14)
      # print(timestr)

      print(paste0("dir_out: ", dir_out))

  
      for(j in dir_out){

        j_parts<-length(dir(paste0(j,"/particles")))

        if(j_parts == 0){next()}
        # this part is slow
        # j_sim<-dir(paste0(j,"/by-id"), pattern="urban", full.names=T, recursive=T)

        # all<-dir(paste0(j,"/by-id"), pattern="urban", full.names=T, recursive=T)
        # city<-dir(paste0(j,"/by-id"), pattern="city", full.names=T, recursive=T)
        # all<-dir(paste0(j,"/by-id"), pattern="odiac_urban", full.names=T, recursive=T)
        # city<-dir(paste0(j,"/by-id"), pattern="odiac_city", full.names=T, recursive=T)
        all<-dir(paste0(j,"/by-id"), pattern=all_fname_pattern, full.names=T, recursive=T)
        city<-dir(paste0(j,"/by-id"), pattern=city_fname_pattern, full.names=T, recursive=T)
        print(paste0("length of all:", length(all)))
        print(paste0("length of all:", length(city)))
        # Skip iteration if either 'all' or 'city' files do not exist
        if (length(all) == 0 || length(city) == 0) {
          next
        }
      
        print(paste0("number of by-id files:",length(all)))
        print(paste0("number of city files:",length(city)))
        print(paste0("number of j_parts:",j_parts))
        # print(all[1])
     
        if(length(all)/j_parts < 0.95){next()}
        
        lons<-as.numeric(unlist(strsplit(basename(dirname(all)), "_"))[seq(2, length(unlist(strsplit(basename(dirname(all)), "_"))), 4)])
        lats<-as.numeric(unlist(strsplit(basename(dirname(all)), "_"))[seq(3, length(unlist(strsplit(basename(dirname(all)), "_"))), 4)])
        
        # create a sequence of latitudes binned by 0.05 degrees    
        lat_seq<-seq(floor(min(lats)), ceiling(max(lats)), 0.05)
      
        combined_all<-NULL
        combined_city<-NULL   
        total_files_all<-NULL
        total_files_city<-NULL  
          # for city files
        for(k in 1:(length(lat_seq)-1)){
          # files<-j_sim[lats > lat_seq[k] & lats < lat_seq[k+1]]
          files_all<-all[lats > lat_seq[k] & lats < lat_seq[k+1]]
          files_city<-city[lats > lat_seq[k] & lats < lat_seq[k+1]]

          total_files_all<-c(total_files_all, length(files_all))
          total_files_city<-c(total_files_city, length(files_city))
      
          if(length(files_city) == 0 && length(files_all) == 0){
            vect_city<-rep(NA, 24)
            combined_city<-rbind(combined_city, vect_city)

            vect_all<-rep(NA, 24)
            combined_all<-rbind(combined_all, vect_all)
          
            next()
          }
        
          vect_all<-0
          vect_city<-0
          # for all files
          for(l in files_all){
            # print(l)
            l_prof<-try(read.table(l, header=T, stringsAsFactors = F), silent=T)
          
            if(length(l_prof) == 1){next()}
            l_vect<-l_prof$conv[length(l_prof$conv):1]
          
            if(length(l_vect) < 24){
              l_vect<-c(l_vect, rep(0, 24-length(l_vect)))
            }
            vect_all<-vect_all+l_vect
          }
          
          vect_all<-vect_all/length(files_all)
      
          combined_all<-rbind(combined_all, vect_all)

          # for city files
          for(l in files_city){
            # print(l)
            l_prof<-try(read.table(l, header=T, stringsAsFactors = F), silent=T)
          
            if(length(l_prof) == 1){next()}
            l_vect<-l_prof$conv[length(l_prof$conv):1]
          
            if(length(l_vect) < 24){
              l_vect<-c(l_vect, rep(0, 24-length(l_vect)))
            }
            vect_city<-vect_city+l_vect
          }
          vect_city<-vect_city/length(files_city)
          
          combined_city<-rbind(combined_city, vect_city)

        }

        df<-data.frame(lats = rep(lat_seq[1:(length(lat_seq)-1)]+0.05, dim(combined_all)[2]), 
                        time = rep(1:dim(combined_all)[2], each = length(lat_seq)-1), 
                        enhancement = as.vector(combined_all), city = as.vector(combined_city))
      
        df<-na.omit(df)
     
        urban_enhanced_range<-range(df$lats[df$city > 0])
        p<-ggplot()+geom_tile(aes(x = time, y = lats, fill = enhancement), data = df, width = 1, height =0.05)+
        scale_fill_gradientn(colors = c("purple","blue", "green", "yellow","orange","red"))+
        # ggtitle(paste0(basename(dirname(overpass)),", ", basename(overpass)))+
        #color city extents with cyan
        geom_tile(aes(x = time, y = lats), fill = NA, color = "cyan", data = df[df$city > 0, ], width = 1, height =0.05, lwd=0.75)
        # add urban enhanced_range
        urban_df<-data.frame(lat = c(min(urban_enhanced_range)-0.025, max(urban_enhanced_range)+0.025), time = c(0,0) )
        p<-p+geom_line(aes(x=time, y=lat), data=urban_df, color = "darkred", lwd =2)

        # likely background range
        bg_df<-df[df$lats < urban_enhanced_range[1] | df$lats > urban_enhanced_range[2],]
        # # if city covers all bins, go find the latitude bin of smallest enhancement
        if(length(bg_df[,1]) == 0){
          bg_df<-df
        }
        bg_df<-bg_df %>% group_by(lats) %>% mutate(total = sum(enhancement))
        bg_df<-as.data.frame(bg_df)

        # the background latitude is the one with the minimum total enhancement
        bg_lats<-bg_df$lats[bg_df$total == min(bg_df$total)]
        bg_lat<-bg_lats[1]
        bg_df<-data.frame(lat = c(bg_lat - 0.025, bg_lat + 0.025), time = c(0,0))
        p<-p+geom_line(aes(x=time, y=lat), data = bg_df, color ="magenta", lwd=2)
        
        # reasonable cutoff time
        bg_prof<-df[df$lats == bg_lat, ][, 2:3]
        colnames(bg_prof)[2]<-"bg"
        enhanced_profs<-df[df$lats >= min(urban_df$lat) & df$lats <= max(urban_df$lat), ]
        enhanced_profs<-inner_join(enhanced_profs, bg_prof)
        enhanced_profs$diff<-enhanced_profs$enhancement - enhanced_profs$bg
      

        # Original method
        thresh <- max(enhanced_profs$diff, na.rm = TRUE) * 0.1
        thresh <- min(max(thresh, 0.01), 0.2)
        enhanced_profs$thresh<-enhanced_profs$diff < thresh

        # ECDF-based threshold: 10th percentile of non-zero diff
        ecdf_thresh <- max(quantile(enhanced_profs$diff[enhanced_profs$diff > 0], probs = 0.10, na.rm = TRUE), 0.01)
        enhanced_profs$thresh_ecdf <- enhanced_profs$diff < ecdf_thresh
        

        # Create ECDF function
        ecdf_diff <- ecdf(enhanced_profs$diff)

        # Create plot-ready data frame
        ecdf_df <- data.frame(
          diff = sort(enhanced_profs$diff),
          percentile = ecdf_diff(sort(enhanced_profs$diff))
        )

    
        ggplot(ecdf_df, aes(x = diff, y = percentile)) +
          geom_step(color = "black", linewidth = 1) +
          geom_vline(xintercept = thresh, color = "blue", linetype = "dashed", linewidth = 1) +
          geom_vline(xintercept = ecdf_thresh, color = "red", linetype = "dotted", linewidth = 1) +
          
          annotate("text", x = thresh, y = 0.95, label = "Max*0.1", color = "blue", angle = 0, vjust = -0.5, size = 4) +
          annotate("text", x = ecdf_thresh, y = 0.65, label = "10% (non-zero)", color = "red", angle = 0, vjust = 1.5, size = 4) +
          
          labs(
            title = "ECDF of Enhancement Differences",
            subtitle = "Comparison of Threshold Strategies",
            x = "Enhancement Difference",
            y = "Cumulative Fraction"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_blank()
          )

          
          # ggsave(paste0(j,"_thresh_test.png"))
          # ggsave(filename = file.path(plot_dir, paste0(basename(j), "_thresh_test_shp.png")), create.dir = TRUE)
          ggsave(filename = file.path(plot_dir, paste0(basename(j), thresh_plot_fname)), create.dir = TRUE)

        cat(" Max-based threshold:", round(thresh, 4), "\n")
        cat(" ECDF-based threshold (10th percentile):", round(ecdf_thresh, 4), "\n")

        get_nhrs_values=function(df_enhance,column){
          times<-c()
          # the values of length(unique(enhanced_profs$thresh[enhanced_profs$time)) is 2 for True/False
          # if the length is 1, then all values are True or False
          # if all values are True, then the time is the max time
          for(i in 1:24){
            
            if(length(unique(column[df_enhance$time == i])) ==1 ){
              if(unique(column[df_enhance$time == i]) ==T){
                times<-c(times, i)
              }
            }
          }
          times<-c(1:24)[!(c(1:24)%in% times)]
          time<-max(times)
          return(time)
        }

        time_thresh<-get_nhrs_values(enhanced_profs, enhanced_profs$thresh) # use the threshold nhrs as time
        time_ecdf<-get_nhrs_values(enhanced_profs, enhanced_profs$thresh_ecdf)


        cat(" NHRS cutoff (max * 0.1):", time_thresh, "\n")
        cat("NHRS cutoff (ECDF 10%):", time_ecdf, "\n")



        time_thresh_df<-data.frame(lat = unique(df$lats), time = rep(time_thresh+0.5, length(unique(df$lats))))
        time_ecdf_df<-data.frame(lat = unique(df$lats), time = rep(time_ecdf+0.5, length(unique(df$lats))))
        p<-p+geom_line(aes(x = time, y=lat), data=time_thresh_df, linetype = "dashed",color="black", lwd=2)
        p<-p+geom_line(aes(x = time, y=lat), data=time_ecdf_df,  linetype = "dotted",color="black", lwd=2)
          
        # ggsave(filename = file.path(plot_dir, paste0(basename(j), "_nhrs_shp.png")), create.dir = TRUE)
        ggsave(filename = file.path(plot_dir, paste0(basename(j), nhrs_plot_fname)), create.dir = TRUE)
        # ggsave(paste0(j,"_nhrs.png"))
        
        sd_vect_all<-apply(combined_all, 2, sd, na.rm=T)
        sd_vect_city<-apply(combined_city, 2, sd, na.rm=T)
        
        thresh_all<-max(sd_vect_all)*0.1
        if(0.2 < thresh_all){thresh_all = 0.2}
        time_all<-max(c(1:24)[sd_vect_all > thresh_all])
        
        thresh_city<-max(sd_vect_city)*0.1
        if(0.2 < thresh_city){thresh_city = 0.2}
        time_city<-max(c(1:24)[sd_vect_city > thresh_city])
        
        # png(paste0(j,"_sd.png"))
        plot(1:24, sd_vect_all, pch = 16, col = "blue", xlab = "Hour", ylab = "SD", main = "SD by Hour")
        points(1:24, sd_vect_city, pch = 17, col = "red")
        legend("topright", legend = c("All", "City"), pch = c(16, 17), col = c("blue", "red"))
        title(j)
        ggsave(filename = file.path(plot_dir, paste0(basename(j), "_sd.png")), create.dir = TRUE)
        dev.off()
        
        nhours_results<-rbind(nhours_results, c(site, timestr,time_thresh, time_ecdf, time_all, time_city))
        colnames(nhours_results) <- c("city", "timestr", "nhrs_thresh", "nhrs_ecdf","nhrs_sd_all", "nhrs_sd_city")
      }
    }
    print(nhours_results)
    print(outfile_fname)
    # Write results to file, appending if file exists
    output_file <- paste0(nhrs_output_path, outfile_fname, site, ".txt")
    if (file.exists(output_file)) {
      # Read existing data
      existing <- try(read.table(output_file, header = TRUE, sep = "", stringsAsFactors = FALSE), silent = TRUE)
      # If file is empty or unreadable, just write new results
      if (inherits(existing, "try-error") || nrow(existing) == 0) {
        write.table(nhours_results, file = output_file, quote = FALSE, row.names = FALSE, col.names = TRUE)
      } else {
        # Combine and remove duplicates (by city and timestr)
        combined <- rbind(existing, nhours_results)
        combined <- combined[!duplicated(combined[, c("city", "timestr")]), ]
        write.table(combined, file = output_file, quote = FALSE, row.names = FALSE, col.names = TRUE)
      }
    } else {
      write.table(nhours_results, file = output_file, quote = FALSE, row.names = FALSE, col.names = TRUE)
    }
    # nhours_results <- NULL  # Reset for the next file type
  }
}

# }

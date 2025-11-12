# # uses 2022 data for all simulations
# # two functions here
# # calc_gdp for generate_post_processing and calc_gdp_nhrs for convolution
# #https://www.nature.com/articles/s41597-025-04487-x for citation

# ## calc_gdp_nhrs for calc_gdp_nhrs
calc_eff_gdp_2022_nhrs<-function(foot, foot_byid, nhrs, city,shp_file){
  library(raster); library(ncdf4)
  
  for (i in 1:length(foot)){
    foot_i=foot[i]
    foot_byid_i=foot_byid[i]
    nhrs_i=nhrs[i]
    print(nhrs_i)
    print(class(nhrs_i))
    nhrs_i <- as.numeric(nhrs_i)
    foot_year<-substr(unlist(strsplit(basename(foot_i),"_"))[1],1,4)
    print(foot_year)
    
    print(paste0("footprint: ", foot_i, " i: ", i))
  # check if file exist
    foot_exist<-file.exists(foot_i)
    
    # select only timesteps for 2:nhrs. 1 is thereceptor timestamp
    foot_timestr<-substr(basename(foot_i), 1, 12)
    foot_timestr<-as.POSIXct(foot_timestr, format = "%Y%m%d%H%M", tz = "UTC")
    foot_timestrs<-seq(from = foot_timestr - (nhrs_i)*60*60,
                      to   = foot_timestr-1,
                      by   = 'hour')
    
    
    if(foot_exist == T){
    # load gdp data
      base_dir= "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GDP"
      gdp_file=file.path(base_dir, "rast_adm1_gdp_perCapita_1990_2022.tif")

      gdp<-brick(gdp_file)
      years2022 <- as.numeric(gsub("\\D", "", names(gdp)))
      years2022 <- years2022[!is.na(years2022)]

      #  get just 2022 data
   
      
      if (foot_year>2022){
        idx <- which(years2022 == 2022)
      } else {
        idx <- which(years2022 == foot_year)
      }
      print(paste0("using gdp year index: ", idx," for foot year ", years2022[idx]))
      # print(gdp)
      # gdp<-subset(gdp, idx)
      gdp <- gdp[[idx]]
      # print(gdp)


      
      # read in foot
      footprint<-try(brick(foot_i),silent = T) # broken foots issues

      if(is.null(dim(footprint)) == FALSE){
          foot_hours<-names(footprint)
          foot_hours<-gsub("X","", foot_hours)
          foot_hours<-substr(foot_hours, 1,16)
          foot_hours<-as.POSIXct(foot_hours, format = "%Y.%m.%d.%H.%M", tz = "UTC")
          
          # check if foot_hours is a a subset of foot_timestrs
          new_flag<-foot_hours %in% foot_timestrs

        if(TRUE %in% new_flag){ # only if foot_hours
          new_flag<-(1:length(new_flag))[new_flag == TRUE]
          footprint<-subset(footprint, new_flag) # this should deal with mssing hours in foots
        
          # sum across layers
          footprint<-calc(footprint, sum)
          footprint_egdp<-calc(footprint, sum)#apply(footprint, c(1,2), sum)
        
          # normalize the footprint
          footprint<-footprint/cellStats(footprint, sum)

        # subset gdp data
        
        # Auckland fix
          x1<-extent(footprint)[1]
          x2<-extent(footprint)[2]


          gdp<-crop(gdp, extent(x1, x2, extent(footprint)[3], extent(footprint)[4]))
          # foot<-crop(foot, extent(gdp))
        

          blank<-raster(nrow = dim(footprint)[1], ncol = dim(footprint)[2], xmn = extent(footprint)[1], xmx = extent(footprint)[2], ymn = extent(footprint)[3], ymx = extent(footprint)[4])
          gdp<-resample(gdp, blank, method = "ngb")
          gdp<-as.matrix(gdp)

          #}
          
          # convolve
          footprint<-as.matrix(footprint)
          footprint_egdp<-as.matrix(footprint_egdp)
          
          # calc convolution
          conv<-sum(footprint*gdp, na.rm=T)
          egdp<-sum(footprint_egdp*gdp, na.rm=T)

          print(paste0("Effective GDP: ", conv))
          print(paste0("Effective EGDP: ", egdp))
          
          if (shp_file){
            fname="/gdp_shp_v2022.txt"
            fname_egdp="/GDP_conv_shp_v2022.txt"
          }else{
            fname="/gdp_v2022.txt"
            fname_egdp="/GDP_conv_v2022.txt"
          }
          # save output and remove garbage
          write.table(conv, file=paste0(foot_byid_i,fname), quote=F, row.names=F)
          write.table(egdp, file=paste0(foot_byid_i,fname_egdp), quote=F, row.names=F)
          # remove gdp and foot  
          gc()
        } #inner flag loop
      } #dim(foot) loop
      } #dim(foot) loop
    } # iteration loop
} # function end
calc_eff_gdp_2022_nhrs<-Vectorize(calc_eff_gdp_2022_nhrs)


# calc_eff_gdp_2022_nhrs <- function(foot, foot_byid, nhrs, city, shp_file) {
#   library(raster); library(ncdf4)

#   for (i in seq_along(foot)) {

#     foot_i      <- foot[i]
#     foot_byid_i <- foot_byid[i]
#     nhrs_i_raw  <- nhrs[i]

#     cat("\n--- i =", i, " ---\n")
#     print(nhrs_i_raw)

#     ## 1) Ensure nhrs_i is a valid numeric
#     nhrs_i <- suppressWarnings(as.numeric(nhrs_i_raw))
#     if (is.na(nhrs_i) || !is.finite(nhrs_i) || nhrs_i <= 0) {
#       warning("Skipping i = ", i, ": invalid nhrs_i = ", nhrs_i_raw)
#       next
#     }

#     ## 2) Extract year and receptor time from filename
#     base_name <- basename(foot_i)

#     # foot_year from first 4 characters (adjust if your naming is different)
#     foot_year_str <- substr(base_name, 1, 4)
#     foot_year     <- suppressWarnings(as.numeric(foot_year_str))

#     if (is.na(foot_year)) {
#       warning("Skipping i = ", i, ": could not parse foot_year from ", base_name)
#       next
#     }

#     cat("foot_year:", foot_year, "\n")
#     cat("footprint file:", foot_i, "\n")

#     ## 3) Parse receptor timestamp (first 12 chars: YYYYMMDDHHMM)
#     foot_timestr_str <- substr(base_name, 1, 12)
#     foot_timestr <- as.POSIXct(foot_timestr_str,
#                                format = "%Y%m%d%H%M",
#                                tz = "UTC")

#     if (is.na(foot_timestr)) {
#       warning("Skipping i = ", i, ": could not parse foot_timestr from ", foot_timestr_str)
#       next
#     }

#     ## 4) Build sequence of footprint hours
#     foot_timestrs <- seq(
#       from = foot_timestr - nhrs_i * 60 * 60,
#       to   = foot_timestr - 1,
#       by   = "hour"
#     )

#     if (length(foot_timestrs) == 0 || any(!is.finite(as.numeric(foot_timestrs)))) {
#       warning("Skipping i = ", i, ": invalid foot_timestrs sequence.")
#       next
#     }

#     ## 5) Check if footprint exists
#     foot_exist <- file.exists(foot_i)
#     if (!foot_exist) {
#       warning("Skipping i = ", i, ": footprint file does not exist: ", foot_i)
#       next
#     }

#     ## 6) Load GDP data
#     base_dir <- "/uufs/chpc.utah.edu/common/home/lin-group20/CarlXav/Data/GDP"
#     gdp_file <- file.path(base_dir, "rast_adm1_gdp_perCapita_1990_2022.tif")
#     gdp      <- brick(gdp_file)

#     years2022 <- as.numeric(gsub("\\D", "", names(gdp)))
#     years2022 <- years2022[!is.na(years2022)]

#     # choose GDP year: use 2022 if foot_year > 2022, else matching year
#     use_year <- if (foot_year > 2022) 2022 else foot_year
#     idx <- which(years2022 == use_year)

#     if (length(idx) == 0) {
#       warning("Skipping i = ", i, ": no GDP layer for year ", use_year)
#       next
#     }

#     cat("Using GDP year index:", idx, " (year ", years2022[idx], ")\n")

#     gdp <- gdp[[idx]]

#     ## 7) Read footprint (may be multi-layer)
#     footprint <- try(brick(foot_i), silent = TRUE)

#     if (inherits(footprint, "try-error") || is.null(dim(footprint))) {
#       warning("Skipping i = ", i, ": could not read footprint brick.")
#       next
#     }

#     foot_hours <- names(footprint)
#     foot_hours <- gsub("^X", "", foot_hours)
#     foot_hours <- substr(foot_hours, 1, 16)
#     foot_hours <- as.POSIXct(foot_hours,
#                              format = "%Y.%m.%d.%H.%M",
#                              tz = "UTC")

#     # keep only hours that match our expected sequence
#     new_flag <- foot_hours %in% foot_timestrs

#     if (!any(new_flag)) {
#       warning("Skipping i = ", i, ": no matching foot_hours in foot_timestrs.")
#       next
#     }

#     footprint <- subset(footprint, which(new_flag))

#     ## 8) Collapse layers, normalize, convolve with GDP
#     footprint_sum <- calc(footprint, sum)
#     footprint_egdp <- footprint_sum

#     footprint_norm <- footprint_sum / cellStats(footprint_sum, sum)

#     # spatial alignment
#     x1 <- extent(footprint_norm)[1]
#     x2 <- extent(footprint_norm)[2]

#     gdp_crop <- crop(gdp, extent(x1, x2,
#                                  extent(footprint_norm)[3],
#                                  extent(footprint_norm)[4]))

#     blank <- raster(
#       nrow = dim(footprint_norm)[1],
#       ncol = dim(footprint_norm)[2],
#       xmn = extent(footprint_norm)[1],
#       xmx = extent(footprint_norm)[2],
#       ymn = extent(footprint_norm)[3],
#       ymx = extent(footprint_norm)[4]
#     )

#     gdp_res <- resample(gdp_crop, blank, method = "ngb")

#     fp_mat    <- as.matrix(footprint_norm)
#     fp_egdp_m <- as.matrix(footprint_egdp)
#     gdp_mat   <- as.matrix(gdp_res)

#     conv <- sum(fp_mat * gdp_mat, na.rm = TRUE)
#     egdp <- sum(fp_egdp_m * gdp_mat, na.rm = TRUE)

#     cat("Effective GDP:", conv, "\n")
#     cat("Effective EGDP:", egdp, "\n")

#     ## 9) Output filenames
#     if (isTRUE(shp_file)) {
#       fname      <- "/gdp_shp_v2022.txt"
#       fname_egdp <- "/GDP_conv_shp_v2022.txt"
#     } else {
#       fname      <- "/gdp_v2022.txt"
#       fname_egdp <- "/GDP_conv_v2022.txt"
#     }

#     write.table(conv, file = paste0(foot_byid_i, fname),
#                 quote = FALSE, row.names = FALSE, col.names = FALSE)
#     write.table(egdp, file = paste0(foot_byid_i, fname_egdp),
#                 quote = FALSE, row.names = FALSE, col.names = FALSE)

#     gc()
#   } # end for
# } # end function


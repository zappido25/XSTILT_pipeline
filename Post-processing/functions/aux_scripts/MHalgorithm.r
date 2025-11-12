# Function to perform adaptive random walk MCMC estimation for bin-level modeled parameters
# and compute posterior enhancement estimates and Epc based on sampled parameters
library(fmcmc)

initial_bg_xco2_mean_guess=function(data, sect){
        
        data$category<-0
        
        quant<-runif(1,0,0.2)

        data$category[data$city_ppm > quantile(data$city_ppm[data$city_ppm > 0], quant, na.rm=T)]<-"city"



        # expand to city enhanced latitudes
        data$category[data$lat_bin > min(data$lat_bin[data$category == "city"]) &
                        data$lat_bin < max(data$lat_bin[data$category == "city"])]<-"city"

        # iterate until at least 5 receptors are part of the background
        iterate_val<-sum(data$recp_num, na.rm=T)*0.2
        if(iterate_val < 10){iterate_val<-10} # minimum number of background receptors
        iterate_val<-round(iterate_val, digits =0)


        vect<-data$lat_bin[data$category != "city"]
        scores_odiac<-data$odiac_ppm[data$category != "city"]/sum(data$odiac_ppm[data$category != "city"], na.rm =T)
        scores_edgar<-data$edgar_ppm[data$category != "city"]/sum(data$edgar_ppm[data$category != "city"], na.rm =T)
        scores_odiac_tot<-data$odiac_total[data$category != "city"]/sum(data$odiac_total[data$category != "city"], na.rm =T)
        scores_pps<-data$mean_pps[data$category != "city"]/sum(data$mean_pps[data$category != "city"], na.rm =T)

        #XCO2,bio−adj=XCO2−ΔCO2,bio
        temp_enh<-data$mean_XCO2 - data$bio_ppm
   
       # get background scores
        scores_obs<-temp_enh[data$category != "city"]/sum(temp_enh[data$category != "city"], na.rm =T)

        # background scores
        scores<-scores_odiac+scores_edgar+scores_odiac_tot+scores_pps+scores_obs
        bg_vect<-vect[order(scores)]
        bg_recp_count<-c()
       
        # cumulative sum of the number of backgroundreceptors
        #STEP 3
        for(i in 1:length(bg_vect)){
            if(i == 1){
                bg_recp_count<-c(bg_recp_count, data$recp_num[data$lat_bin == bg_vect[i]])
            }
            if(i > 1){
                bg_recp_count<-c(bg_recp_count, data$recp_num[data$lat_bin == bg_vect[i]] + bg_recp_count[i-1])
            }
        }
      
        #The indices stored in bg_ind correspond to latitude bins that collectively meet the receptor count threshold (iterate_val).
        # These bins are likely used in subsequent steps to classify background regions.
        # index of background bins
        bg_ind<-which(bg_recp_count < iterate_val)
        if(is.na(bg_recp_count[max(bg_ind)+1]) == F){bg_ind<-c(bg_ind, max(bg_ind) +1)}

        bg_bins<-bg_vect[bg_ind]
        data$category[data$lat_bin %in% bg_bins]<-"bg"


        data$mean_CO2_bg=mean(data$mean_XCO2[data$category == "bg"], na.rm=T)
        return(data)
}

estimate_enhancement_params <- function(data, nsteps = 100000, burnin = 10000,
                                        prior_means = list(city_ppm = 0.1, odiac_ppm = 0.1, odiac_total = 0.1,
                   edgar_ppm = 0.1, mean_pps = 100, bio_ppm = NULL, mean_XCO2 = NULL),
                                        prior_sds = list(city_ppm = 0.1, odiac_ppm = 0.1, odiac_total = 0.1,
                   edgar_ppm = 0.1, mean_pps = 50, bio_ppm = NULL, mean_XCO2 = NULL)) {

  city_data <-data#subset(data, category == "city" & !is.na(mean_XCO2) & !is.na(bio_ppm) &
                   #     !is.na(mean_XCO2.uncert) & !is.na(bio_uncert))
  N <- nrow(city_data)

  # Initial values: concatenate parameter vectors for all bins
  theta_start <- rep(0.1, 7 * N)  # city_ppm, odiac_ppm, odiac_total, edgar_ppm, mean_pps, bio_ppm, mean_XCO2  # city_ppm, odiac_ppm, odiac_total, edgar_ppm, mean_pps, bio_ppm, mean_XCO2, mean_pps_err  # city_ppm, odiac_ppm, odiac_total, edgar_ppm, mean_pps

  log_post_joint <- function(theta) {
    if (any(!is.finite(theta)) || any(is.na(theta))) return(-Inf)
    tryCatch({
        

    city_ppm     <- theta[1:N]
    odiac_ppm    <- theta[(N + 1):(2 * N)]
    odiac_total  <- theta[(2 * N + 1):(3 * N)]
    edgar_ppm    <- theta[(3 * N + 1):(4 * N)]
    mean_pps     <- theta[(4 * N + 1):(5 * N)]
    bio_ppm      <- theta[(5 * N + 1):(6 * N)]
    mean_XCO2    <- theta[(6 * N + 1):(7 * N)]
    
    
   
    bg_ppm           <- city_data$mean_CO2_bg
    sigma_odiac      <- city_data$odiac_sd
    sigma_odiac_tot  <- city_data$odiac_tot_sd
    sigma_edgar      <- city_data$sd_edgar
    sigma_pps        <- sqrt(city_data$sd_pps^2 + city_data$mean_err_pps^2) # get the error from the pps and pps_err_from horr winds
    sigma_bio        <- city_data$bio_uncert
    sigma_xco2       <- city_data$mean_XCO2.uncert

    sigma_total <- sqrt(sigma_xco2^2 + sigma_bio^2 + sigma_odiac^2 +
                        sigma_odiac_tot^2 + sigma_edgar^2 + sigma_pps^2 + 1e-6)

    modeled <- bg_ppm + city_ppm + bio_ppm
    log_lik <- -0.5 * sum((mean_XCO2 - modeled)^2 / sigma_total^2)

    # Normal priors
    log_prior <- sum(dnorm(city_ppm,   mean = prior_means$city_ppm,   sd = prior_sds$city_ppm,   log = TRUE)) +
                 sum(dnorm(odiac_ppm,  mean = prior_means$odiac_ppm,  sd = prior_sds$odiac_ppm,  log = TRUE)) +
                 sum(dnorm(odiac_total,mean = prior_means$odiac_total,sd = prior_sds$odiac_total,log = TRUE)) +
                 sum(dnorm(edgar_ppm,  mean = prior_means$edgar_ppm,  sd = prior_sds$edgar_ppm,  log = TRUE)) +
                 sum(dnorm(mean_pps,   mean = prior_means$mean_pps,   sd = prior_sds$mean_pps,   log = TRUE)) +
                 sum(dnorm(bio_ppm,    mean = city_data$bio_ppm,      sd = city_data$bio_uncert, log = TRUE)) +
                 sum(dnorm(mean_XCO2,  mean = city_data$mean_XCO2,    sd = city_data$mean_XCO2.uncert, log = TRUE)) 

    return(log_lik + log_prior)
#   log_post <- log_lik + log_prior
# if (!is.finite(log_post)) return(-Inf)
# return(log_post)
  }, error = function(e) {
    cat("Numerical error at step with theta:\n")
    print(theta)
    cat("With theta:\n")
    print(theta)
    return(-Inf)
  })
}
  
test_theta <- rep(0, 7 * N)
log_post_joint(test_theta)

print("next")
#   set.seed(123)
#   chain <- MCMC(
#     fun = log_post_joint,
#     initial = theta_start,
#     nsteps = nsteps,
#     kernel = kernel_adapt(),
#     progress = TRUE
#   )

  samples <- chain[(burnin + 1):nrow(chain), ]

  # Posterior means for each parameter
  param_means <- apply(samples, 2, mean)

  city_data$city_ppm   <- param_means[1:N]
  city_data$odiac_ppm  <- param_means[(N + 1):(2 * N)]
  city_data$odiac_total<- param_means[(2 * N + 1):(3 * N)]
  city_data$edgar_ppm  <- param_means[(3 * N + 1):(4 * N)]
  city_data$mean_pps   <- param_means[(4 * N + 1):(5 * N)]

    print(paste0("City ppm: ", city_data$city_ppm))
    stop("Debugging: Check city_ppm values")
  # Compute scores (used for background bin selection)
  scores_odiac     <- city_data$odiac_ppm[city_data$category != "city"] / sum(city_data$odiac_ppm[city_data$category != "city"], na.rm = TRUE)
  scores_edgar     <- city_data$edgar_ppm[city_data$category != "city"] / sum(city_data$edgar_ppm[city_data$category != "city"], na.rm = TRUE)
  scores_odiac_tot <- city_data$odiac_total[city_data$category != "city"] / sum(city_data$odiac_total[city_data$category != "city"], na.rm = TRUE)
  scores_pps       <- city_data$mean_pps[city_data$category != "city"] / sum(city_data$mean_pps[city_data$category != "city"], na.rm = TRUE)
  temp_enh         <- city_data$mean_XCO2 - city_data$bio_ppm
  scores_obs       <- temp_enh / sum(temp_enh, na.rm = TRUE)
  scores_obs       <- temp_enh / sum(temp_enh, na.rm = TRUE)

  city_data$scores <- scores_odiac + scores_edgar + scores_odiac_tot + scores_pps + scores_obs

  # Background bin identification: lowest scores
  vect<-city_data$lat_bin[city_data$category != "city"]
  sorted_bins <- order(city_data$scores)
  threshold_receptors <- max(10, round(0.2 * sum(city_data$recp_num, na.rm = TRUE)))
  bg_bins <- NULL
  acc_recp <- 0
  for (i in sorted_bins) {
    acc_recp <- acc_recp + city_data$recp_num[i]
    bg_bins <- c(bg_bins, i)
    if (acc_recp >= threshold_receptors) break
  }

  city_data$category[] <- "city"
  city_data$category[bg_bins] <- "bg"

  # Enhancement relative to background
  enh_raw <- city_data$mean_XCO2 - city_data$bio_ppm
  bg_mean <- mean(enh_raw[city_data$category == "bg"], na.rm = TRUE)
  city_data$enhancement <- enh_raw - bg_mean

  # Compute Epc per sample and return posterior mean and SD
  epc_all <- matrix(NA, nrow = nrow(samples), ncol = N)
  for (i in 1:nrow(samples)) {
    city_ppm_i <- samples[i, 1:N]
    mean_pps_i <- samples[i, (4 * N + 1):(5 * N)]
    bio        <- city_data$bio_ppm
    enh_i      <- city_ppm_i
    epc_all[i, ] <- enh_i / mean_pps_i
  }

  city_data$epc_mean <- apply(epc_all, 2, mean)
  city_data$epc_sd   <- apply(epc_all, 2, sd)

  # Trace plot for first few parameters (optional: customize which bins or parameters to show)
par(mfrow = c(3, 2))
for (j in 1:min(6, ncol(samples))) {
  plot(samples[, j], type = 'l', main = paste('Trace plot for param', j),
       xlab = 'Iteration', ylab = 'Value')
}

# Autocorrelation plots
par(mfrow = c(3, 2))
for (j in 1:min(6, ncol(samples))) {
  acf(samples[, j], main = paste('ACF for param', j))
}

# Gelman-Rubin diagnostic (requires converting samples to mcmc.list)
if (requireNamespace("coda", quietly = TRUE)) {
  library(coda)
  mcmc_obj <- mcmc(samples)
  print(gelman.diag(mcmc.list(mcmc_obj, mcmc_obj)))  # Duplicate chain for placeholder
}

# Post-processing loop over posterior samples with classification logic
valid_enh <- c()
valid_epc <- c()

for (i in 1:nrow(samples)) {
  city_ppm_i   <- samples[i, 1:N]
  odiac_ppm_i  <- samples[i, (N + 1):(2 * N)]
  odiac_total_i<- samples[i, (2 * N + 1):(3 * N)]
  edgar_ppm_i  <- samples[i, (3 * N + 1):(4 * N)]
  mean_pps_i   <- samples[i, (4 * N + 1):(5 * N)]

  # Compute scores for classification
  scores_odiac     <- odiac_ppm_i / sum(odiac_ppm_i, na.rm = TRUE)
  scores_edgar     <- edgar_ppm_i / sum(edgar_ppm_i, na.rm = TRUE)
  scores_odiac_tot <- odiac_total_i / sum(odiac_total_i, na.rm = TRUE)
  scores_pps       <- mean_pps_i / sum(mean_pps_i, na.rm = TRUE)
  enh_raw_i        <- city_ppm_i
  scores_obs       <- enh_raw_i / sum(enh_raw_i, na.rm = TRUE)

  scores_i <- scores_odiac + scores_edgar + scores_odiac_tot + scores_pps + scores_obs

  # Classify background bins based on scores
  sorted_idx <- order(scores_i)
  threshold_receptors <- max(10, round(0.2 * sum(city_data$recp_num, na.rm = TRUE)))
  acc_recp <- 0
  bg_idx <- c()
  for (k in sorted_idx) {
    acc_recp <- acc_recp + city_data$recp_num[k]
    bg_idx <- c(bg_idx, k)
    if (acc_recp >= threshold_receptors) break
  }

  # Compute background mean and enhancement
  bg_mean_i <- mean(city_ppm_i[bg_idx], na.rm = TRUE)
  enhancement_i <- city_ppm_i - bg_mean_i

  # t-test
  test_result <- try(t.test(enhancement_i[-bg_idx], enhancement_i[bg_idx]), silent = TRUE)
  if (!inherits(test_result, "try-error")) {
    if (test_result$p.value <= 0.1 && mean(enhancement_i[-bg_idx], na.rm = TRUE) > mean(enhancement_i[bg_idx], na.rm = TRUE)) {
      # Compute Epc only if enhancement is valid
      epc_i <- enhancement_i / mean_pps_i
      valid_enh <- c(valid_enh, mean(enhancement_i[-bg_idx], na.rm = TRUE))
      valid_epc <- c(valid_epc, mean(epc_i[-bg_idx], na.rm = TRUE))
    }
  }
}

city_data$enh_posterior_mean <- mean(valid_enh, na.rm = TRUE)
city_data$enh_posterior_sd   <- sd(valid_enh, na.rm = TRUE)
city_data$epc_posterior_mean <- mean(valid_epc, na.rm = TRUE)
city_data$epc_posterior_sd   <- sd(valid_epc, na.rm = TRUE)

# Log accepted vs. rejected sample counts
accepted_samples <- length(valid_enh)
total_samples <- nrow(samples)
acceptance_rate <- accepted_samples / total_samples
cat("Accepted samples:", accepted_samples, "/", total_samples, " (", round(100 * acceptance_rate, 2), "%)
")

# Log accepted vs. rejected sample counts
accepted_samples <- length(valid_enh)
total_samples <- nrow(samples)
acceptance_rate <- accepted_samples / total_samples
cat("Accepted samples:", accepted_samples, "/", total_samples, " (", round(100 * acceptance_rate, 2), "%)
")

# === Save validated enhancement and Epc summary to CSV ===
output_summary <- data.frame(
  city = unique(city_data$city)[1],
  accepted_samples = accepted_samples,
  total_samples = total_samples,
  acceptance_rate = acceptance_rate,
  enh_posterior_mean = mean(valid_enh, na.rm = TRUE),
  enh_posterior_sd = sd(valid_enh, na.rm = TRUE),
  epc_posterior_mean = mean(valid_epc, na.rm = TRUE),
  epc_posterior_sd = sd(valid_epc, na.rm = TRUE)
)

# Save to file (customize path as needed)
write.csv(output_summary,
          file = paste0("posterior_summary_", unique(city_data$city)[1], ".csv"),
          row.names = FALSE)

# === Save full bin-level posterior summaries ===
bin_summary <- data.frame(
  lat_bin = city_data$lat_bin,
  lat_mid = city_data$lat_mid,
  epc_mean = city_data$epc_mean,
  epc_sd = city_data$epc_sd,
  enhancement = city_data$enhancement
)

write.csv(bin_summary,
          file = paste0("posterior_bin_summary_", unique(city_data$city)[1], ".csv"),
          row.names = FALSE)

return(list(
    city_data = city_data,
    samples = samples,
    epc_all = epc_all
  ))
}

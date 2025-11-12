library(ggpubr)
library(broom)
library(purrr)
library(tidyr)
library(dplyr)
# Optional: add parsed time fields to each (still separate)

# Remove outliers beyond ±2 SD of median of Epc for each method based on Kai w work
remove_outliers <- function(df) {
  df <- df %>%
    mutate(Epc = as.numeric(Epc)) %>%
    filter(!is.na(Epc))
  med <- median(df$Epc, na.rm = TRUE)
  s <- sd(df$Epc, na.rm = TRUE)
  df %>% filter(Epc >= (med - 2 * s) & Epc <= (med + 2 * s))
  
}


parse_times <- function(df) {
  if (!nrow(df)) return(df)
  df %>%
    dplyr::mutate(
      date_time = lubridate::parse_date_time(
        timestr,
        orders = c("%Y%m%d%H","%Y%m%d","%Y-%m-%d %H:%M:%S","%Y-%m-%d")
      ),
      year  = lubridate::year(date_time),
      month = lubridate::month(date_time),
      season = factor(dplyr::case_when(
        month %in% c(12,1,2)  ~ "DJF",
        month %in% c(3,4,5)   ~ "MAM",
        month %in% c(6,7,8)   ~ "JJA",
        month %in% c(9,10,11) ~ "SON"
      ), levels = c("DJF","MAM","JJA","SON"))
    )
}

# helper functions to get seasonal df: seasonal means + SD + N
seasonal_epc <- function(df, label) {
  df %>%
    group_by(city, season) %>%
    summarise(
      epc_mean = mean(as.numeric(Epc), na.rm = TRUE),
      epc_sd   = sd(as.numeric(Epc),   na.rm = TRUE),
      n        = sum(!is.na(Epc)),
      .groups = "drop"
    ) %>%
    mutate(method = label)
}


# ---------- 1) Build seasonal pre/post covid summaries for ONE method ----------
epc_prepost_summ <- function(df) {
    df %>%
      mutate(period = ifelse(year < 2020, "preCOVID", "postCOVID")) %>%
      group_by(city, period, season) %>%
      summarise(
        epc_mean = mean(as.numeric(Epc), na.rm = TRUE),
        epc_sd   = sd(as.numeric(Epc),   na.rm = TRUE),
        n        = sum(!is.na(Epc)),
        .groups  = "drop"
      ) %>%
      mutate(se = epc_sd / sqrt(pmax(n, 1)),
            lo = epc_mean - 1.96*se,
            hi = epc_mean + 1.96*se)
}

# ---------- 2) Plot helper for ONE method ----------
plot_prepost <- function(epc_prepost_df, method_label = "method") {
  ggplot(epc_prepost_df, aes(season, epc_mean, fill = period)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = lo, ymax = hi),
                  position = position_dodge(width = 0.9), width = 0.2) +
    facet_wrap(~city, scales = "free_y") +
    labs(
      x = "Season (DJF/MAM/JJA/SON)",
      y = "tCO₂ / person / year",
      title = paste("Pre vs Post COVID seasonal per-capita emissions (", method_label, ")", sep = ""),
      fill = "Period"
    ) +
    theme_minimal()
}


# plotting the epc time series
plot_epc_timeseries <- function(epc_df) {
  ggplot(epc_df, aes(x = date_time, y = as.numeric(Epc), color = method)) +
    geom_line(alpha = 0.8) +
    geom_point(alpha = 0.3, size = 0.7) +
    geom_smooth(
      aes(group = method, fill = method),
      method = "lm", linetype = "dashed", size = 1, se = TRUE, alpha = 0.2
    ) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"), color = method),
      method = "lm",
      label.x.npc = 0,
      label.y.npc = 0.95,
      show.legend = TRUE
    ) +
    labs(
      x = "Date",
      y = "tCO₂ / person / year",
      title = "Temporal change in per-capita emissions (SHP vs CLUSTER)",
      color = "Method"
    ) +
    theme_minimal()
}


plot_seasonality = function (epc_seasonal){
  ggplot(epc_seasonal, aes(season, epc_mean, fill = method)) +
    geom_col(position = position_dodge(width = 0.9)) +
    geom_errorbar(aes(ymin = epc_mean - epc_sd, ymax = epc_mean + epc_sd),
                  position = position_dodge(width = 0.9), width = 0.2) +
    geom_text(
      aes(label = paste0("n=", n)),
      position = position_dodge(width = 0.9),
      vjust = -0.7, size = 3
    ) +
    labs(
      x = "Season",
      y = "tCO₂/person/year",
      title = "Emissions per-capita seasonality by city",
      fill = "Method"
    ) +
    theme_minimal()
}



#	estimate = slope (Δ Epc per year).
#	p.value < 0.05 → significant trend.
check_trend <- function(df, method_label) {
  df %>%
    group_by(city) %>%
    summarise(
      model = list(lm(Epc ~ date_time, data = cur_data())),
      .groups = "drop"
    ) %>%
    mutate(method = method_label,
           tidy = map(model, broom::tidy)) %>%
    dplyr::select(city, method, tidy) %>%
    unnest(tidy) %>%
    filter(term == "date_time") %>%
    dplyr::select(city, method, estimate, std.error, statistic, p.value) %>%
    rename(slope = estimate) %>%
    mutate(label = paste0("y = intercept + ", formatC(slope, format = "e", digits = 2), "·year"))
}

# non parametric Mann-Kendall test 
#	tau = Kendall’s rank correlation (positive = upward trend).
#	p < 0.05 → significant monotonic trend#

library(Kendall)

mk_test <- function(df, method_label) {
  df %>%
    group_by(city) %>%
    summarise(
      mk = list(Kendall(year, Epc)),
      .groups = "drop"
    ) %>%
    mutate(method = method_label,
           tau = map_dbl(mk, ~ .x$tau[1]),
           p   = map_dbl(mk, ~ .x$sl[1])) %>%
    dplyr::select(city, method, tau, p)
}


plot_monthly_profiles =function(monthly_summary) {
  p1=ggplot(monthly_summary, aes(x = as.numeric(month), y = median_epc, color = method)) +
      geom_line(size = 1) +
      geom_point() +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      theme_minimal() +
      labs(
        title = "Seasonality of Per Capita CO₂ Emissions (Phoenix)",
        x = "Month",
        y = "Median Epc (Tg CO₂ /cap/year)"
      )
}

trend_timeseries=function(epc_combined) {
  # plot with regression line
  ggplot( epc_combined, aes(x = year, y = as.numeric(Epc), color = method)) +
  geom_line(alpha = 0.7, size = 0.8) +  # individual time series per method
  # add trend line (black dashed)
  geom_smooth(aes(x = year, y = as.numeric(Epc)), inherit.aes = FALSE,
              method = "lm", color = "black", linetype = "dashed", size = 1) +
  facet_wrap(~city, scales = "free_y") +
  labs(
    x = "Date",
    y = "tCO₂ / person / year",
    title = "Epc time series with linear trend (black dashed)",
    color = "Method"
  ) +
  theme_minimal()
}


## sectoral related functions
####
plot_sectoral_profiles_yearly_timeseries = function(sector_yearly) {
  

  # 1) Compute the per-sector median within each (city, method, sector)
  # Compute the 10th percentile of epc_mean within each (city, method, year)
  # 1) Compute the 10th percentile of epc_mean within each (city, method, year)
  sector_yearly_pct10 <- sector_yearly %>%
    group_by(city, method, year) %>%
    summarise(pct10_sector = quantile(epc_mean, probs = 0.1, na.rm = TRUE), .groups = "drop")

  # 2) Collapse sectors below pct10 into "Other"
  sector_yearly_collapsed <- sector_yearly %>%
    left_join(sector_yearly_pct10, by = c("city", "method", "year")) %>%
    mutate(sector_collapsed = ifelse(epc_mean >= pct10_sector, sector, "Other")) %>%
    group_by(city, method, year, sector_collapsed) %>%
    summarise(epc_mean = sum(epc_mean, na.rm = TRUE), .groups = "drop")

  # 3) Plot only the above-pct10 segments (with "Other" collapsed), with a dashed black pct10 line per facet
  ggplot(sector_yearly_collapsed,
         aes(x = year, y = epc_mean,
             color = sector_collapsed, linetype = sector_collapsed, shape = sector_collapsed)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    # pct10 cutoff line
    geom_hline(data = sector_yearly_pct10,
               aes(yintercept = pct10_sector),
               linetype = "dashed", color = "black", linewidth = 0.6,
               inherit.aes = FALSE) +
    scale_color_brewer(palette = "Set2") +
    scale_linetype_manual(values = rep(1:6, length.out = 18)) +
    scale_shape_manual(values = rep(0:16, length.out = 18)) +
    facet_grid(method ~ city, scales = "free_y") +
    labs(
      title = "Sectoral per-capita contributions (sectors < pct10 collapsed to Other)",
      x = "Year", y = "tCO₂/person/year"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
}

plot_sectoral_change_yearly=function(sector_yearly) {
  ## get the sectoral trends

  sector_trends <- sector_yearly %>%
    group_by(city, method, sector) %>%
    summarise(model = list(lm(epc_mean ~ year, data = cur_data())),
              .groups = "drop") %>%
    mutate(tidy = map(model, tidy)) %>%
    unnest(tidy) %>%
    filter(term == "year") %>%
    dplyr::select(city, method, sector, slope = estimate, p.value)
  
  # get the relative change values
  sector_change <- sector_yearly %>%
    group_by(city, method, sector) %>%
    summarise(
      delta = last(epc_mean, order_by = year) -
              first(epc_mean, order_by = year),
      .groups = "drop"
    ) %>%
    left_join(sector_trends, by = c("city","method","sector")) %>%
    mutate(sig = p.value < 0.05)   # flag significant slopes


    ggplot(sector_change,
       aes(x = reorder(sector, delta), y = delta, fill = method)) +
    geom_col(position = "dodge", color = "black") +
    # add significance markers above bars
    geom_text(aes(label = ifelse(p.value < 0.05, "*", "")),
              position = position_dodge(width = 0.9),
              vjust = -0.5, size = 5, color = "black") +
    coord_flip() +
    facet_wrap(~city, scales = "free_y") +
    labs(
      title = "Net change in sectoral per-capita CO₂ (last − first year)",
      x = "Sector", y = "Δ tCO₂/person/year",
      fill = "Method"
    ) +
    theme_minimal()

}


plot_sectoral_stacked_area=function(sector_yearly) {

  # select values above the lowest 10%percentile Epc
  sector_yearly_pct10 <- sector_yearly %>%
    group_by(city, method, year) %>%
    summarise(pct10_sector = quantile(epc_mean, probs = 0.1, na.rm = TRUE), .groups = "drop")

  # 2) Join and keep only values above the pct10
    sector_yearly_filtered <- sector_yearly %>%
    left_join(sector_yearly_pct10, by = c("city","method","year")) %>%

    filter(epc_mean >= pct10_sector)   # keep only above-median sector contributions

  ggplot(sector_yearly, aes(x = year, y = epc_mean, fill = sector)) +
    geom_area(position = "stack",alpha = 0.8) +
    facet_wrap(~city + method, scales = "free_y") +
    labs(
      title = "Sectoral Contributions to Per Capita CO₂ Emissions",
      x = "Year",
      y = "Mean tCO₂ / person / year",
      fill = "Sector"
    ) +
    theme_minimal()
}

plot_normalized_contributions = function (sector_yearly) {

  # normalize the Epc to show the relative fractions rather than the absolute contributions, showing how each sector contributions
  # change over time

  sector_relative <- sector_yearly %>%
    group_by(city, method, year) %>%
    mutate(total_epc = sum(epc_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA))

    ggplot(sector_relative,
       aes(x = year, y = rel_contrib, fill = sector)) +
    geom_area(position = "stack", alpha = 0.9) +
    facet_wrap(~ city + method, scales = "free_y") +
    labs(
      title = "Relative sectoral contributions to per-capita emissions",
      x = "Year", y = "Relative contribution (fraction of total Epc)",
      fill = "Sector"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

library(dplyr)
library(forcats)
library(ggplot2)

# --- Option 1: single plot after filtering by 10th percentile of 10-year mean ---
plot_normalized_contributions_filtered <- function(
  sector_yearly
){
  q_cutoff = 0.10   # 10th percentile
  # 1) normalize to relative contributions
  sector_relative <- sector_yearly %>%
    group_by(city, method, year) %>%
    mutate(total_epc = sum(epc_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA_real_))

  # 2) compute mean Epc per sector over time within each city×method
  mean_epc <- sector_relative %>%
    group_by(city, method, sector) %>%
    summarize(mean_epc_10yr = mean(epc_mean, na.rm = TRUE), .groups = "drop")

  # 3) threshold = 10th percentile (per city×method)
  keep_tbl <- mean_epc %>%
    group_by(city, method) %>%
    mutate(thresh = quantile(mean_epc_10yr, q_cutoff, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(keep = mean_epc_10yr >= thresh) %>%
    dplyr::select(city, method, sector, keep)

  dat_keep <- sector_relative %>%
    inner_join(keep_tbl, by = c("city", "method", "sector")) %>%
    filter(keep, !is.na(rel_contrib)) %>%
    mutate(sector = fct_drop(sector))  # drop unplotted levels from legend

  ggplot(dat_keep, aes(x = year, y = rel_contrib, fill = sector)) +
    geom_area(position = "stack", alpha = 0.9) +
    facet_wrap(~ city + method, scales = "free_y") +
    labs(
      title = "Relative sectoral contributions (filtered by 10th-percentile of 10-year mean Epc)",
      x = "Year", y = "Relative contribution (fraction of total Epc)", fill = "Sector"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# --- Option 2: split into 2 plots: top K sectors vs the rest (others bucket) ---
plot_normalized_contributions_split <- function(
  sector_yearly
){
  top_k = 5  # top K sectors to show individually, rest collapsed into "Other"
  # normalize
  sector_relative <- sector_yearly %>%
    group_by(city, method, year) %>%
    mutate(total_epc = sum(epc_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA_real_))

  # rank sectors by average contribution (using mean of epc_mean over all years)
  sector_rank <- sector_relative %>%
    group_by(city, method, sector) %>%
    summarize(mean_epc_10yr = mean(epc_mean, na.rm = TRUE), .groups = "drop") %>%
    group_by(city, method) %>%
    arrange(desc(mean_epc_10yr), .by_group = TRUE) %>%
    mutate(rank = row_number()) %>%
    ungroup()

  # tag top vs other
  tagged <- sector_relative %>%
    left_join(sector_rank, by = c("city","method","sector")) %>%
    mutate(group = ifelse(rank <= top_k, "Top", "Other"),
           sector = fct_drop(sector))

  # TOP PLOT
  top_dat <- tagged %>% filter(group == "Top", !is.na(rel_contrib))
  p_top <- ggplot(top_dat, aes(year, rel_contrib, fill = sector)) +
    geom_area(position = "stack", alpha = 0.9) +
    facet_wrap(~ city + method, scales = "free_y") +
    labs(
      title = sprintf("Top %d sectors — relative contributions", top_k),
      x = "Year", y = "Relative contribution", fill = "Sector"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # OTHER PLOT (collapse others or show individually — here we keep individually)
  other_dat <- tagged %>% filter(group == "Other", !is.na(rel_contrib))
  p_other <- ggplot(other_dat, aes(year, rel_contrib, fill = sector)) +
    geom_area(position = "stack", alpha = 0.9) +
    facet_wrap(~ city + method, scales = "free_y") +
    labs(
      title = "Other sectors — relative contributions",
      x = "Year", y = "Relative contribution", fill = "Sector"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")

  # If you prefer a single “Others” bucket instead:
  # other_bucket <- tagged %>%
  #   filter(group == "Other") %>%
  #   group_by(city, method, year) %>%
  #   summarize(rel_contrib = sum(rel_contrib, na.rm = TRUE), .groups = "drop") %>%
  #   mutate(sector = factor("Others"))
  # and then bind_rows with the top_dat before plotting.

  list(top = p_top, other = p_other)
}

plot_normalized_contributions_100_percent_stacked_bar = function (sector_yearly) {

  # normalize the Epc to show the relative fractions rather than the absolute contributions, showing how each sector contributions
  # change over time
  #. bar plots

  sector_relative <- sector_yearly %>%
  group_by(city, method, year) %>%
  mutate(total_epc = sum(epc_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA),
         pct_label   = paste0(round(rel_contrib * 100, 1), "%"))

ggplot(sector_relative,
       aes(x = factor(year), y = rel_contrib, fill = sector)) +
  geom_bar(stat = "identity", position = "fill", color = "black") +
  # only label if > 10%
  geom_text(aes(label = ifelse(rel_contrib > 0.10, pct_label, "")),
            position = position_fill(vjust = 0.5),
            size = 2.8, color = "black") +
  facet_wrap(~ city + method, scales = "free_y") +
  labs(
    title = "Relative sectoral contributions (labels only for >10%)",
    x = "Year", y = "Share of total Epc",
    fill = "Sector"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "bottom")

}


plot_sectors_greater_pct10 = function (sector_yearly) {
  

# INPUT: sector_yearly with columns: city, method, year, sector, epc_mean

# 1) Compute relative contributions (fractions that sum to 1 per city-method-year)
  sector_relative <- sector_yearly %>%
    group_by(city, method, year) %>%
    mutate(total_epc = sum(epc_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA_real_))

  # 2) Collapse sectors below threshold into "Other" (threshold = 10%)
  threshold <- 0.10

  sector_rel_collapsed <- sector_yearly %>%
  group_by(city, method, year) %>%
  mutate(total_epc = sum(epc_mean, na.rm = TRUE),
         rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA_real_),
         sector_collapsed = ifelse(rel_contrib >= threshold, sector, "Other")) %>%
  group_by(city, method, year, sector_collapsed) %>%
  summarise(rel_contrib = sum(rel_contrib, na.rm = TRUE), .groups = "drop") %>%
  mutate(pct_label = paste0(round(rel_contrib * 100, 1), "%"))

  # (Optional) Order legend: biggest average first, "Other" last
  order_levels <- sector_rel_collapsed %>%
    group_by(sector_collapsed) %>%
    summarise(avg_share = mean(rel_contrib, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(if_else(sector_collapsed == "Other", -Inf, avg_share))) %>%
    pull(sector_collapsed)

  sector_rel_collapsed <- sector_rel_collapsed %>%
    mutate(sector_collapsed = factor(sector_collapsed, levels = order_levels))

  # 3) Plot: 100% stacked bars with % labels inside each segment
    ggplot(sector_rel_collapsed,
        aes(x = factor(year), y = rel_contrib, fill = sector_collapsed)) +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    geom_text(aes(label = ifelse(rel_contrib >= threshold, pct_label, "")),
              position = position_stack(vjust = 0.5),
              size = 2.8, color = "black") +
    facet_wrap(~ city + method, scales = "free_y") +
    labs(
      title = "Relative sectoral contributions to per-capita emissions (≤10% collapsed to Other)",
      x = "Year", y = "Share of total Epc",
      fill = "Sector"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

seasonality_sectors=function(df_pc) {
  sector_long <- df_pc %>%
  dplyr::select(city, method, season, all_of(pc_cols)) %>%
  pivot_longer(cols = all_of(pc_cols), names_to = "sector", values_to = "epc_value") %>%
  mutate(
    sector = sub("^pc_", "", sector),
    season = factor(season, levels = c("DJF","MAM","JJA","SON"))
  )
  # Aggregate to seasonal means per sector
  sector_season_abs <- sector_long %>%
    group_by(city, method, season, sector) %>%
    summarise(epc_mean = mean(epc_value, na.rm = TRUE), .groups = "drop")

  # Convert to relative shares within each (city, method, season)
  sector_season_rel <- sector_season_abs %>%
    group_by(city, method, season) %>%
    mutate(total = sum(epc_mean, na.rm = TRUE),
          rel   = ifelse(total > 0, epc_mean / total, NA_real_)) %>%
    ungroup() %>%
    mutate(pct_label = paste0(round(rel * 100, 1), "%"))

  # Optional: collapse small shares (<10%) into "Other" per season
  threshold <- 0.10
  season_rel_collapsed <- sector_season_rel %>%
    group_by(city, method, season) %>%
    mutate(sector_c = ifelse(rel >= threshold, sector, "Other")) %>%
    ungroup() %>%
    group_by(city, method, season, sector_c) %>%
    summarise(rel = sum(rel, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct_label = paste0(round(rel * 100, 1), "%"))

  # Nice legend order: biggest average first, "Other" last
  order_levels <- season_rel_collapsed %>%
    group_by(sector_c) %>%
    summarise(avg_share = mean(rel, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(if_else(sector_c == "Other", -Inf, avg_share))) %>%
    pull(sector_c)

  season_rel_collapsed <- season_rel_collapsed %>%
    mutate(sector_c = factor(sector_c, levels = order_levels))
  
  print(season_rel_collapsed %>% dplyr::filter(sector_c == "IND"))
  print(sector_season_rel %>% filter(sector == "IND", season == "JJA") )
  
  # Plot: 100% stacked bars by season with % labels (labels only for ≥10% segments)
   ggplot(season_rel_collapsed,
        aes(x = season, y = rel, fill = sector_c)) +
    geom_col(position = "fill", color = "black") +
    geom_text(aes(label = ifelse(rel >= threshold, pct_label, "")),
              position = position_fill(vjust = 0.5), size = 3) +
    facet_wrap(~ city + method, scales = "free_y") +
    labs(
      title = "Relative sectoral contributions by season (shares sum to 100%)",
      x = "Season", y = "Share of per-capita emissions", fill = "Sector"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_minimal() +
    theme(legend.position = "bottom")

}
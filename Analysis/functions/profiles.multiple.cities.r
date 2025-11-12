library(ggpubr)
library(broom)
library(purrr)
library(tidyr)
library(dplyr)
# Optional: add parsed time fields to each (still separate)


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


plot_season_trends_light_dark_city=function(df) {

 
  epc_season <- df %>%
    mutate(
      date_time = as.Date(date_time),                 # or as.POSIXct if you prefer
      mo   = lubridate::month(date_time),
      season = if_else(mo %in% 3:8, "Light (Mar–Aug)", "Dark (Sep–Feb)"),
      season = factor(season, levels = c("Light (Mar–Aug)", "Dark (Sep–Feb)"))
    )

  ggplot(
  epc_season,
  aes(x = date_time, y = as.numeric(Epc),
      color = season, group = interaction(city, season))
) +
  geom_line(alpha = 0.8, aes(linetype = season)) +
  geom_point(alpha = 0.35, size = 0.8) +
  geom_smooth(
    aes(fill = season), method = "lm", se = TRUE,
    linetype = "dashed", linewidth = 1, alpha = 0.18
  ) +
  facet_wrap(~ city, scales = "free_y") +
  labs(
    x = "Date",
    y = "tCO\u2082 / person / year",
    title = "Temporal change in per-capita emissions by season",
    color = "Season", fill = "Season", linetype = "Season"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))



  
}
plot_ratio_population_step <- function(df) {
  library(dplyr)
  library(ggplot2)

  df <- df %>%
    mutate(
      date_time = as.Date(date_time),
      ratio_pop = as.numeric(ratio_pop)
    ) %>%
    arrange(city, date_time)

  print(df$ratio_pop)
  ggplot(df, aes(x = date_time, y = ratio_pop, color = city, linetype = city)) +
    geom_step(linewidth = 0.9, alpha = 0.9, direction = "hv") +  # step style
    geom_point(size = 1.4, alpha = 0.8) +                         # markers on steps
    facet_wrap(~ city, scales = "free_y") +
    labs(
      x = "Date",
      y = "Population Ratio (world_pop_data / gpw)",
      title = "Population Ratio Over Time ",
      color = "City", linetype = "City"
    ) +
    theme_bw(base_size = 12)  +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 30, hjust = 1)
    )
}

# plot carbon intensity time series
plot_carbon_intensity_city=function(df){

  df =df %>%
    mutate(CI_kgCO2= as.numeric(CI)*1e6) # convert to kgCO2/$GDP

  ggplot(df, aes(x = date_time, y = as.numeric(CI_kgCO2), color = city)) +
      geom_line(alpha = 0.8) +
      geom_point(alpha = 0.3, size = 0.7) +
      geom_smooth(
        aes(group = city, fill = city),
        method = "lm", linetype = "dashed", size = 1, se = TRUE, alpha = 0.2
      ) +
      stat_regline_equation(
        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"), color = city),
        method = "lm",
        label.x.npc = 0,
        label.y.npc = 0.95,
        show.legend = TRUE
      ) +
      labs(
        x = "Date",
        y = "kgCO₂ / $GDP",
        title = "Temporal change in carbon intensity for different cities",
        color = "City"
      ) +
      theme_bw(base_size = 12) 


}
plot_carbon_intensity_box_plot_city=function(df){

  df =df %>%
    mutate(CI_kgCO2= as.numeric(CI)*1e6,
           mean_ci=mean(CI_kgCO2, na.rm = TRUE),
    sd_ci=sd(CI_kgCO2)) # convert to kgCO2/$GDP

  p=ggplot(df, aes(x = factor(year), y = CI_kgCO2 , fill = city)) +
  geom_errorbar(
    aes(ymin = mean_ci - sd_ci, ymax = mean_ci+ sd_ci),
    width = 0.2,
    position = position_dodge(width = 0.9),
    colour = "black"
  ) +
  geom_col(position = "dodge", alpha = 0.9) +
  labs(
    title = "Carbon intensity by year",
    x = "Year",
    y = "Carbon intensity (kg CO₂ / USD)",
    fill = "City"
  ) +
  theme_bw(base_size = 12)  +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

  n_cities <- length(unique(df$city))
    if (n_cities > 2) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
}
plot_carbon_intensity_grouped_yearly_city=function(df){

  df =df %>%
    mutate(CI_kgCO2= as.numeric(CI)*1e6) # convert to kgCO2/$GDP

  df_ci_grouped <- df %>%
  mutate(period = case_when(
    year <= 2019 ~ "2015–2019",
    # year == 2020 ~ "2020",
    year >= 2020 & year <= 2021 ~ "2020–2021",
    year >= 2022 & year <= 2023 ~ "2021–2023"
  )) %>%
  group_by(city, period) %>%
  summarise(mean_ci = mean(CI_kgCO2),
  sd_ci   = sd(CI_kgCO2, na.rm = TRUE),
    n       = n(), .groups = "drop")

 p=ggplot(df_ci_grouped, aes(x = period, y = mean_ci, fill = city)) +
  geom_col(position = "dodge", alpha = 0.9) +
  geom_errorbar(
    aes(ymin = mean_ci - sd_ci, ymax = mean_ci + sd_ci),
    width = 0.2,
    position = position_dodge(width = 0.9),
    colour = "black"
  ) +
  labs(
    title = "Carbon intensity by period",
    x = "Period",
    y = "Carbon intensity (kg CO₂ / USD)",
    fill = "City"
  ) +
   
  theme_bw(base_size = 12) 

  n_cities <- length(unique(df$city))
    if (n_cities > 2) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
}

# plotting the epc time series
plot_epc_timeseries_city <- function(epc_df) {
 p= ggplot(epc_df, aes(x = date_time, y = as.numeric(Epc), color = city)) +
    geom_line(alpha = 0.8) +
    geom_point(alpha = 0.3, size = 0.7) +
    geom_smooth(
      aes(group = city, fill = city),
      method = "lm", linetype = "dashed", size = 1, se = TRUE, alpha = 0.2
    ) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"), color = city),
      method = "lm",
      label.x.npc = 0,
      label.y.npc = 0.95,
      show.legend = TRUE
    ) +
    labs(
      x = "Date",
      y = "tCO₂ / person / year",
      title = "Temporal change in per-capita emissions for different cities",
      color = "City"
    ) +
    theme_bw(base_size = 12) 
     plot_name="../figures/cities_epc_timeseries.png"
     ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
}

plot_epc_timeseries_city_pop <- function(epc_df) {

  library(ggpmisc)   # for stat_regline_equation

  # prepare data: add scaled EPC, then pivot longer
  df_long <- epc_df %>%
    mutate(
      epc = as.numeric(Epc),
      epc_scaled = as.numeric(Epc) / as.numeric(ratio_pop)
    ) %>%
    dplyr::select(city, date_time, epc, epc_scaled) %>%
    pivot_longer(
      cols = c(epc, epc_scaled),
      names_to = "series",
      values_to = "value"
    )

  # relabel series for legend
  df_long$series <- factor(df_long$series,
                           levels = c("epc", "epc_scaled"),
                           labels = c("EPC", "EPC/pop_ratio"))

  ggplot(df_long, aes(x = date_time, y = value, color = series)) +
    geom_line(alpha = 0.8) +
    geom_point(alpha = 0.3, size = 0.7) +
    geom_smooth(
      aes(fill = series),
      method = "lm", linetype = "dashed", size = 1, se = TRUE, alpha = 0.2
    ) +
    stat_regline_equation(
      aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"), color = series),
      method = "lm",
      label.x.npc = 0,
      label.y.npc = 0.95,
      show.legend = FALSE
    ) +
    facet_wrap(~ city, scales = "free_y") +
    labs(
      x = "Date",
      y = "tCO₂ / person / year",
      title = "Temporal change in per-capita emissions and EPC/pop_ratio",
      color = "Series", fill = "Series"
    ) +
    theme_bw(base_size = 12)  +
    theme(legend.position = "bottom")
}

plot_seasonality_city = function (epc_seasonal){
 p= ggplot(epc_seasonal, aes(season, epc_mean, fill = city)) +
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
      title = "Emissions per-capita seasonality",
      fill = "City"
    ) +
    theme_bw(base_size = 12) 
     plot_name="../figures/cities_epc_seasonality.png"
   ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
}




plot_monthly_profiles_city =function(monthly_summary) {
  p1=ggplot(monthly_summary, aes(x = as.numeric(month), y = median_epc, color = city)) +
      geom_line(size = 1) +
      geom_point() +
      scale_x_continuous(breaks = 1:12, labels = month.abb) +
      theme_bw(base_size = 12)  +
      labs(
        title = "Seasonality of Per Capita CO₂ Emissions (Phoenix)",
        x = "Month",
        y = "Median Epc (Tg CO₂ /cap/year)"
      )
}



## sectoral related functions
####
plot_sectoral_profiles_yearly_timeseries_city = function(sector_yearly) {
  

  # 1) Compute the per-sector median within each (city, method, sector)
  # Compute the 10th percentile of epc_mean within each (city, method, year)
  # 1) Compute the 10th percentile of epc_mean within each (city, method, year)
  sector_yearly_pct10 <- sector_yearly %>%
    group_by(city,  year) %>%
    summarise(pct10_sector = quantile(epc_mean, probs = 0.1, na.rm = TRUE), .groups = "drop")

  # 2) Collapse sectors below pct10 into "Other"
  sector_yearly_collapsed <- sector_yearly %>%
    left_join(sector_yearly_pct10, by = c("city",  "year")) %>%
    mutate(sector_collapsed = ifelse(epc_mean >= pct10_sector, sector, "Other")) %>%
    group_by(city, year, sector_collapsed) %>%
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
    scale_linetype_manual(values = rep(1:6, length.out = 17)) +
    scale_shape_manual(values = rep(0:16, length.out = 17)) +
    facet_grid(year ~ city, scales = "free_y") +
    labs(
      title = "Sectoral per-capita contributions (sectors < pct10 collapsed to Other)",
      x = "Year", y = "tCO₂/person/year"
    ) +
    theme_bw(base_size = 12)  +
    theme(legend.position = "bottom")
  
}



plot_normalized_contributions_100_percent_stacked_bar_city = function (sector_yearly) {

  # normalize the Epc to show the relative fractions rather than the absolute contributions, showing how each sector contributions
  # change over time
  #. bar plots

  sector_relative <- sector_yearly %>%
  group_by(city,  year) %>%
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
  facet_wrap(~ city , scales = "free_y") +
  labs(
    title = "Relative sectoral contributions (labels only for >10%)",
    x = "Year", y = "Share of total Epc",
    fill = "Sector"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw(base_size = 12)  +
  theme(legend.position = "bottom")

}


plot_sectors_greater_pct10_city = function (sector_yearly) {
  

# INPUT: sector_yearly with columns: city, method, year, sector, epc_mean

# 1) Compute relative contributions (fractions that sum to 1 per city-method-year)
  sector_relative <- sector_yearly %>%
    group_by(city, year) %>%
    mutate(total_epc = sum(epc_mean, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA_real_))

  # 2) Collapse sectors below threshold into "Other" (threshold = 10%)
  threshold <- 0.10

  sector_rel_collapsed <- sector_yearly %>%
  group_by(city,  year) %>%
  mutate(total_epc = sum(epc_mean, na.rm = TRUE),
         rel_contrib = ifelse(total_epc > 0, epc_mean / total_epc, NA_real_),
         sector_collapsed = ifelse(rel_contrib >= threshold, sector, "Other")) %>%
  group_by(city, year, sector_collapsed) %>%
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
   p= ggplot(sector_rel_collapsed,
        aes(x = factor(year), y = rel_contrib, fill = sector_collapsed)) +
    geom_bar(stat = "identity", position = "stack", color = "black") +
    geom_text(aes(label = ifelse(rel_contrib >= threshold, pct_label, "")),
              position = position_stack(vjust = 0.5),
              size = 2.8, color = "black") +
    facet_wrap(~ city , scales = "free_y") +
    labs(
      title = "Relative sectoral contributions to per-capita emissions (≤10% collapsed to Other)",
      x = "Year", y = "Share of total Epc",
      fill = "Sector"
    ) +
    scale_y_continuous(labels = scales::percent) +
    theme_bw(base_size = 12)  +
    theme(legend.position = "bottom")

    n_cities <- length(unique(sector_rel_collapsed$city))
    if (n_cities > 2) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
}

seasonality_sectors_city=function(df_pc) {
  sector_long <- df_pc %>%
  dplyr::select(city, season, all_of(pc_cols)) %>%
  pivot_longer(cols = all_of(pc_cols), names_to = "sector", values_to = "epc_value") %>%
  mutate(
    sector = sub("^pc_", "", sector),
    season = factor(season, levels = c("DJF","MAM","JJA","SON"))
  )
  # Aggregate to seasonal means per sector
  sector_season_abs <- sector_long %>%
    group_by(city, season, sector) %>%
    summarise(epc_mean = mean(epc_value, na.rm = TRUE), .groups = "drop")

  # Convert to relative shares within each (city, method, season)
  sector_season_rel <- sector_season_abs %>%
    group_by(city, season) %>%
    mutate(total = sum(epc_mean, na.rm = TRUE),
          rel   = ifelse(total > 0, epc_mean / total, NA_real_)) %>%
    ungroup() %>%
    mutate(pct_label = paste0(round(rel * 100, 1), "%"))

  # Optional: collapse small shares (<10%) into "Other" per season
  threshold <- 0.10
  season_rel_collapsed <- sector_season_rel %>%
    group_by(city, season) %>%
    mutate(sector_c = ifelse(rel >= threshold, sector, "Other")) %>%
    ungroup() %>%
    group_by(city, season, sector_c) %>%
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
  
  # print(season_rel_collapsed %>% dplyr::filter(sector_c == "IND"))
  # print(sector_season_rel %>% filter(sector == "IND", season == "JJA") )
  
  # Plot: 100% stacked bars by season with % labels (labels only for ≥10% segments)
  p= ggplot(season_rel_collapsed,
        aes(x = season, y = rel, fill = sector_c)) +
    geom_col(position = "fill", color = "black") +
    geom_text(aes(label = ifelse(rel >= threshold, pct_label, "")),
              position = position_fill(vjust = 0.5), size = 3) +
    facet_wrap(~ city, scales = "free_y") +
    labs(
      title = "Relative sectoral contributions by season (shares sum to 100%)",
      x = "Season", y = "Share of per-capita emissions", fill = "Sector"
    ) +
    scale_y_continuous(labels = scales::percent) +
    # theme_bw(base_size = 12)  +
    theme_bw(base_size = 12) +
    theme(legend.position = "bottom")

    n_cities <- length(unique(season_rel_collapsed$city))
    if (n_cities > 2) {
      p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    plot_name="../figures/cities_sectors_seasonality.png"
  ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)

}

# seasonality_sectors_city_absolute_values=function(df_pc) {
# sector_long <- df_pc %>%
#     dplyr::select(city, season, all_of(pc_cols)) %>%
#     pivot_longer(
#       cols = all_of(pc_cols),
#       names_to = "sector",
#       values_to = "epc_value"
#     ) %>%
#     mutate(
#       sector = sub("^pc_", "", sector),
#       season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
#     )

#   # ---- Compute absolute seasonal means ----
#   sector_season_abs <- sector_long %>%
#     group_by(city, season, sector) %>%
#     summarise(epc_mean = mean(epc_value, na.rm = TRUE), .groups = "drop")

#   # ---- Determine 10% of the maximum absolute value ----
#   max_val <- max(sector_season_abs$epc_mean, na.rm = TRUE)
#   threshold_abs <- 0.05 * max_val
#   message("Absolute threshold (10% of max): ", signif(threshold_abs, 3))

#   # ---- Collapse sectors below threshold ----
#   sector_abs_collapsed <- sector_season_abs %>%
#     mutate(sector_c = ifelse(epc_mean >= threshold_abs, sector, "Other")) %>%
#     group_by(city, season, sector_c) %>%
#     summarise(epc_mean = sum(epc_mean, na.rm = TRUE), .groups = "drop")

#   # ---- Order sectors by mean magnitude ----
#   order_levels <- sector_abs_collapsed %>%
#     group_by(sector_c) %>%
#     summarise(avg_abs = mean(epc_mean, na.rm = TRUE), .groups = "drop") %>%
#     arrange(desc(if_else(sector_c == "Other", -Inf, avg_abs))) %>%
#     pull(sector_c)

#   sector_abs_collapsed <- sector_abs_collapsed %>%
#     mutate(
#       sector_c = factor(sector_c, levels = order_levels),
#       label_val = round(epc_mean, 2)
#     )

#   # ---- Plot ----
#   p <- ggplot(sector_abs_collapsed, aes(x = season, y = epc_mean, fill = sector_c)) +
#     geom_col(color = "black") +
#     geom_text(
#       aes(label = ifelse(epc_mean >= 0.05 * max_val, label_val, "")),
#       position = position_stack(vjust = 0.5),
#       size = 3,
#       color = "black"
#     ) +
#     facet_wrap(~city, scales = "free_y") +
#     labs(
#       title = "Absolute sectoral per-capita emissions by season",
#       subtitle = paste0(
#         "Sectors contributing <10% of global max (",
#         round(threshold_abs, 3),
#         ") collapsed into 'Other'"
#       ),
#       x = "Season",
#       y = "EPC (absolute units)",
#       fill = "Sector"
#     ) +
#     theme_bw(base_size = 12) +
#     theme(
#       legend.position = "bottom",
#       panel.grid.minor = element_blank(),
#       panel.grid.major.x = element_blank(),
#       plot.title = element_text(face = "bold", hjust = 0.5),
#       plot.subtitle = element_text(hjust = 0.5)
#     )

#   if (length(unique(sector_abs_collapsed$city)) > 2) {
#     p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   }
#   outfile="../figures/cities_sectors_seasonality_absolute_values.png"
#   ggsave(outfile, plot = p, width = 9, height = 6, dpi = 300)
#   message("✅ Saved absolute seasonality plot: ", outfile)
#   invisible(p)
# }

seasonality_sectors_city_absolute_values <- function(df_pc) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)

  # --- Reshape data ---
  sector_long <- df_pc %>%
    dplyr::select(city, season, all_of(pc_cols)) %>%
    pivot_longer(
      cols = all_of(pc_cols),
      names_to = "sector",
      values_to = "epc_value"
    ) %>%
    mutate(
      sector = sub("^pc_", "", sector),
      season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"))
    )

  # --- Compute absolute seasonal means ---
  sector_season_abs <- sector_long %>%
    group_by(city, season, sector) %>%
    summarise(epc_mean = mean(epc_value, na.rm = TRUE), .groups = "drop")

  # --- Collapse all sectors with epc_mean < 1.0 into "Other" ---
  threshold_abs <- 1.0
  message("Collapsing sectors with EPC < ", threshold_abs, " into 'Other'")

  sector_abs_collapsed <- sector_season_abs %>%
    mutate(sector_c = ifelse(epc_mean < threshold_abs, "Other", sector)) %>%
    group_by(city, season, sector_c) %>%
    summarise(epc_mean = sum(epc_mean, na.rm = TRUE), .groups = "drop")

  # --- Order legend by average magnitude ---
  order_levels <- sector_abs_collapsed %>%
    group_by(sector_c) %>%
    summarise(avg_abs = mean(epc_mean, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(if_else(sector_c == "Other", -Inf, avg_abs))) %>%
    pull(sector_c)

  sector_abs_collapsed <- sector_abs_collapsed %>%
    mutate(
      sector_c = factor(sector_c, levels = order_levels),
      label_val = ifelse(epc_mean >= threshold_abs, round(epc_mean, 2), NA_real_)
    )

  # --- Distinct color palette (visually clean, accessible) ---
  sector_palette <- c(
    "Power-Gen"                = "#E64B35",  # strong red
    "Road_Trans"               = "#4DBBD5",  # blue-teal
    "Oil_Ref"                  = "#FFC107",  # golden yellow
    "Residential_other_sectors"= "#00A087",  # green-turquoise
    "manufra_Comb"             = "#3C5488",  # dark blue
    "Avi-Climb/Descent"        = "#F39B7F",  # orange-pink
    "Avi-Landing/Takeoff"      = "#91D1C2",  # mint-green
    "Other"                    = "grey70"    # neutral gray
  )

  # --- Plot ---
  p <- ggplot(sector_abs_collapsed, aes(x = season, y = epc_mean, fill = sector_c)) +
    geom_col(color = "black") +
    geom_text(
      aes(label = ifelse(!is.na(label_val), label_val, "")),
      position = position_stack(vjust = 0.5),
      size = 4,
      fontface = "bold",
      color = "black"
    ) +
    facet_wrap(~city, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = sector_palette, drop = FALSE) +
    labs(
      title = "Absolute sectoral per-capita emissions by season",
      subtitle = paste0("Sectors with EPC < ", threshold_abs, " collapsed into 'Other'"),
      x = "Season",
      y = "EPC (absolute units)",
      fill = "Sector"
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  # --- Save output ---
  outfile <- "../figures/cities_sectors_seasonality_absolute_values.png"
  ggsave(outfile, plot = p, width = 9, height = 7.5, dpi = 300)
  message("✅ Saved absolute seasonality plot: ", outfile)

  invisible(p)
}

plot_epc_groups_cities <- function(df) {
  df_grouped <- df %>%
    mutate(period = case_when(
      year >= 2015 & year <= 2019 ~ "2015–2019",
        # year == 2020                ~ "2020",
        year >= 2020 & year <= 2021 ~ "2020–2021",
        year >= 2022 & year <= 2024 ~ "2022–2024",
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(period)) %>%
    group_by(city, period) %>%
    summarise(
      epc_mean = mean(Epc, na.rm = TRUE),
      epc_sd   = sd(Epc, na.rm = TRUE),
      n        = n(),
      .groups = "drop"
    )
  # y_max <- max(df_grouped$ymax, na.rm = TRUE) * 1.15
  # p <- ggplot(df_grouped, aes(x = period, y = epc_mean, fill = period)) +
  #   geom_col(alpha = 0.9) +
  #   geom_errorbar(
  #     aes(ymin = epc_mean - epc_sd, ymax = epc_mean + epc_sd),
  #     width = 0.2, colour = "black"
  #   ) +
  #   geom_text(aes(label = paste0("n=", n)), vjust = -0.8, size = 3.5) +
  #   facet_wrap(~ city, scales = "free_y") +   # each city independent
  #   # coord_cartesian(ylim = c(0, y_max)) +
  #   labs(
  #     title = "Per-capita emissions by grouped periods (mean ± sd)",
  #     x = "Period",
  #     y = "Per-capita emissions (mean ± SD)"
  #   ) +
  #   theme_bw(base_size = 12) (base_size = 12) +
  #   theme(legend.position = "none")
  # y_max <- max(df_grouped$epc_mean + df_grouped$epc_sd, na.rm = TRUE) * 1.15
  y_max <- max(df_grouped$epc_mean , na.rm = TRUE) * 1.05
# 
# Plot with fixed y-axis
p <- ggplot(df_grouped, aes(x = period, y = epc_mean, fill = period)) +
  geom_col(alpha = 0.9) +
  geom_errorbar(
    aes(ymin = epc_mean - epc_sd, ymax = epc_mean + epc_sd),
    width = 0.2, colour = "black"
  ) +
  geom_text(aes(label = paste0("n=", n)), vjust = -0.8, size = 3.5) +
  facet_wrap(~ city, scales = "fixed") +  # <-- fixed y axis
  coord_cartesian(ylim = c(0, y_max)) +   # <-- shared y-axis limit
  labs(
    title = "Per-capita emissions by grouped periods (mean ± sd)",
    x = "Period",
    y = "Per-capita emissions (mean ± SD)"
  ) +
  theme_bw(base_size = 12) +
  theme(legend.position = "none")
  # rotate x-axis labels if more than 2 cities
  n_cities <- length(unique(df_grouped$city))
  if (n_cities > 2) {
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  plot_name="../figures/cities_epc_periods.png"
  ggsave(plot_name, plot = p, width = 8, height = 6, dpi = 300)
  return(p)
}


plot_bg_city_enh <- function(city, basemap, timestr, data) {
  library(dplyr)
  library(ggplot2)
  library(patchwork)

  # df <- data %>%
  #   filter(!is.na(Epc)) %>%
  #   mutate(
  #     lat_mid  = as.numeric(lat_mid),
  #     mean_XCO2  = as.numeric(mean_XCO2),
  #     category = factor(category, levels = c("bg","city")),
  #     enh_out_is_nan = as.logical(enh_out_is_nan) # ensure logical
  #   )
  df <- data %>%
    # filter(!is.na(Epc)) %>%
    mutate(
      lat_mid  = as.numeric(lat_mid),
      mean_XCO2  = as.numeric(mean_XCO2),
      category = factor(category, levels = c("bg","city")),
      enh_out_is_nan = as.logical(enh_out_is_nan) # ensure logical
    )

  xlim_vals <- c(min(df$lat_mid, na.rm = TRUE) -0.05,
               max(df$lat_mid, na.rm = TRUE) + 0.05)
  # print(xlim_vals)
  shp_vals <- c(bg = 16, city = 17)   # shapes intact

  # Enhancement plot: shape by category, color conditional
  p_enh <- ggplot(df, aes(x = lat_mid, y = mean_XCO2,
                          shape = category,
                          color = ifelse(enh_out_is_nan, "NAflag", as.character(category)))) +
    geom_point(size = 2.8, alpha = 0.95) +
    scale_shape_manual(values = shp_vals, name = "Category") +
    scale_color_manual(
      values = c("bg" = "#1f77b4", "city" = "#d62728", "NAflag" = "grey60"),
      breaks = c("bg","city"),
      labels = c("Background","City"),
      name   = "Category"
    ) +
    scale_x_continuous(limits = xlim_vals) +
    labs(x = "Latitude (°)", y = "XCO2 (ppm)",
         title = paste0("XCO2: ", timestr)) +
    theme_minimal(base_size = 12)

  # EPC plot: same logic
  p_epc <- ggplot(df, aes(x = lat_mid, y = Epc,
                          shape = category,
                          color = ifelse(enh_out_is_nan, "NAflag", as.character(category)))) +
    geom_point(size = 2.8, alpha = 0.95) +
    scale_shape_manual(values = shp_vals, name = "Category") +
    scale_color_manual(
      values = c("bg" = "#1f77b4", "city" = "#d62728", "NAflag" = "grey60"),
      breaks = c("bg","city"),
      labels = c("Background","City"),
      name   = "Category"
    ) +
    scale_x_continuous(limits = xlim_vals) +
    labs(x = "Latitude (°)",
         y = expression(Epc~"(tCO"[2]*"/ person)"),
         title = paste0("Epc: ", timestr)) +
    theme_minimal(base_size = 12)

  # Combine vertically with shared legend
  (p_enh + p_epc) +
    plot_layout(ncol = 1, guides = "collect") &
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold"))
}

deltaCO2_comparisons <- function(city, basemap, timestr, data) {
  library(dplyr)
  library(ggplot2)
  library(patchwork)

  # df <- data %>%
  #   filter(!is.na(Epc)) %>%
  #   mutate(
  #     lat_mid  = as.numeric(lat_mid),
  #     mean_XCO2  = as.numeric(mean_XCO2),
  #     category = factor(category, levels = c("bg","city")),
  #     enh_out_is_nan = as.logical(enh_out_is_nan) # ensure logical
  #   )
  df <- data %>%
    # filter(!is.na(Epc)) %>%
    mutate(
      lat_mid  = as.numeric(lat_mid),
      mean_XCO2  = as.numeric(mean_XCO2),
      mean_ODIAC  = as.numeric(odiac_ppm),
      mean_EDGAR  = as.numeric(edgar_ppm),
      mean_VULCAN  = as.numeric(mean_vulcan_ppm),
      category = factor(category, levels = c("bg","city")),
      enh_out_is_nan = as.logical(enh_out_is_nan) # ensure logical
    )
 
 # get more differences

  df_delta <- df %>%
  # Remove rows that are not 'city' or 'bg'
  filter(category %in% c("city", "bg")) %>%
  # Compute ΔCO₂ only for 'city' bins, using bg mean as reference
  mutate(
    deltaCO2_sat = if_else(
      category == "city",
      mean_XCO2 - mean(mean_XCO2[category == "bg"], na.rm = TRUE),
      NA_real_
    ),
    deltaCO2_odiac = if_else(
      category == "city",
      odiac_ppm,# - mean(odiac_ppm[category == "bg"], na.rm = TRUE),
      NA_real_
    ),
    deltaCO2_edgar = if_else(
      category == "city",
      edgar_ppm,# - mean(edgar_ppm[category == "bg"], na.rm = TRUE),
      NA_real_
    ),
    deltaCO2_vulcan = if_else(
      category == "city",
      mean_vulcan_ppm,# - mean(mean_vulcan_ppm[category == "bg"], na.rm = TRUE),
      NA_real_
    )
  ) %>%
  # Keep only the city rows with valid ΔCO₂
  filter(category == "city")


  df_delta2 <- df %>%
  # Remove rows that are not 'city' or 'bg'
  filter(category %in% c("city", "bg")) %>%
  # Compute ΔCO₂ only for 'city' bins, using bg mean as reference
  mutate(
    deltaCO2_sat = if_else(
      category == "city",
      mean_XCO2 - mean(mean_XCO2[category == "bg"], na.rm = TRUE) - bio_ppm,
      NA_real_
    ),
    deltaCO2_odiac = if_else(
      category == "city",
      odiac_ppm, #- mean(odiac_ppm[category == "bg"], na.rm = TRUE)- bio_ppm,
      NA_real_
    ),
    deltaCO2_edgar = if_else(
      category == "city",
      edgar_ppm,# - mean(edgar_ppm[category == "bg"], na.rm = TRUE)- bio_ppm,
      NA_real_
    ),
    deltaCO2_vulcan = if_else(
      category == "city",
      mean_vulcan_ppm,# - mean(mean_vulcan_ppm[category == "bg"], na.rm = TRUE)- bio_ppm,
      NA_real_
    )
  ) %>%
  # Keep only the city rows with valid ΔCO₂
  filter(category == "city")

  
  
  
  df <- df_delta %>%
  mutate(across(starts_with("deltaCO2_"), as.numeric))
  df2 <- df_delta2 %>%
  mutate(across(starts_with("deltaCO2_"), as.numeric))

# --- Pivot longer for easy faceted plotting ---
df_long <- df %>%
  select(lat_mid, category,
         deltaCO2_sat, deltaCO2_odiac, deltaCO2_edgar, deltaCO2_vulcan) %>%
  pivot_longer(
    cols = c(deltaCO2_odiac, deltaCO2_edgar, deltaCO2_vulcan),
    names_to = "inventory",
    values_to = "deltaCO2_model"
  ) %>%
  mutate(inventory = recode(inventory,
                            deltaCO2_odiac = "ODIAC",
                            deltaCO2_edgar = "EDGAR",
                            deltaCO2_vulcan = "Vulcan"))

df_long2 <- df2 %>%
  select(lat_mid, category,
         deltaCO2_sat, deltaCO2_odiac, deltaCO2_edgar, deltaCO2_vulcan) %>%
  pivot_longer(
    cols = c(deltaCO2_odiac, deltaCO2_edgar, deltaCO2_vulcan),
    names_to = "inventory",
    values_to = "deltaCO2_model"
  ) %>%
  mutate(inventory = recode(inventory,
                            deltaCO2_odiac = "ODIAC",
                            deltaCO2_edgar = "EDGAR",
                            deltaCO2_vulcan = "Vulcan"))

  # print(df_delta$deltaCO2_sat)
  # print(df_delta2$deltaCO2_sat)
  # print(df$bio_ppm[df$category == "city"])
  
# --- Base plot ---
p1 <- ggplot(df_long, aes(y = deltaCO2_sat, x = deltaCO2_model, 
                         color = inventory, shape = inventory)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title = bquote(Delta * "CO"[2] ~ "(Inventory vs Satellite) for timestr: " ~ .(timestr)),
    x = expression(Delta * "CO"[2] ~ "Satellite (ppm)"),
    y = expression(Delta * "CO"[2] ~ "Inventory (ppm)"),
    color = "Inventory",
    shape = "Inventory"
  ) +
  scale_color_manual(values = c(
    "ODIAC" = "#1b9e77",
    "EDGAR" = "#d95f02",
    "Vulcan" = "#7570b3"
  )) +
  scale_shape_manual(values = c(
    "ODIAC" = 16,   # filled circle
    "EDGAR" = 17,   # filled triangle
    "Vulcan" = 15   # filled square
  )) +
  theme_minimal(base_size = 13) #+
  # theme(
  #   plot.title = element_text(hjust = 0.5, face = "bold"),
  #   legend.position = "right",
  #   panel.grid.minor = element_blank()
  # )

p2 <- ggplot(df_long2, aes(y = deltaCO2_sat, x = deltaCO2_model, 
                         color = inventory, shape = inventory)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, 
              linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title = bquote(Delta * "CO"[2] ~ "(Inventory vs Satellite) -bio for timestr: " ~ .(timestr)),
    x = expression(Delta * "CO"[2] ~ "Satellite (ppm)"),
    y = expression(Delta * "CO"[2] ~ "Inventory (ppm)"),
    color = "Inventory",
    shape = "Inventory"
  ) +
  scale_color_manual(values = c(
    "ODIAC" = "#1b9e77",
    "EDGAR" = "#d95f02",
    "Vulcan" = "#7570b3"
  )) +
  scale_shape_manual(values = c(
    "ODIAC" = 16,   # filled circle
    "EDGAR" = 17,   # filled triangle
    "Vulcan" = 15   # filled square
  )) +
  theme_minimal(base_size = 13) #+
  # theme(
  #   plot.title = element_text(hjust = 0.5, face = "bold"),
  #   legend.position = "right",
  #   panel.grid.minor = element_blank()
  # )
  (p1 + p2) +
    plot_layout(ncol = 1, guides = "collect") &
    theme(legend.position = "bottom",
          legend.title = element_text(face = "bold"))
# print(p)
}
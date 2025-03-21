#===============================================================================
# Create dataset to update trends page for Canadian cities, Nov 2024-March 2025
#===============================================================================

# Load packages
#=====================================

source('~/git/timathomas/functions/functions.r')
ipak(c('tidyverse', 'sf', 'lubridate', 'leaflet', 'plotly', 'htmlwidgets', 
       'broom', 'forecast'))

# Load downtown & MSA data
#=====================================

filepath <- '/Users/jpg23/data/downtownrecovery/spectus_exports/canada_trends/'

cities <- read.csv(paste0(filepath, 'agg_stopuplevelled_230399_20241121_to_20250320.csv'))
msa <- read.csv(paste0(filepath, 'msa_agg_stopuplevelled_230399_20241121_to_20250320.csv'))

head(cities)
head(msa)

unique(cities$PROVIDER_ID)
unique(msa$PROVIDER_ID)

msa %>% filter(MSA_NAME == 'Detroit-Warren-Dearborn\\') %>% head()

# Filter these out
msa1 <- msa %>% filter(MSA_NAME != 'Detroit-Warren-Dearborn\\')
unique(msa1$PROVIDER_ID)

msa1 %>% filter(PROVIDER_ID == "")

# Filter these out as well
msa2 <- msa1 %>% filter(PROVIDER_ID != "")
unique(msa2$PROVIDER_ID)

range(cities$ZONE_DATE)
range(msa2$ZONE_DATE)

# Remove outliers in downtown data
#=====================================

cities_no_outliers <- cities %>%
  group_by(CITY) %>%
  mutate(
    n_stops_cleaned = tsclean(N_STOPS),
    n_distinct_devices_cleaned = tsclean(N_DISTINCT_DEVICES)
  )

# Plot with vs without outliers
dt_outliers_unique <- plot_ly() %>%
  add_lines(data = cities_no_outliers,
            x = ~ZONE_DATE, y = ~N_DISTINCT_DEVICES,
            name = ~paste0(CITY, ': downtown'),
            opacity = .7,
            split = ~CITY,
            text = ~paste0(CITY, ' downtown: ', round(N_DISTINCT_DEVICES, 3)),
            line = list(shape = "linear", color = '#b4e0a8')) %>%  
  add_lines(data = cities_no_outliers,
            x = ~ZONE_DATE, y = ~n_distinct_devices_cleaned,
            name = ~paste0(CITY, ': downtown'),
            opacity = .7,
            split = ~CITY,
            text = ~paste0(CITY, ' downtown: ', round(n_distinct_devices_cleaned, 3)),
            line = list(shape = "linear", color = '#445e3d'))

dt_outliers_unique

saveWidget(
  dt_outliers_unique,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/nov2024_march2025/downtown_unique_devices_outliers_vs_not.html')

dt_outliers_total <- plot_ly() %>%
  add_lines(data = cities_no_outliers,
            x = ~ZONE_DATE, y = ~N_STOPS,
            name = ~paste0(CITY, ': downtown'),
            opacity = .7,
            split = ~CITY,
            text = ~paste0(CITY, ' downtown: ', round(N_STOPS, 3)),
            line = list(shape = "linear", color = '#b4e0a8')) %>%  
  add_lines(data = cities_no_outliers,
            x = ~ZONE_DATE, y = ~n_stops_cleaned,
            name = ~paste0(CITY, ': downtown'),
            opacity = .7,
            split = ~CITY,
            text = ~paste0(CITY, ' downtown: ', round(n_stops_cleaned, 3)),
            line = list(shape = "linear", color = '#445e3d'))

dt_outliers_total

saveWidget(
  dt_outliers_total,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/nov2024_march2025/downtown_total_stops_outliers_vs_not.html')

# Join them
#=====================================

dt_goodnames <- cities_no_outliers %>%
  mutate(
    CITY = str_remove_all(CITY, '\\.'),
    CITY = str_replace_all(CITY, 'é', 'e'),
    CITY = case_when(
      CITY == "Ottawa - Gatineau (Ontario part / partie de l'Ontario)" ~ 'Ottawa',
      TRUE ~ CITY
    )
  )

unique(dt_goodnames$CITY)

msa_names <- read.csv('/Users/jpg23/data/downtownrecovery/sensitivity_analysis/msa_names.csv')

msa_cities <- unique(msa_names$city)
dt_cities <- unique(dt_goodnames$CITY)

setdiff(dt_cities, msa_cities)
setdiff(msa_cities, dt_cities)

dt_new <- dt_goodnames %>% 
  rename(city = CITY) %>%
  left_join(msa_names) %>% 
  data.frame()

head(dt_new)
head(msa2)

dt_new %>% filter(is.na(msa_name)) # should be no rows

final_df <-
  dt_new %>% rename(date = ZONE_DATE) %>% select(-PROVIDER_ID) %>%
  left_join(msa2 %>% 
              rename(msa_name = MSA_NAME, 
                     date = ZONE_DATE, 
                     n_stops_msa = N_STOPS, 
                     n_distinct_devices_msa = N_DISTINCT_DEVICES) %>%
              mutate(n_stops_msa = as.integer(n_stops_msa)), 
            by = c('msa_name', 'date')) %>%
  select(-c(N_STOPS, N_DISTINCT_DEVICES, PROVIDER_ID)) %>%
  mutate(normalized_distinct = n_distinct_devices_cleaned/n_distinct_devices_msa,
         normalized_stops = n_stops_cleaned/n_stops_msa)

head(final_df)

msa_distinct <- plot_ly() %>%
  add_lines(data = final_df,
            x = ~date, y = ~n_distinct_devices_msa,
            name = ~paste0(city, ': distinct devices (MSA)'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ': distinct devices (MSA):', round(n_distinct_devices_msa, 3)),
            line = list(shape = "linear", color = 'orange')) 

msa_distinct

saveWidget(
  msa_distinct,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/nov2024_march2025/msa_distinct.html')

final_df1 <- final_df %>%
  select(-c(n_stops_msa, n_distinct_devices_msa, n_stops_cleaned,
            n_distinct_devices_cleaned))

head(final_df1)

plot_ly() %>%
  add_lines(data = final_df1,
            x = ~date, y = ~normalized_stops,
            name = ~paste0(city, ': downtown'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ' downtown: ', round(normalized_stops, 3)),
            line = list(shape = "linear", color = 'blue')) 

norm_dist <- plot_ly() %>%
  add_lines(data = final_df1,
            x = ~date, y = ~normalized_distinct,
            name = ~paste0(city, ': downtown'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ' downtown: ', round(normalized_distinct, 3)),
            line = list(shape = "linear", color = 'red'))

norm_dist

saveWidget(
  norm_dist,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/nov2024_march2025/normalized_distinct.html')

# Remove outliers for normalized data
#=====================================

final_df_no_outliers <- final_df %>%
  group_by(city) %>%
  mutate(
    normalized_distinct_clean = tsclean(normalized_distinct),
    normalized_stops_clean = tsclean(normalized_stops)
  ) %>%
  ungroup() %>%
  data.frame()

# Check the resulting dataframe
head(final_df_no_outliers)

norm2 <- plot_ly() %>%
  add_lines(data = final_df_no_outliers,
            x = ~date, y = ~normalized_stops,
            name = ~paste0(city, ': downtown'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ' downtown: ', round(normalized_stops, 3)),
            line = list(shape = "linear", color = '#b4e0a8')) %>%
  add_lines(data = final_df_no_outliers,
            x = ~date, y = ~normalized_stops_clean,
            name = ~paste0(city, ': downtown'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ' downtown: ', round(normalized_stops_clean, 3)),
            line = list(shape = "linear", color = '#445e3d'))

norm2

saveWidget(
  norm2,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/nov2024_march2025/normalized_distinct_no_outliers.html')

# Export final data
#=====================================

write.csv(final_df_no_outliers,
          '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/nov2024_march2025/stopuplevelled_canada_nov2024_march2025_outliers_removed.csv',
          row.names = F)

# Combine with earlier data
#=====================================

# created in 'canada_sept_nov_2024.R':
before <- read.csv('/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/sept_nov_2024/stopuplevelled_canada_sept_nov_2024_outliers_removed.csv')
head(before)
range(before$date)

head(final_df_no_outliers)
range(final_df_no_outliers$date)

both <- rbind(before %>% filter(date < as.Date('2024-11-21')), 
              final_df_no_outliers)

both_stops_plot <- plot_ly() %>%
  add_lines(data = both,
            x = ~date, y = ~normalized_stops_clean,
            name = ~paste0(city, ': downtown'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ' downtown: ', round(normalized_stops_clean, 3)),
            line = list(shape = "linear", color = '#445e3d'))

both_stops_plot

saveWidget(
  both_plot,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/may2024_march2025/normalized_stops_no_outliers.html')

both_distinct_plot <- plot_ly() %>%
  add_lines(data = both,
            x = ~date, y = ~normalized_distinct_clean,
            name = ~paste0(city, ': downtown'),
            opacity = .7,
            split = ~city,
            text = ~paste0(city, ' downtown: ', round(normalized_distinct_clean, 3)),
            line = list(shape = "linear", color = '#445e3d'))

both_distinct_plot

saveWidget(
  both_distinct_plot,
  '/Users/jpg23/UDP/downtown_recovery/trend_updates/canada/may2024_march2025/normalized_distinct_no_outliers.html')

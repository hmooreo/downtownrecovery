#===============================================================================
# Show how a couple different cities look with various area size cutoffs for
# commercial districts
#===============================================================================

# Load packages
#-----------------------------------------

library('tidyverse', 'sf')

# Load data
#-----------------------------------------

base_path <- '/Users/jpg23/data/downtownrecovery/commercial_districts/'

c <- sf::st_read(paste0(base_path, 'byeonghwa_commercial_districts.geojson'))
head(c)

# New York, Ottawa, San Francisco, Toronto, Columbus, Tulsa, Salt Lake City, Phoenix
c_small <- c %>% 
  filter(MSA_NAME %in% c('New York-Newark-Jersey City', 'Ottawa',
                         'San Francisco-Oakland-Berkeley', 'Toronto',
                         'Columbus', 'Tulsa', 'Salt Lake City', 
                         'Phoenix-Mesa-Chandler'))
unique(c_small$MSA_NAME)

# Filter to only polygons at least x square meters
c300 <- c_small %>% filter(Area.m2. >= 300000)
c600 <- c_small %>% filter(Area.m2. >= 600000)
c900 <- c_small %>% filter(Area.m2. >= 900000)

# Filter to only top half of districts in each place
c_half <- c_small %>%
  group_by(MSA_NAME) %>%
  mutate(top_half_threshold = quantile(Area.m2., 0.5, na.rm = TRUE)) %>%
  filter(Area.m2. >= top_half_threshold) %>%
  select(-top_half_threshold)

# Filter to only top quintile of districts in each place
c_quintile <- c_small %>%
  group_by(MSA_NAME) %>%
  mutate(top_quintile_threshold = quantile(Area.m2., 0.8, na.rm = TRUE)) %>%
  filter(Area.m2. >= top_quintile_threshold) %>%
  select(-top_quintile_threshold)

# Filter to only top 33% of districts in each place
c_33 <- c_small %>%
  group_by(MSA_NAME) %>%
  mutate(top_33_threshold = quantile(Area.m2., 0.6666, na.rm = TRUE)) %>%
  filter(Area.m2. >= top_33_threshold) %>%
  select(-top_33_threshold)

# Export these to look at them in QGIS
sf::st_write(c_small, paste0(base_path, 'commercial_districts_paper/commercial_districts_sample.geojson'))
sf::st_write(c300, paste0(base_path, 'commercial_districts_paper/commercial_districts_300_sq_m.geojson'))
sf::st_write(c600, paste0(base_path, 'commercial_districts_paper/commercial_districts_600_sq_m.geojson'))
sf::st_write(c900, paste0(base_path, 'commercial_districts_paper/commercial_districts_900_sq_m.geojson'))
sf::st_write(c_half, paste0(base_path, 'commercial_districts_paper/commercial_districts_top_half.geojson'))
sf::st_write(c_quintile, paste0(base_path, 'commercial_districts_paper/commercial_districts_top_quintile.geojson'))
sf::st_write(c_33, paste0(base_path, 'commercial_districts_paper/commercial_districts_top_33.geojson'))

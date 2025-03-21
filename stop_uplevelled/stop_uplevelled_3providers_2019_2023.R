#===============================================================================
# Explore 3 providers from stop_uplevelled table, for all of 2019 and 2023
#===============================================================================

# Load packages
#=====================================

source('~/git/timathomas/functions/functions.r')
ipak(c('tidyverse', 'plotly', 'htmlwidgets', 'scales'))

# Load data
#=====================================

d0 <- read.csv('/Users/jpg23/data/downtownrecovery/stop_uplevelled_3providers_2019_2023/stop_uplevelled_explore_take2.csv')
d0
d <- d0 %>% distinct()
d

# Grouped bar chart
#=====================================

# separate charts for US and Canada

us <- ggplot(d %>% filter(COUNTRY_CODE=='US'), 
             aes(x = factor(YEAR), y = UNIQUE_DEVICES, fill = PROVIDER_ID)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Unique Devices by Year and Provider (US)",
       x = "Year",
       y = "Number of Unique Devices",
       fill = "Provider") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

us

ggsave("/Users/jpg23/UDP/downtown_recovery/stop_uplevelled_exploration/us_stop_uplevelled_by_provider_2019_2023.png", 
       us, width = 8, height = 5, dpi = 300)

ca <- ggplot(d %>% filter(COUNTRY_CODE=='CA'), 
             aes(x = factor(YEAR), y = UNIQUE_DEVICES, fill = PROVIDER_ID)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Unique Devices by Year and Provider (Canada)",
       x = "Year",
       y = "Number of Unique Devices",
       fill = "Provider") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ca

ggsave("/Users/jpg23/UDP/downtown_recovery/stop_uplevelled_exploration/ca_stop_uplevelled_by_provider_2019_2023.png", 
       ca, width = 8, height = 5, dpi = 300)

# Grouped bar chart - filter out 'PAPA'
#=====================================

# separate charts for US and Canada

us <- ggplot(d %>% filter(COUNTRY_CODE=='US' & PROVIDER_ID!='PAPA'), 
             aes(x = factor(YEAR), y = UNIQUE_DEVICES, fill = PROVIDER_ID)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Unique Devices by Year and Provider (US)",
       x = "Year",
       y = "Number of Unique Devices",
       fill = "Provider") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

us

ggsave("/Users/jpg23/UDP/downtown_recovery/stop_uplevelled_exploration/us_stop_uplevelled_by_provider_2019_2023_no_PAPA.png", 
       us, width = 8, height = 5, dpi = 300)

ca <- ggplot(d %>% filter(COUNTRY_CODE=='CA' & PROVIDER_ID!='PAPA'), 
             aes(x = factor(YEAR), y = UNIQUE_DEVICES, fill = PROVIDER_ID)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Unique Devices by Year and Provider (Canada)",
       x = "Year",
       y = "Number of Unique Devices",
       fill = "Provider") +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ca

ggsave("/Users/jpg23/UDP/downtown_recovery/stop_uplevelled_exploration/ca_stop_uplevelled_by_provider_2019_2023_no_PAPA.png", 
       ca, width = 8, height = 5, dpi = 300)

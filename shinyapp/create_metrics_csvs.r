library(ggplot2)
library(gsignal)
library(readxl)
library(lmtest)
library(stringr)
library(forecast)
library(arrow)
library(lubridate)
library(tidyverse)
library(broom)
library(dplyr)
library(scales)
library(plotly)
library(astsa)

rm(list=ls())
gc()


# updated cuebiq query
cuebiq_data_stoppers <- read.csv("~/data/downtownrecovery/update_2023/stoppers_query_20230413.csv") %>%
  mutate(
    event_date = str_extract(event_date, "\\d+$"),
    as_datetime = as.Date(as.character(event_date), format = "%Y%m%d"),
    source = "stoppers_hll",
    userbase = as.numeric(userbase))%>%
  select(-X)

cuebiq_stops_uplevelled <- read.csv("~/data/downtownrecovery/counts/cuebiq_daily_agg.csv")

cuebiq_data_stoppers %>% glimpse()

# read in safegraph raw device counts
safegraph_data <- read_parquet("~/data/downtownrecovery/counts/safegraph_dt_recovery.pq") %>%
  mutate(city = str_replace(city, "é", "e")) %>%
  left_join(cuebiq_stops_uplevelled %>% distinct(city, country_code)) %>%
  select(-country, -postal_code, -is_downtown, -normalized_visits_by_state_scaling) %>%
  group_by(date_range_start, city, country_code) %>%
  summarise(raw_visit_counts = sum(raw_visit_counts),
            normalized_visits_by_total_visits = sum(normalized_visits_by_total_visits)) %>%
  mutate(source = "safegraph")

safegraph_data %>% glimpse()


cuebiq_stoppers_agg <- cuebiq_data_stoppers %>%
  # day, state, city, country, and source are the columns to index by
  group_by(as_datetime, geography_name, city, source) %>%
  summarise(raw_visit_counts = n_devices,
            total_visits = userbase,
            date_range_start = lubridate::floor_date(as_datetime,
                                                     unit = "week",
                                                     week_start = getOption("lubridate.week.start", 1))
  ) %>%
  ungroup() %>%
  group_by(date_range_start, geography_name, city, source) %>%
  summarise(
    normalized_visits_by_total_visits = sum(raw_visit_counts) / sum(total_visits),
    raw_visit_counts = sum(raw_visit_counts),
    total_visits = sum(total_visits)
  ) %>%
  ungroup()

cuebiq_stoppers_agg %>% glimpse()
safegraph_data %>% glimpse()

#shared_start <- min(cuebiq_stoppers_agg$date_range_start)
shared_start <- "2021-12-06"
# last date of safegraph's continuous panel dataset
# there is data beyond this, but for comparison's sake end it here and use cuebiq
shared_end <- "2022-05-02"

safegraph_data_subset <- safegraph_data %>%
  dplyr::filter((date_range_start <= shared_end) &
                  (date_range_start >= shared_start)
  )

all_counts <- bind_rows(safegraph_data_subset,
                        cuebiq_stoppers_agg) %>%
  dplyr::filter((date_range_start <= shared_end) &
                  (date_range_start >= shared_start)) %>%
  dplyr::distinct(date_range_start, city, source, .keep_all = TRUE)


n_weeks <- all_counts %>% distinct(date_range_start) %>% nrow()

# weekly normalized_visits - no minmax scaling
# stoppers_hll has a smaller userbase but about the same downtown raw visit counts
# so the normalized_visits are always some factor higher than safegraph, except dallas and oklahoma city
ggplotly(all_counts %>%
           ggplot(aes(x = date_range_start, y = normalized_visits_by_total_visits, color = source, alpha = .75)) +
           geom_line() +
           facet_wrap(.~city, scales = 'free', nrow = 6) +
           theme(axis.text = element_blank()
           ))

minmax_normalizer <- function(x, min_x, max_x) {
  return((x- min_x) /(max_x-min_x))
}

standard_normalizer <- function(x, mean_x, sd_x) {
  return((x - mean_x) / sd_x)
  
}

city_source_scaling_df <- all_counts %>%
  dplyr::group_by(city, source) %>%
  summarise(min_val = min(normalized_visits_by_total_visits, na.rm = TRUE),
            max_val = max(normalized_visits_by_total_visits, na.rm = TRUE),
            avg_val = mean(normalized_visits_by_total_visits, na.rm = TRUE),
            sd_val = sd(normalized_visits_by_total_visits, na.rm = TRUE)
  ) %>%
  ungroup()

city_source_scaling_df %>% glimpse()



# create df with scaled columns
comparisons_df <- all_counts %>% 
  left_join(city_source_scaling_df) %>%
  mutate(minmax_scaled = minmax_normalizer(normalized_visits_by_total_visits, min_val, max_val),
         standard_scaled = standard_normalizer(normalized_visits_by_total_visits, avg_val, sd_val))

safegraph_ts_scaled <- comparisons_df %>%
  dplyr::filter(source == 'safegraph') %>%
  pivot_wider(id_cols = 'date_range_start', names_from = 'city', values_from = 'minmax_scaled') %>%
  ungroup() %>%
  arrange(date_range_start)

safegraph_ts_scaled %>% glimpse()



safegraph_data_subset <- safegraph_data %>%
  dplyr::filter((date_range_start <= shared_end) &
                  (date_range_start >= shared_start)
  )

cuebiq_ts_sclaed <- comparisons_df %>%
  dplyr::filter(source == 'stoppers_hll') %>%
  pivot_wider(id_cols = 'date_range_start', names_from = 'city', values_from = 'normalized_visits_by_total_visits') %>%
  ungroup() %>%
  arrange(date_range_start)



cosine_similarity <- function(A, B) {
  
  return(sum(A*B)/sqrt(sum(A^2) * sum(B^2)))
}


cos_similarities <- list()


for (cities in cuebiq_data_stoppers %>% distinct(city) %>% arrange(city) %>% pull(city)) {
  
  SG <- comparisons_df %>%
    dplyr::filter((source == 'safegraph') &
                    (city == cities)) %>%
    arrange(date_range_start) %>%
    pull(minmax_scaled) %>%
    as.vector()
  
  HLL <- comparisons_df %>%
    dplyr::filter((source == 'stoppers_hll') & (city == cities)) %>%
    arrange(date_range_start) %>%
    pull(minmax_scaled) %>%
    as.vector()
  
  cos_similarities[[cities]] <- cosine_similarity(SG, HLL)
}

cos_similarity_scores = cbind(names(cos_similarities),
                              do.call("rbind", cos_similarities)) %>%
  as.data.frame() %>%
  rename(city = V1, cosine_score = V2)


cos_similarity_scores %>% arrange(cosine_score)

# after scaling, how does the difference change over time?
p <- comparisons_df %>%
  distinct() %>%
  left_join(cos_similarity_scores) %>%
  arrange(cosine_score) %>%
  mutate_at(vars(city), funs(factor(., levels=unique(.)))) %>%
  ggplot(aes(x = date_range_start, y = minmax_scaled, color = source)) +
  geom_line() +
  facet_wrap(.~city, ncol = 6)

p

comparisons_with_diff <- comparisons_df %>%
  pivot_wider(
    id_cols = c('date_range_start', 'city'),
    names_from = 'source',
    # change this to test different comparison metrics
    values_from = 'normalized_visits_by_total_visits') %>%
  arrange(date_range_start) %>%
  group_by(city) %>%
  mutate(
    normalized_safegraph_cuebiq_diff = stoppers_hll - safegraph
  )

comparisons_df %>% glimpse()

# 
p0 <- comparisons_with_diff %>%
  ggplot(aes(x = normalized_safegraph_cuebiq_diff)) +
  geom_density()

p0

# t test for all observations - fails for all cities over all time periods
t.test(x = comparisons_with_diff %>%
         pull(normalized_safegraph_cuebiq_diff)
)

qqnorm(comparisons_with_diff$normalized_safegraph_cuebiq_diff)

# granger test by city
comparisons_with_diff %>% glimpse()


granger_test_city <- lapply(split(comparisons_with_diff, factor(comparisons_with_diff$city)), function(x) {lmtest::grangertest(x$safegraph, x$stoppers_hll, 1)})

granger_test_sheet <- as.data.frame(do.call(rbind, granger_test_city))
granger_test_sheet$city <- row.names(granger_test_sheet)
granger_test_sheet %>% glimpse()
t_test_sheet_df <- t_test_sheet %>%
  unnest_wider(conf.int, names_sep = '_') %>%
  unnest(statistic:data.name) %>% as.data.frame()

t_test_sheet_df %>%
  select(statistic, p.value, city) %>%
  #dplyr::filter(p.value >= .05) %>%
  arrange(-p.value)





write.xlsx2(t_test_sheet_df %>% select(city, statistic:data.name),
            file = "data/downtownrecovery/t_tests/t_tests.xlsx",
            sheetName = "Prepandemic",
            col.names = TRUE,
            row.names = TRUE,
            append = FALSE)

all_seasons <- comparisons_df %>% pull(Season) %>% unique()
all_seasons

for (seasons in all_seasons[-1]) {
  season_df <- comparisons_df %>% dplyr::filter(Season == seasons)
  t_test_city_seasons <- lapply(split(season_df, factor(season_df$city)), function(x) {t.test(x$normalized_safegraph_cuebiq_diff)})
  t_test_sheet <- as.data.frame(do.call(rbind, t_test_city_seasons))
  t_test_sheet$city <- row.names(t_test_sheet)
  t_test_sheet_df <- t_test_sheet %>%
    unnest_wider(conf.int, names_sep = '_') %>%
    unnest(statistic:data.name) %>% as.data.frame()
  
  write.xlsx2(t_test_sheet_df %>% select(city, statistic:data.name),
              "data/downtownrecovery/t_tests/t_tests.xlsx",
              sheetName = seasons,
              col.names = TRUE,
              row.names = TRUE,
              append = TRUE)
}

# bind each separate list to into a data frame
t_test_df <- as.data.frame(do.call(rbind, t_test_list))

t_test_df %>% glimpse()


normalized_cuebiq <-  cuebiq_stoppers_agg %>%
  dplyr::filter(date_range_start > shared_end) %>%
  left_join(comparisons_with_diff %>%
              ungroup() %>%
              # give this  ___ to 'adjust' to safegraph counts
              dplyr::filter((date_range_start >= "2022-01-01") & (date_range_start <= shared_end)) %>%
              group_by(city) %>%
              summarise(
                #' on average, what would cuebiq's normalized visits have to be multiplied by to
                #' get safegraph's normalized visits for the last ___? 
                ratio = mean(safegraph / stoppers_hll)) %>% 
              dplyr::filter(!is.na(ratio)) %>%
              distinct()
  ) %>%
  group_by(date_range_start, city) %>%
  mutate(normalized_visits_by_total_visits = ratio * normalized_visits_by_total_visits,
         source = 'normalized_cuebiq') %>%
  ungroup()

normalized_cuebiq %>% glimpse()

ggplotly(rbind(safegraph_data_subset, cuebiq_stoppers_agg, normalized_cuebiq) %>% 
           ggplot(aes(x = date_range_start, y = normalized_visits_by_total_visits, color = source)) +
           geom_line() +
           facet_wrap(.~city, ncol = 6, scales = 'free') +
           theme(axis.text = element_blank()))


# TODO: append new downtown rqs to old and find 11, 13, 15, etc week rolling average to be 
# recovery patterns plot

# TODO: append new seasonal rqs to old

# TODO: update the csvs used in javascripts to create plotly plots 

# TODO: write up methodology changes since addition of cuebiq data

ntv_full_ts <- rbind(safegraph_data %>%
                       ungroup() %>%
                       dplyr::filter(date_range_start <= shared_end) %>%
                       select(date_range_start, city, normalized_visits_by_total_visits),
                     normalized_cuebiq %>%
                       dplyr::filter(date_range_start > shared_end) %>%
                       select(date_range_start, city, normalized_visits_by_total_visits)) %>%
  distinct() %>%
  arrange(date_range_start)

write.csv(ntv_full_ts, "~/data/downtownrecovery/full_ntv.csv")

ntv_full_ts %>% glimpse()
pd <- position_dodge(0.78)

ntv_full_ts %>%
  ggplot(aes(x = covid_era, y = mean)) +
  #draws the means
  geom_point(position=pd) +
  #draws the CI error bars
  geom_errorbar(aes(ymin=mean-2*se, ymax=mean+2*se, 
                                color=covid_era), width=.1, position=pd) +
  facet_wrap(.~city)




downtown_rq_cuebiq <- rbind(safegraph_data %>%
                              ungroup() %>%
                              dplyr::filter(date_range_start <= shared_end) %>%
                              select(date_range_start, city, normalized_visits_by_total_visits),
                            normalized_cuebiq %>%
                              dplyr::filter(date_range_start > shared_end) %>%
                              select(date_range_start, city, normalized_visits_by_total_visits)) %>%
  mutate(week_num = lubridate::isoweek(date_range_start),
         year = lubridate::year(date_range_start)) %>%
  select(-date_range_start) %>%
  pivot_wider(id_cols = c('city', 'week_num'), names_from = 'year', names_prefix = 'ntv_', values_from = 'normalized_visits_by_total_visits') %>%
  # ignore 
  #dplyr::filter(!is.na(ntv_2023)) %>%
  mutate(rec_2022 = ntv_2022 / ntv_2019,
         rec_2023 = ntv_2023 / ntv_2019) %>%
  pivot_longer(cols = c('rec_2022', 'rec_2023'), names_to = "year", values_to = "normalized_visits_by_total_visits") %>%
  mutate(year = substr(year, 5, 9),
         week = as.Date(paste(year, week_num, 1, sep = "_"), format = "%Y_%W_%w"),
         metric = "downtown") %>%
  dplyr::filter(!is.na(normalized_visits_by_total_visits))

downtown_rq_cuebiq %>% glimpse()

downtown_rq_safegraph <- read.csv("git/downtownrecovery/shinyapp/input_data/all_weekly_metrics.csv") %>%
  dplyr::filter(city != "Hamilton") %>%
  filter(!is.na(normalized_visits_by_total_visits) & (week < min(downtown_rq_cuebiq$week))) %>%
  distinct()


downtown_rq_safegraph %>% glimpse()

summary(downtown_rq_safegraph)

max(downtown_rq_safegraph$week)

min(downtown_rq_cuebiq$week)

downtown_rq <- rbind(downtown_rq_cuebiq %>%
                       left_join(downtown_rq_safegraph %>%
                                   select(city, region, metro_size, display_title) %>% distinct()) %>%
                       select(-ntv_2019,  -ntv_2022, -ntv_2023, -week_num, -year) %>%
                       mutate(source = 'normalized_cuebiq'),
                     downtown_rq_safegraph %>%
                       mutate(source = 'safegraph') %>%
                       dplyr::filter(week <= as.Date(shared_end)))

downtown_rq %>% glimpse()

ggplotly(downtown_rq %>%
           dplyr::filter((week >= "2022-01-01") & (metric == "downtown")) %>%
           ggplot(aes(x = week, y = normalized_visits_by_total_visits, color = source)) +
           geom_line() +
           facet_wrap(.~city, nrow = 6) +
           theme(axis.text = element_blank()) +
           scale_color_manual(values = c("safegraph" = "#4daf4a",
                                         "normalized_cuebiq" = "#fb8072")))

recovery_patterns_plot <- function(df, metric, n) {
  starting_lqs <- df %>%
    dplyr::filter(week == min(week)) %>%
    dplyr::select(city, region, week, normalized_visits_by_total_visits) %>%
    dplyr::arrange(desc(normalized_visits_by_total_visits))
  
  ending_lqs <- df %>%
    group_by(city) %>%
    mutate(latest_data = max(week)) %>%
    ungroup() %>%
    dplyr::filter(week == latest_data) %>%
    dplyr::select(city, region, week, normalized_visits_by_total_visits) %>%
    dplyr::arrange(desc(normalized_visits_by_total_visits))
  
  total_cities <- length(unique(df$city))
  total_weeks <- length(unique(df$week))
  
  g1 <- ggplot(df) + aes(
    x = week,
    y = normalized_visits_by_total_visits,
    group = city,
    color = region,
    label = city,
    
  ) + geom_line(aes(data_id = city), size = 1) +
    geom_point(aes(tooltip =
                     paste0(
                       "<b>City:</b> ", city, "<br>",
                       "<b>Week:</b> ", week, "<br>",
                       n, " <b>week rolling average:</b> ", percent(round(normalized_visits_by_total_visits, 2), 1), "<br>"
                     ),
                   data_id = city), size = 1, alpha = .1) +
    geom_label_repel(
      data = starting_lqs,
      size = 3,
      direction = "y",
      hjust = "right",
      force = 1,
      na.rm  = TRUE,
      min.segment.length = 0,
      segment.curvature = 1e-20,
      segment.angle = 20,
      # this was determined to be a decent offset based on the commented out line below
      # leaving it in as future reference 
      nudge_x = rep(-35, times = total_cities),
      show.legend = FALSE
      #nudge_x = rep(-total_weeks / as.numeric(input$rolling_window[1]), times = total_cities),
    ) +
    geom_label_repel(
      data = ending_lqs,
      size = 3,
      direction = "y",
      hjust = "left",
      force = 1,
      na.rm = TRUE,
      min.segment.length = 0,
      segment.curvature =  1e-20,
      segment.angle = 20,
      # this was determined to be a decent offset based on the commented out line below
      # leaving it in as future reference 
      nudge_x = rep(35, times = total_cities),
      show.legend = FALSE
      #nudge_x = rep(total_weeks / as.numeric(input$rolling_window[1]), times = total_cities),
    ) +
    labs(title = paste(names(named_metrics[named_metrics == metric])),
         subtitle = paste(n, " week rolling average"),
         color = "Region",
         y = "Metric",
         x = "Month"
    ) +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
      axis.title = element_text(size = 10, hjust = .5),
      plot.title = element_text(size = 12, hjust = .5),
      plot.subtitle = element_text(size = 10, hjust = .5)
    ) +
    scale_x_date(
      breaks = "4 weeks",
      date_labels = "%Y.%m",
      expand = expansion(mult = .15)
    ) +
    scale_y_continuous("Metric", labels = scales::percent) +
    # 2023/01 update: changed to school of cities colors
    scale_color_manual(values = region_colors)
  g1
}

recovery_patterns_plot(downtown_rq %>%
                         dplyr::filter((week >= "2022-01-01") &
                                         (metric == "downtown") &
                                         (display_title %in% plot_cities)), "downtown", 0)


# this will become all_weekly_metrics.csv, minus the Season column

write.csv(downtown_rq %>% select(-source), "~/git/downtownrecovery/shinyapp/input_data/all_weekly_metrics_cuebiq_update_hll.csv")



downtown_rq[(downtown_rq$week >= base::as.Date("2020-03-02")) & (downtown_rq$week < base::as.Date("2020-06-01")), "Season"] = "Season_1" 
downtown_rq[(downtown_rq$week >= base::as.Date("2020-06-01")) & (downtown_rq$week < base::as.Date("2020-08-31")), "Season"] = "Season_2"
downtown_rq[(downtown_rq$week >= base::as.Date("2020-08-31")) & (downtown_rq$week < base::as.Date("2020-11-30")), "Season"] = "Season_3"
downtown_rq[(downtown_rq$week >= base::as.Date("2020-11-30")) & (downtown_rq$week < base::as.Date("2021-03-01")), "Season"] = "Season_4"
downtown_rq[(downtown_rq$week >= base::as.Date("2021-03-01")) & (downtown_rq$week < base::as.Date("2021-05-31")), "Season"] = "Season_5"
downtown_rq[(downtown_rq$week >= base::as.Date("2021-05-31")) & (downtown_rq$week < base::as.Date("2021-08-30")), "Season"] = "Season_6"
downtown_rq[(downtown_rq$week >= base::as.Date("2021-08-30")) & (downtown_rq$week < base::as.Date("2021-12-06")), "Season"] = "Season_7"
# these are edited to be consistent with policy brief but they do not quite fully represent the months in season 8 and 9- consider changing these for the paper
downtown_rq[(downtown_rq$week >= base::as.Date("2021-12-06")) & (downtown_rq$week < base::as.Date("2022-03-07")), "Season"] = "Season_8"
downtown_rq[(downtown_rq$week >= base::as.Date("2022-03-07")) & (downtown_rq$week < base::as.Date("2022-06-13")), "Season"] = "Season_9"
downtown_rq[(downtown_rq$week >= base::as.Date("2022-06-13")) & (downtown_rq$week < base::as.Date("2022-08-29")), "Season"] = "Season_10"
downtown_rq[(downtown_rq$week >= base::as.Date("2022-08-29")) & (downtown_rq$week < base::as.Date("2022-12-05")), "Season"] = "Season_11"
downtown_rq[(downtown_rq$week >= base::as.Date("2022-12-05")) & (downtown_rq$week < base::as.Date("2023-03-07")), "Season"] = "Season_12"

downtown_rq %>%
  dplyr::filter((city %in% c("Washington DC", "Salt Lake City", "New York","San Francisco",
                             "El Paso", "Los Angeles", "San Diego", "Portland",
                             "Boston", "Chicago", "Vancouver", "Toronto")) &
                  (metric == "downtown") &
                  (week >= "2022-01-01"))%>%
  ggplot(aes(x = week, y = normalized_visits_by_total_visits)) +
  geom_line() +
  facet_wrap(.~city) +
  geom_vline(xintercept = as.Date("2022-05-02"), color = "purple")

ranking_df_safegraph <- read.csv("~/git/downtownrecovery/shinyapp/input_data/all_seasonal_metrics.csv") %>%
  dplyr::filter(city != "Hamilton")

ranking_df_safegraph %>% glimpse()

seasonal_rq <- downtown_rq %>%
  select(-source) %>%
  dplyr::filter(Season %in% c("Season_10", "Season_11", "Season_12")) %>%
  group_by(city, Season) %>%
  mutate(seasonal_average = mean(normalized_visits_by_total_visits, na.rm = TRUE)) %>%
  select(-week, -normalized_visits_by_total_visits) %>%
  distinct()

seasonal_rq %>% glimpse()

ranking_df <- rbind(seasonal_rq,
                    ranking_df_safegraph) %>%
  group_by(Season) %>%
  dplyr::arrange(-seasonal_average) %>%
  mutate(lq_rank = rank(-seasonal_average,
                        ties.method = "first")) %>%
  ungroup() 


ranking_df %>% glimpse()

ranking_df_prev <- read.csv("~/git/downtownrecovery/shinyapp/input_data/all_seasonal_metrics_cuebiq_update.csv")

ranking_df_prev %>% glimpse()

write.csv(ranking_df, "~/git/downtownrecovery/shinyapp/input_data/all_seasonal_metrics_cuebiq_update_hll.csv")



comparison_rankings <- rbind(ranking_df %>%
                               dplyr::filter(Season %in% c("Season_10", "Season_11")) %>%
                               mutate(source = "stoppers_hll"),
                             ranking_df_prev %>%
                               select(-X) %>%
                               dplyr::filter(Season %in% c("Season_10", "Season_11")) %>%
                               mutate(source = 'stops_uplevelled')
) %>%
  pivot_wider(names_from = 'source', values_from = c('seasonal_average', 'lq_rank')) %>%
  mutate(lq_rank_change = lq_rank_stoppers_hll - lq_rank_stops_uplevelled,
         seasonal_average_change = seasonal_average_stoppers_hll - seasonal_average_stops_uplevelled) %>%
  left_join(
    ranking_df %>%
      dplyr::filter(Season == "Season_12") %>%
      mutate(source = 'stoppers_hll') %>%
      pivot_wider(names_from = 'Season', values_from = c('seasonal_average', 'lq_rank')) %>%
      select(-source)) %>%
  arrange(city, Season)

comparison_rankings %>% glimpse()



comparison_rankings %>%
  select(city, Season, seasonal_average_stoppers_hll:seasonal_average_change) %>%
  arrange(-abs(seasonal_average_change))

comparison_rankings %>%
  select(city, Season, seasonal_average_stoppers_hll:seasonal_average_change) %>%
  arrange(-abs(lq_rank_change))

comparison_rankings %>%
  group_by(Season, lq_rank_change) %>%
  count() %>%
  arrange(Season, -n) %>%
  print(n = Inf)

write.csv(comparison_rankings, "~/data/downtownrecovery/comparisons/seasonal_metrics_stops_hll.csv")
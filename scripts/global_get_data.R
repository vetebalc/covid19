pacman::p_load(tidyverse, lubridate, countrycode)

# Global 

clean_jhd_to_long <- function(df) {
  df_str <- deparse(substitute(df))
  var_str <- substr(df_str, 1, str_length(df_str) - 4)
  
  df %>% 
    select(-`Province/State`, -Lat, -Long) %>%
    rename(country = `Country/Region`) %>%
    mutate(iso3c = countrycode(country,
                               origin = "country.name",
                               destination = "iso3c")) %>%
    select(-country) %>%
    filter(!is.na(iso3c)) %>%
    group_by(iso3c) %>%
    summarise_at(vars(-group_cols()), sum) %>% 
    pivot_longer(
      -iso3c, 
      names_to = "date_str", 
      values_to = var_str
    ) %>%
    ungroup() %>%
    mutate(date = mdy(date_str)) %>%
    select(iso3c, date, !! sym(var_str)) 
}

confirmed_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", col_types = cols())
deaths_raw <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", col_types = cols())

jh_covid19_data <- clean_jhd_to_long(confirmed_raw) %>%
  full_join(clean_jhd_to_long(deaths_raw), by = c("iso3c", "date")) #%>%

# jhd_countries <- tibble(
#   country = unique(confirmed_raw$`Country/Region`),
#   iso3c = countrycode(country,
#                       origin = "country.name",
#                       destination = "iso3c")
# ) %>% filter(!is.na(iso3c))

old_jhd_countries <- tibble(
  country = unique(confirmed_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")) %>% 
  filter(!is.na(iso3c),
             ! iso3c %in% jhd_countries$iso3c)

jhd_countries <- rbind(jhd_countries, old_jhd_countries)

jh_covid19_data %>%
  left_join(jhd_countries, by = "iso3c") %>%
  select(country, iso3c, date, confirmed, deaths) %>% 
  mutate_at("country", as.factor) -> jh_covid19_data

# saveRDS(jh_covid19_data, here::here("data", paste0("global_", Sys.Date(), ".rds")))

# Selected countries

data_start <- jh_covid19_data %>% pull(date) %>% min()
data_end <- jh_covid19_data %>% pull(date) %>% max()
n_days <- interval(data_start,data_end)/days(1)

# jh_covid19_data %>% pull(country) %>% unique()

jh_covid19_data %>% 
  filter(country %in% c("Argentina", "Italy", "Korea, South", "US","Spain")) %>% 
  mutate(days = as.numeric((date - data_start))) %>% 
  dplyr::filter(confirmed > 0) -> dglob

start_dataset <- dglob %>%   
  mutate(days = as.numeric((date - data_start))) %>% 
  group_by(country) %>% 
  summarise(onset = min(days))

dglob <- dglob %>% 
  dplyr::left_join(start_dataset) %>% 
  mutate(matched_days = days - onset, 
         country = recode_factor(country, 
         Italy = "Italia", `Korea, South` = "Korea del Sur", 
         US = "Estados Unidos", Spain = "España",
         .default = levels(country)),
         line_wdt = as.numeric(case_when(country == "Argentina" ~ 1, TRUE ~ 0.1))) %>% 
  droplevels()

# saveRDS(dglob, here::here("data", paste0("dglob_", Sys.Date(), ".rds")))
saveRDS(dglob, here::here("data", "global_last.rds"))

# Latam 
# all_data %>% pull(`Country/Region`) %>% unique()

jh_covid19_data %>% 
  filter(country %in% c("Argentina", "Brazil", "Chile", "Uruguay", "Paraguay", "Bolivia" )) %>% 
  mutate(days = as.numeric((date - data_start))) %>% 
  dplyr::filter(confirmed > 0) -> latam

start_dataset <- latam %>%   
  mutate(days = as.numeric((date - data_start))) %>% 
  group_by(country) %>% 
  summarise(onset = min(days))

latam <- latam %>% 
  dplyr::left_join(start_dataset) %>% 
  mutate(matched_days = days - onset) %>% droplevels()

# saveRDS(latam, here::here("data", paste0("latam_", Sys.Date(), ".rds")))
saveRDS(latam, here::here("data", "latam_last.rds"))


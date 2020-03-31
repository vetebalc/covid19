library(tidyverse)
library(lubridate)
library(countrycode)

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

jhd_countries <- tibble(
  country = unique(confirmed_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c))

old_jhd_countries <- tibble(
  country = unique(confirmed_raw$`Country/Region`),
  iso3c = countrycode(country,
                      origin = "country.name",
                      destination = "iso3c")
) %>% filter(!is.na(iso3c),
             ! iso3c %in% jhd_countries$iso3c)

jhd_countries <- rbind(jhd_countries, old_jhd_countries)

jh_covid19_data %>%
  left_join(jhd_countries, by = "iso3c") %>%
  select(country, iso3c, date, confirmed, deaths) -> jh_covid19_data

saveRDS(jh_covid19_data, here::here("data", paste0("global_", Sys.Date(), ".rds")))

# Selected countries

# data_start <- dglob1 %>% pull(date) %>% min()
# data_end <- dglob1 %>% pull(date) %>% max()
# n_days <- interval(data_start,data_end)/days(1)

# dglob %>% pull(country) %>% unique()

dglob %>% 
  filter(country %in% c("Argentina", "Italy", "Korea, South", "US", "Brazil")) %>% 
  mutate(days = as.numeric((date - data_start))) %>% 
  dplyr::filter(confirmed > 0) -> dglob1

start_dataset <- dglob1 %>%   
  mutate(days = as.numeric((date - data_start))) %>% 
  group_by(country) %>% 
  summarise(onset = min(days))

dglob1 <- dglob1 %>% 
  dplyr::left_join(start_dataset) %>% 
  mutate(matched_days = days - onset, 
         country = factor(country)) 

saveRDS(dglob1, here::here("data", paste0("dglob1_", Sys.Date(), ".rds")))

# Argentina 

url <- "https://raw.githubusercontent.com/SistemasMapache/Covid19arData/master/CSV/Covid19arData%20-%20historico.csv"

# Agrupamos por día y obtenemos el valor máximo, ya que en 
# estos datos acumulan por provincia
url %>%  
  read_csv(col_types = cols()) %>% 
  select(fecha, dia_inicio, tot_casosconf, tot_fallecidos) -> arg0
  
arg0 %>% 
  group_by(dia_inicio) %>% 
  summarize(fecha = first(fecha),
            tot_conf = max(tot_casosconf), 
            tot_muert = max(tot_fallecidos)) %>%
  mutate(
    new_confirmados = tot_conf - lag(tot_conf, default = first(tot_conf-1)),
    new_muertos = tot_muert - lag(tot_muert, default = first(tot_muert))) -> arg

saveRDS(arg, here::here("data", paste0("arg_", Sys.Date(), ".rds")))

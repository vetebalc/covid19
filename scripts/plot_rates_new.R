library(tidyverse)
library(lubridate)
library(directlabels)
library(scales)  

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

dglob <- readRDS("~/git_juan/covid19_bce/data/dglob_2020-03-31.rds")
# dglob <- readRDS(file = here::here("data", paste0("dglob_", Sys.Date(), ".rds")))
# write_csv(dglob1, "~/git_juan/covid19_bce/data/dglob1_2020-03-30.csv")

# dglob1 %>% pull(country) %>% unique()

data_start <- dglob %>% pull(date) %>% min()
data_end <- dglob %>% pull(date) %>% max()
n_days <- interval(data_start,data_end)/days(1)

ref <- tibble(fecha = data_start + days(0:n_days), 
              matched_days = row_number(fecha)) %>%
  mutate(confirmed_10 = 1*(1.1)^matched_days, 
         confirmed_20 = 1*(1.2)^matched_days, 
         confirmed_30 = 1*(1.3)^matched_days, 
         confirmed_40 = 1*(1.4)^matched_days) 

ks <- function (x) { number_format(accuracy =1,
                                   scale = 1/1000,
                                   suffix = "k",
                                   big.mark = ",")(x) }
ggplot2::theme_set(theme_bw(base_size = 10))

p_glob <- ref %>% 
  pivot_longer(cols = starts_with("confirmed"), 
               names_to = "var", values_to = "confirmed") %>% 
  separate(var, c(NA, "growth_rate")) %>% 
  mutate(growth_rate = factor(paste0("+", growth_rate,"%"))) %>%  
  ggplot(aes(x=matched_days, y=confirmed)) +
  geom_line(aes(group=growth_rate), linetype=2, size=0.5, col="grey70")+
  geom_dl(aes(label=growth_rate), method = list(box.color = NA, "angled.boxes",
  dl.combine("last.points"), cex = 0.7))+
  geom_line(data=dglob, aes(x=matched_days, y=confirmed, col=country))+
  scale_y_log10(limits = c(1,1e5), breaks=c(0,1,100,1000,10000),
                labels=c(0,1,100,1000,10000))+
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados", 
       subtitle = "Tasas de progreso diaria estan representadas por la inclinación\nde lineas punteadas representan  ")
p_glob  

# dglob1 %>%     
#   ggplot(aes(x=matched_days, y=log(confirmed)))+ 
#   map(1:4, ~ stat_function(fun = function (x) log((1+.x/10)^x), 
#                            aes(color = factor(.x)), linetype=2))+
#   scale_color_discrete(name = "Crecimiento\ndiario:",
#                      labels = c("+10%", "+20%", "+30%", "+40%"))+
#   geom_line(aes(group=country))+
#   geom_dl(aes(label=country), method = list(dl.trans(x = x - 0.3, y = y + 0.4),
#               dl.combine("last.points"), cex = 0.7)) +
#   # scale_y_reverse()+
#   # scale_y_continuous(labels = scales::unit_format(unit = "k"))+
#   labs(x="Días desde inicio de la epidemia")
  
arg <- readRDS("~/git_juan/covid19_bce/data/arg_2020-03-31.rds")

p_arg_conf <- arg %>% 
  ggplot(aes(x=dmy(fecha), 
             y = tot_conf, 
             label = paste0("+", new_confirmados)))+
  scale_x_date(breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) +
  labs(x="", y="", title="Casos confirmados en Argentina") +
  geom_point() + geom_smooth(se=F)+
  # hrbrthemes::theme_ipsum_rc()+
  ggrepel::geom_label_repel(
    force = 2,
    nudge_y = 3,
    direction = "y", 
    hjust = 0, 
    show.legend = FALSE, 
    size = 3, min.segment.length = 0.2) 
# p_arg_conf

p_arg_dead <- arg %>% 
  ggplot(aes(x=dmy(fecha), 
             y = tot_muert, 
             label = paste0("+", new_muertos)))+
  scale_x_date(breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) +
  labs(x="", y="", title="Fallecidos en Argentina") +
  geom_point() + geom_smooth(se=F)+
  # hrbrthemes::theme_ipsum_rc()+
  ggrepel::geom_label_repel(
    force = 2,
    nudge_y = 3,
    direction = "y", 
    hjust = 0, 
    show.legend = FALSE, 
    size = 3, min.segment.length = 0.2) 
# p_arg_dead

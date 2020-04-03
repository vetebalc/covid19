library(tidyverse)
library(lubridate)
library(directlabels)
library(scales)  
options(scipen = 999)
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)

# dglob <- readRDS("~/git_juan/covid19_bce/data/dglob_2020-03-31.rds")
dglob <- readRDS(file = here::here("data", paste0("dglob_", Sys.Date(), ".rds"))) %>% 
   mutate(country = fct_relevel(country, "Argentina"))
# dglob %>% pull(country) %>% unique()

# data_start <- dglob %>% pull(date) %>% min()
# data_end <- dglob %>% pull(date) %>% max()
# n_days <- interval(data_start,data_end)/days(1)

ggplot2::theme_set(theme_bw(base_size = 10))

ref <- tibble(fecha = data_start + days(0:n_days), 
              matched_days = row_number(fecha)) %>%
  mutate(confirmed_10 = 1*(1.1)^matched_days, 
         confirmed_20 = 1*(1.2)^matched_days, 
         confirmed_30 = 1*(1.3)^matched_days, 
         confirmed_40 = 1*(1.4)^matched_days) %>% 
  pivot_longer(cols = starts_with("confirmed"), 
               names_to = "var", values_to = "confirmed") %>% 
  separate(var, c(NA, "growth_rate")) %>% 
  mutate(growth_rate = factor(paste0("+", growth_rate,"%")))

p_glob <- ref %>% 
  ggplot(aes(x=matched_days, y=confirmed)) +
  geom_line(aes(group=growth_rate), linetype=2, size=0.5, col="grey70")+
  geom_dl(aes(label=growth_rate), method = list(box.color = NA, "angled.boxes",
  dl.combine("last.points"), cex = 0.7))+
  geom_line(data=dglob, 
            aes(x=matched_days, y=confirmed, col=country))+
  scale_color_viridis_d()+
  # scale_size_manual(guide=FALSE, values = c(0.5, rep(1, nlevels(dglob$country)-1)))+
  # scale_y_log10(limits = c(1,1e5), 
  #               breaks=c(0,1,100,1000,10000),
  #               labels=c(0,1,100,1000,10000))+
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados en escala logaritmica", 
       subtitle = "Tasas de progreso diaria estan representadas por la inclinación\nde lineas punteadas representan  ")
p_glob

p_glob_orig  <- dglob %>% 
  ggplot(aes(x=matched_days, y=confirmed)) +
  geom_line(aes(col=country), size=1.2)+
  # scale_size_manual(guide=FALSE, values = c(1, rep(1.2, nlevels(dglob$country)-1)))+
  scale_x_continuous(expand = c(0.1, 0)) +  
  # scale_y_continuous() +  
  scale_color_viridis_d()+
  scale_y_log10(limits = c(1,1e6), expand = c(0.1, 0),
                breaks=c(0,1,10,100,1000,10000,100000),
                labels=c(0,1,10,100,1000,10000,100000))+
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados (escala logarítmica)")+
  ggrepel::geom_text_repel(data=dglob  %>% 
              arrange(desc(confirmed)) %>% 
              group_by(country) %>% 
              slice(1), 
            aes(label=confirmed), size = 3, 
            position=position_nudge(0.2), hjust=0, show.legend=FALSE)
p_glob_orig

dglob %>% mutate(diff = confirmed / lag(confirmed, default = first(confirmed)))


dglob %>%
  mutate(diff = (confirmed / lag(confirmed, default = first(confirmed))-1)*100) %>% 
  filter(matched_days>1) %>% 
  ggplot(aes(x=matched_days, y=diff))+
  # map(1:4, ~ stat_function(fun = function (x) log((1+.x/10)^x),
  #                          aes(color = factor(.x)), linetype=2))+
  # scale_color_discrete(name = "Crecimiento\ndiario:",
  #                    labels = c("+10%", "+20%", "+30%", "+40%"))+
  geom_point(size=0.1,alpha=0.5) + 
  geom_smooth(se=F, span = 2)+ 
  facet_wrap(~country)+
  scale_y_continuous(limits = c(0,50))+#, breaks=c(0:2), labels=c(0:2))+
  # geom_dl(aes(label=country), method = list(dl.trans(x = x - 0.3, y = y + 0.4),
  #             dl.combine("last.points"), cex = 0.7)) +
  # scale_y_reverse()+
  # scale_y_continuous(labels = scales::unit_format(unit = "k"))+
  labs(x="Días desde inicio de la epidemia", y = "Incemento diario %")
  
latam <- readRDS(file = here::here("data", paste0("latam_", Sys.Date(), ".rds"))) %>% 
  mutate(country = fct_relevel(country, "Argentina"))


latam %>% pull(country) %>% unique()

p_latam <- ref %>% 
  ggplot(aes(x=matched_days, y=confirmed)) +
  geom_line(aes(group=growth_rate), linetype=2, size=0.5, col="grey70")+
  geom_dl(aes(label=growth_rate), method = list(box.color = NA, "angled.boxes",
                                                dl.combine("last.points"), cex = 0.7))+
  geom_line(data=latam, 
            aes(x=matched_days, y=confirmed, col=country))+
  scale_color_viridis_d()+
    # scale_size_manual(guide=FALSE, values = c(0.5, rep(1, nlevels(dglob$country)-1)))+
  scale_y_log10(limits = c(1,1e4), 
                breaks=c(0,1,100,1000),
                labels=c(0,1,100,1000))+
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados en escala logaritmica", 
       subtitle = "Tasas de progreso diaria estan representadas por la inclinación\nde lineas punteadas representan  ")

p_latam_orig  <- latam %>% 
  ggplot(aes(x=matched_days, y=confirmed)) +
  geom_line(aes(col=country))+
  scale_size_manual(guide=FALSE, values = c(0.5, rep(1, nlevels(dglob$country)-1)))+
  scale_x_continuous(expand = c(0.1, 0)) +  
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados en países Limitrofes") + 
  ggrepel::geom_text_repel(data=latam  %>% 
              arrange(desc(confirmed)) %>% 
              group_by(country) %>% 
              slice(1), 
            aes(label=confirmed), size = 3, 
            position=position_nudge(0.1), hjust=0, show.legend=FALSE)

difu_latam  <- latam %>% 
  ggplot(aes(x=matched_days, y=deaths)) +
  geom_line(aes(col=country))+
  scale_size_manual(guide=FALSE, values = c(0.5, rep(1, nlevels(dglob$country)-1)))+
  scale_x_continuous(expand = c(0.1, 0)) +  #               breaks=c(0,1,100,1000,10000),
  #               labels=c(0,1,100,1000,10000))+
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Difuntos") + 
  geom_text(data=latam  %>% 
              arrange(desc(deaths)) %>% 
              group_by(country) %>% 
              slice(1), 
            aes(label=deaths), size = 3, 
            position=position_nudge(0.1), hjust=0, show.legend=FALSE)

latam_long <- latam%>% 
  select(country, matched_days, `COVID positivos` = confirmed, Fallecidos=deaths)%>%  
  pivot_longer(-(country:matched_days), names_to = "var", values_to = "val") 
  # group_by(var) %>% 
  # mutate(new_val = val - lag(val, default = first(val-1))) %>% 
latam_long %>% 
  ggplot(aes(matched_days, val))+ 
  geom_line(aes(col=country))+
  scale_size_manual(guide=FALSE, values = c(0.5, rep(1, nlevels(dglob$country)-1)))+
  scale_x_continuous(expand = c(0.1, 0)) +  
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados en países Limitrofes") + 
  facet_wrap(~var, scales = "free_y", ncol=1)+
  ggrepel::geom_text_repel(data=latam_long %>% 
                           arrange(desc(val)) %>% 
                           group_by(country, var) %>% 
                           slice(1), 
                         aes(label=val), size = 3, 
                         position=position_nudge(0.1), hjust=0, show.legend=FALSE)-> p_latam 

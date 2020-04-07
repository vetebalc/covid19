pacman::p_load(tidyverse, lubridate, countrycode, directlabels, scales)  

source("_site.R") # especificaciones globales
source('scripts/get_data_global.R')

dglob <- readRDS(file = here::here("data", "global_last.rds")) %>% 
   mutate(country = fct_relevel(country, "Argentina"))

# dglob %>% pull(country) %>% unique()

# data_start <- dglob %>% pull(date) %>% min()
# data_end <- dglob %>% pull(date) %>% max()
# n_days <- interval(data_start,data_end)/days(1)

ggplot2::theme_set(theme_bw(base_size = 10))

p_glob  <- dglob %>% 
  ggplot(aes(x=matched_days, y=confirmed)) +
  geom_line(aes(col=country), size=1.2)+
  # scale_size_manual(guide=FALSE, values = c(1, rep(1.2, nlevels(dglob$country)-1)))+
  scale_x_continuous(expand = c(0.1, 0)) +  
  # scale_y_continuous() +  
  scale_color_discrete()+
  scale_y_log10(limits = c(1,1e6), #expand = c(0.1, 0),
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
p_glob

ggsave(here::here("plots", "global_log.jpg"), width=6, height=4, units="in", dpi=300)

p_growthF <- dglob %>%
  mutate(growth_factor = (confirmed / lag(confirmed, default = first(confirmed))-1)*100) %>% 
  filter(matched_days>1) %>% 
  ggplot(aes(x=matched_days, y=growth_factor))+
  geom_point(size=0.1,alpha=0.5) + 
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3))+ # Cubic Spline
  facet_wrap(~country)+
  scale_y_continuous(limits = c(0,50))+
  labs(x="Días desde inicio de la epidemia", y = "Incemento diario %", 
       title = bquote("Factor de crecimiento =" ~ "Nuevos casos" [t] / "Casos Acumulados"[t-1]))
# p_growthF

ggsave(here::here("plots", "global_GF.jpg"), width=6, height=4, units="in", dpi=300)

latam <- readRDS(file = here::here("data", "latam_last.rds")) %>% 
  mutate(country = fct_relevel(country, "Argentina"))

# latam %>% pull(country) %>% unique()

latam_long <- latam %>% 
  dplyr::select(country, matched_days, `COVID positivos` = confirmed, Fallecidos=deaths)%>%  
  pivot_longer(-(country:matched_days), names_to = "var", values_to = "val") 
  # group_by(var) %>% 
  # mutate(new_val = val - lag(val, default = first(val-1))) %>% 

p_latam <- latam_long %>% 
  ggplot(aes(matched_days, val))+ 
  geom_line(aes(col=country),size=1.2)+
  scale_size_manual(guide=FALSE, values = c(0.5, rep(1, nlevels(dglob$country)-1)))+
  scale_x_continuous(expand = c(0.1, 0)) +  
  scale_color_discrete()+
  labs(x="Días desde inicio de la epidemia", col= NULL, y = "",
       title = "Casos confirmados en países Limitrofes") + 
  facet_wrap(~var, scales = "free_y", ncol=1)+
  ggrepel::geom_text_repel(data=latam_long %>% 
                           arrange(desc(val)) %>% 
                           group_by(country, var) %>% 
                           slice(1), 
                         aes(label=val), size = 3, 
                         position=position_nudge(0.1), hjust=0, show.legend=FALSE)  
# p_latam

ggsave(here::here("plots", "latam_lineal.jpg"), width=6, height=4, units="in", dpi=300)

p_growthF_latam <- latam %>%
  mutate(growth_factor = (confirmed / lag(confirmed, default = first(confirmed))-1)*100) %>% 
  filter(matched_days>1) %>% 
  ggplot(aes(x=matched_days, y=growth_factor))+
  geom_point(size=0.1,alpha=0.5) + 
  geom_smooth(method = "lm", formula = y ~ splines::bs(x, df = 3))+ # Cubic Spline
  facet_wrap(~country)+
  scale_y_continuous(limits = c(0,50))+
  labs(x="Días desde inicio de la epidemia", y = "Incemento diario %", 
       title = bquote("Factor de crecimiento =" ~ "Nuevos casos" [t] / "Casos Acumulados"[t-1]))
# p_growthF_latam

ggsave(here::here("plots", "latam_GF.jpg"), width=6, height=4, units="in", dpi=300)



source("_site.R") # especificaciones globales
source('scripts/get_data_arg.R')

arg <- readRDS(here::here("data", "arg_last.rds")) %>%
  mutate(dias =as.numeric( fecha - min(fecha)))


ARG_bars <- arg %>% 
  dplyr::select(fecha, `COVID positivos` = casos, Fallecidos=fallecidos)%>%  
  pivot_longer(-fecha, names_to = "var", values_to = "val") %>%
  ggplot(aes(fecha, val))+ 
  geom_bar(stat="identity", fill = "steelblue") +
  geom_text(aes(label=val), angle=90,size=3, fontface = 2,
            position = position_dodge(0.9), 
            vjust=0.7, hjust = 1.2, color="white")+
  facet_wrap(~var, scales = "free_y", ncol=1)+
  scale_x_date(limits = c(Sys.Date() - dias_epidemia, Sys.Date()),expand = c(0, 0),
               breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m"))+
  labs(y="", x="",
       title = "Casos confirmados en Argentina", 
       subtitle = paste("Datos disponibles al",format(as.Date(Sys.Date(), format = "%Y%m%d"), "%d/%m/%y")))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ARG_bars

ggsave(here::here("plots", "ARG_bars.jpg"), width=6,height=6,units="in",dpi=150)
# ARG_points <- arg %>% 
#   dplyr::select(fecha, `COVID positivos` = casos, Fallecidos=fallecidos)%>%  
#   pivot_longer(-fecha, names_to = "var", values_to = "val") %>%
#   mutate(var = as.factor(var)) %>%
#   ggplot(aes(fecha, val, col = var))+ 
#   geom_point() + geom_line()+
#  # geom_text(aes(label=val), angle=90,size=2, 
#  #            position = position_dodge(0.9), 
#  #            vjust=0.7, hjust = 1.2, color="white")+
#   # facet_wrap(~var, scales = "free_y", ncol=1)+
#   scale_x_date(limits = c(Sys.Date() - dias_epidemia, Sys.Date()),expand = c(0, 0),
#                breaks = "2 days", minor_breaks = "1 day", 
#                labels=scales::date_format("%d/%m"))+
#   labs(y="", x="")+
#   theme(axis.text.x = element_text(angle = 60, hjust = 1))
# ARG_points


# "https://raw.githubusercontent.com/lsaravia/covid19ar/master/coronavirus_ar.csv"%>%
#   read_csv(col_types = cols()) -> saravia
# 
# long <- saravia %>%
#   dplyr::select(fecha, importados, contactos , comunitarios) %>% 
#   pivot_longer(-fecha, names_to="tipo_contagio", values_to = "n") %>%
#   filter(tipo_contagio %in% c("contactos","importados","comunitarios")) %>%
#   mutate(n = ifelse(n==0,NA,n))
# 
# ggplot(long,aes(x=ymd(fecha),y=n,color=tipo_contagio)) +
#   geom_line() + theme_bw() +
#   scale_color_viridis_d(name="Tipo de infección") +
#   scale_y_log10() + ylab("Casos")
# 
# ggsave(here::here("plots", "tipo_infeccion_ARG.jpg"), width=6, height=4, units="in", dpi=300)

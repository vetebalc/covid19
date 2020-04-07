source('~/git_juan/covid19/scripts/get_data_arg.R')

arg <- readRDS(here::here("data", "arg_last.rds")) %>%
  mutate(dias =as.numeric( fecha - min(fecha)))

arg %>% 
  dplyr::select(fecha, `COVID positivos` = casos, Fallecidos=fallecidos)%>%  
  pivot_longer(-fecha, names_to = "var", values_to = "val") %>%
  group_by(var) %>% 
  mutate(new_val = val - lag(val, default = first(val-1))) %>% 
  ungroup %>% 
  mutate(var = as.factor(var)) -> arg_long

p_ARG <- arg_long %>% 
  ggplot(aes(x=fecha, y = val))+ 
  geom_bar(stat="identity") +   
  facet_wrap(~var, scales = "free_y", ncol=1)+
  # stat_bin(aes(label=..count..), vjust=0,  geom="text", position="identity")+
  # geom_bar(aes(y=val))+
  # geom_text(stat='count', aes(label=..count..),vjust=-1)+
  # geom_bar(stat = "identity") +
  # geom_text(stat='identity', aes(label=..count..), vjust=-1)+

  # geom_text(aes(fecha, y= 0, label=paste0("+", new_val)),
  #           size=3, check_overlap = FALSE)+
  ggrepel::geom_text_repel(data=. %>%
                             arrange(desc(val)) %>%
                             group_by(var) %>%
                             slice(1),
                           aes(label=val), size = 3,
                           position=position_nudge(0.1), hjust=1, show.legend=FALSE)+
  scale_x_date(breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) +
  labs(y="", x="")  
p_ARG



ggplot(mtcars, aes(x = gear)) + 
  geom_bar()+ 
  geom_text(stat='count',aes(label=..count..),vjust=-1)+
  facet_wrap(~cyl, scales = "free_y", ncol=1)
  



ggsave(here::here("plots", "p_ARG.jpg"), width=6,height=6,units="in",dpi=150)

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

pacman::p_load(tidyverse, lubridate, countrycode)

#Sistemas mapache
# url <- "https://raw.githubusercontent.com/SistemasMapache/Covid19arData/master/CSV/Covid19arData%20-%20historico.csv"
# url %>%
#   read_csv(col_types = cols()) %>%
#   dplyr::select(fecha, dia_inicio, tot_casosconf, tot_fallecidos) -> arg0
# 
# arg0 %>%
#   group_by(dia_inicio) %>%
#   summarize(fecha = first(fecha),
#             tot_conf = max(tot_casosconf),
#             tot_muert = max(tot_fallecidos)) %>%
#   mutate(
#     new_confirmados = tot_conf - lag(tot_conf, default = first(tot_conf-1)),
#     new_muertos = tot_muert - lag(tot_muert, default = first(tot_muert))) -> arg
# 
# saveRDS(arg, here::here("data", paste0("arg_", Sys.Date(), ".rds")))
# 
# #Datos libres
# arg_dlibres_url <- "https://raw.githubusercontent.com/datoslibres/covid19argentina/master/datos/covid19_por_provincia_argentina.csv"
# arg_dlibres_url %>%  
#   read_csv(col_types = cols()) -> arg_dlibres0
# 
# arg_dlibres <- arg_dlibres0 %>% 
#   mutate(fecha = dmy(fecha)) %>% 
#   arrange(provincia, fecha) %>% 
#   group_by(provincia)%>%  
#   summarise(confir = sum(confirmados))
# 
# arg_dlibres <- arg_dlibres %>% 
#   mutate(
#     nombre = recode(provincia, 
#                        CABA ="Ciudad Autónoma de Buenos Aires",
#                        Cordoba ="Córdoba", 
#                        Tucuman = "Tucumán")) %>% 
#   select(-provincia, nombre, confir)
# 
# arg_dlibres %>% pull(provincia) %>% unique() %>% View
# 
# library(sf)
# argentina_prov <- st_read("https://github.com/Demzayat/varios/raw/master/provinciasND.geojson")
# # argentina_prov %>% pull(nombre) %>% unique()
# argentina_prov <- argentina_prov %>% 
#   left_join(arg_dlibres, by="nombre")
# # identical(argent %>% pull(provincia) %>% unique(), 
# #           arg3 %>% pull(provincia) %>% unique())
# 
# ggplot(argentina_prov)+
#   geom_sf(aes(fill = confir))+
#   coord_sf(xlim = c(-52, -74), ylim = c(-20, -56), expand = FALSE)+
#   labs(title = "",
#        subtitle = "",
#        caption = "")+
#   theme_void()

# Datos Aliaga
library(googlesheets)
url_aliaga <- "https://docs.google.com/spreadsheets/d/1M7uDgWSfy6z1MNbC9FP6jTNgvY7XchJ0m-BfW88SKtQ/edit?usp=sharing"
gsheet::gsheet2tbl(url_aliaga)
gs_url(url_aliaga) %>% gs_read(skip = 18) -> alia

arg  <- alia %>% 
  dplyr::select(Fecha, Casos=X2,	Fallecidos=X3,	Recuperados=X4,	Terapia=X5, 
         Negativos, Totales, decartados_epi = X8, Importados, 
         `Contacto estrecho / Conglomerado`, `Transmisión Comunitaria`)  %>% 
  janitor::clean_names() %>% 
  mutate_at("fecha", dmy) %>% 
  mutate(new_positivos = casos - lag(casos, default = first(casos-1)),
         new_fallecidos = fallecidos - lag(fallecidos, default = first(fallecidos)))

saveRDS(arg, here::here("data", "arg_last.rds"))
# saveRDS(arg, here::here("data", paste0("arg_aliaga_", Sys.Date(), ".rds")))

# arg_alia %>% 
#   select(fecha, importados, contacto_estrecho_conglomerado, transmision_comunitaria)%>% 
#   pivot_longer(-fecha, names_to = "var", values_to = "val") %>% 
#   ggplot(aes(fecha, val)) +
#   geom_area(aes(fill = var))

  
  
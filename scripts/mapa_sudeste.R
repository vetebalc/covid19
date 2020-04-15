pacman::p_load(tidyverse, sf)

url_muni <- "https://raw.githubusercontent.com/SistemasMapache/Covid19arData/master/CSV/Covid19arData%20-%20Prov_BSAS.csv"
url_muni %>%
  read_csv(col_types = cols()) %>% 
  mutate(NAM=Municipio) -> bsas

muni <- st_read("kml/muni/municipio.shp", quiet = T) 

SEBA <- muni %>% 
  cbind(., st_coordinates(st_centroid(muni$geometry))) %>% 
  filter(between(Y, -40, -36), between(X, -61.5, -56)) %>% 
  dplyr::select(NAM, X,Y,geometry) %>% 
  left_join(bsas, by="NAM")

# ggplot(SEBA) + 
#   geom_sf()+
#   geom_text(aes(x=X, y=Y, label= NAM), size = 3,
#             color = "darkblue", check_overlap = TRUE)

map1 <- ggplot(SEBA) +
  geom_sf(aes(fill=Casos))+
  scale_fill_gradient2(
                     low = 'green2',
                       mid = 'yellow',
                       high = 'red3',
                       na.value = 'gray95',)+
  geom_text(aes(x=X, y=Y, label= NAM), size = 3,
            color = "darkblue", check_overlap = TRUE)+
  labs(title = "Casos confirmados por municipio", 
       subtitle = paste("Datos disponibles al",format(as.Date(Sys.Date(), format = "%Y%m%d"), "%d/%m/%y")))+
  theme_void()  

# map1
ggsave(here::here("plots", "mapa.jpg"), width=6,height=5,units="in",dpi=200)

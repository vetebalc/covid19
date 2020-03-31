#install.packages('leaflet')

library(leaflet)
library(tidyverse)
library(shiny)
library(plotly)

######## URL CON DATA

url='https://raw.githubusercontent.com/SistemasMapache/Covid19arData/master/CSV/Covid19arData%20-%20historico.csv'
df=read.csv(url,stringsAsFactors = F,header = T)


####### REGIONES

regiones=data.frame(
  prov=c('Corrientes', 'Entre Ríos', 'Chaco', 'Misiones','Formosa',
         'salta', 'Tucumán', 'Jujuy', 'Catamarca', 'Santiago del estero' ,'la rioja',
         'río negro', 'santa cruz', 'Chubut', 'neuquén','tierra del fuego',
         'buenos aires', 'córdoba', 'la pampa', 'entre ríos','santa fe',
         'mendoza', 'san juan','san luis','CABA'),
  reg=c('NOROESTE','NOROESTE','NOROESTE','NOROESTE','NOROESTE',
        'NORDESTE', 'NORDESTE', 'NORDESTE', 'NORDESTE', 'NORDESTE' ,'NORDESTE',
        'PATAGONIA', 'PATAGONIA', 'PATAGONIA', 'PATAGONIA','PATAGONIA',
        'CENTRO', 'CENTRO', 'CENTRO', 'CENTRO','CENTRO',
        'CUYO', 'CUYO','CUYO','CENTRO'),
  stringsAsFactors = F)

regiones$prov=toupper(regiones$prov)

############ UNION DE DF CON REGIONES

df=df%>%
  mutate(osm_admin_level_4=toupper(osm_admin_level_4))%>%
  left_join(regiones,by=c('osm_admin_level_4'='prov'))%>%
  mutate(reg=ifelse(is.na(reg)==T,'SIN INFORMACION',reg),
         transmision_tipo=ifelse(transmision_tipo=='','SIN INFORMACION',
                                 transmision_tipo))


df_cant=df%>%count(transmision_tipo)

df_cant_prov=df%>%
  group_by(transmision_tipo,osm_admin_level_4)%>%
  summarise(tot=sum(nue_casosconf_diff)) 

df_reg=df%>%
  group_by(reg,transmision_tipo)%>%
  summarise(tot=sum(nue_casosconf_diff)) 



###### GRAFICOS

# CASOS POR REGIONES Y TIPO DE TRANSMISION
region_g=ggplot(df_reg,
              aes(x=reg,y=tot,fill=transmision_tipo))+
  geom_col(color='black',show.legend = F)+
  coord_flip()+
  theme_bw()+
  labs(x='',y='cantidad',caption='Eant')

ggplotly(region_g) 

# BOXPLOT POR REGION
region=ggplot(df%>%filter(nue_casosconf_diff>0),
              aes(x=reg,y=nue_casosconf_diff,fill=reg))+
  geom_boxplot(color='black',show.legend = F)+
  coord_flip()+
  theme_bw()+
  labs(x='',y='cantidad',caption='Eant')

ggplotly(region) %>% layout(showlegend = FALSE)


# BOXPLOT POR PROVINCIA
prov=ggplot(df_cant_prov,
            aes(x=osm_admin_level_4,y=tot,fill=osm_admin_level_4))+
  geom_col(color='black',show.legend = F)+
  coord_flip()+
  theme_bw()+
  labs(x='',y='total',caption='Eant')

ggplotly(prov) %>% layout(showlegend = FALSE)
 
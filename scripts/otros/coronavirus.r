# COVID-19
#
# Estimación de los parametros de una exponecial/ logistica,  el factor de crecimiento
# También para los casos de contacto directo
#
# Fuente @msalnacion 
#

require(dplyr)
require(readr)
require(lubridate)
cor <- read_csv("/home/leonardo/Academicos/GitProjects/covid19/coronavirus_ar.csv") %>% dplyr::select(fecha:comunitarios)
cor <- cor  %>% mutate(fecha=ymd(fecha), dias =as.numeric( fecha - min(fecha))) 
#
# OJO, los casos en estudio = ? comunitarios no estan acumulados
#

require(ggplot2)

# g0 <- ggplot(cor,aes(x=dias,y=casos)) + geom_point() + theme_bw() + geom_smooth(method="glm",family=gaussian(link="log")) 
# g1 <- g0 + expand_limits(x=c(0,240))+
#   geom_smooth(method="glm",family=gaussian(link="log"),
#               fullrange=TRUE)

# Comparación de distintos modelos utilizando el criterio de Akaike (lineal y exponencial nomas)
#
expmdl <- lm(log(casos)~ dias,data=cor)
summary(expmdl)
linmdl <- lm(casos~ dias,data=cor)
summary(linmdl)
AIC(linmdl,expmdl)

# Ajuste no-lineal del los parametros del modelo exponencial
#
model <- nls(casos ~ alpha * exp(beta * dias) , data =cor  , start=list(alpha=0.6,beta=0.4))
# Ajuste del modelo logístico
require(drc)
#
# f(x) = d / (1+exp(b(x - e)))
#
model1 <- drm(casos ~ dias, fct=L.3() , data = cor)
summary(model1)

AIC(model,model1)

# Despues del dia 21 El ajuste del modelo logístico es mejor 
# En el dia 21 todavia ajustaba mejor el modelo exponencial
#

# Extraigo los coeficientes para ponerlos en el gráfico
#
model <- nls(casos ~ alpha * exp(beta * dias) , data = filter(cor, dias<22)  , start=list(alpha=0.6,beta=0.4))

a <- round(coef(model)[1],2)
b <- round(coef(model)[2],2)
summary(model)
# Tiempo de duplicacion
#
tau <-  round(log(2)/b,2)

# Prediccion hasta 12/04
#
ldia <- as.numeric(ymd("2020-04-12") - min(cor$fecha))
predexp <-data.frame(pred=predict(model,newdata=data.frame(dias=0:ldia)),predlog = predict(model1,newdata=data.frame(dias=0:ldia))) %>% mutate(dias=0:ldia, fecha=min(cor$fecha)+dias)
predexp
# Estimación de R0
#
#
# Generation time from
# https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus
# 
require(R0)
mGT<-generation.time("gamma", c(5.2, 1.5))
est.R0.EG(cor$casosdia,mGT,begin = 1,end=22)

tl <- est.R0.ML(cor$casosdia,mGT,begin = 1,end=22)
#plotfit(tl)
r <- round(tl$R,2)
r0 <- round(tl$conf.int,2)

col <- viridisLite::viridis(2)
# Casos totales
#
ggplot(cor, aes(x = fecha, y = casos) ) +
  geom_point() +
#  geom_ribbon( aes(ymin = lwr, ymax = upr), alpha = .15) +
  geom_line(data=predexp, aes(x=fecha,y = pred), size = .5, color= col[1]) + 
  geom_line(data=predexp, aes(x=fecha,y = predlog), size = .5, color= col[2]) +  theme_bw() + 
  annotate("text",x=ymd("20200330"), y=1, label="Fuente @msalnacion\n by @larysar",color="red",size=2) + scale_y_log10() + 
  annotate("text", x=ymd("20200325"), y=3,label=paste("R0 =", r, "[", r0[1], ",", r0[2],"]"),size=3) + 
  annotate("text", x=ymd("20200325"), y=2,label=paste("Tiempo de duplicación =", tau),size=3)

ggsave("/home/leonardo/Academicos/GitProjects/covid19/coronaArTotalesLog.jpg",width=6,height=6,units="in",dpi=600)

# Growth Factor para casos totales
#
cor <- cor %>% mutate(delta=casos-lag(casos),deltaPrev=lag(delta),growthFactor= delta/deltaPrev)

ggplot(cor %>% filter(dias>3),aes(x=dias,y=growthFactor)) + geom_point() + theme_bw() + stat_smooth(method=lm,se=FALSE) +   labs(title = bquote("Growth Factor=" ~ Delta* N[t] / Delta* N[t-1] ))  + theme_bw() + annotate(geom="text", x=5, y=8, label="Fuente @msalnacion\n by @larysar",color="red",size=2)

# Nuevos casos vs casos totales en log
#

predexp <- predexp %>% mutate( preddia = pred - lag(pred))

ggplot(cor, aes(x = casos, y = casosdia) ) +
  geom_point() + geom_line() + scale_x_log10() + scale_y_log10() + theme_bw() + ylab("Nuevos casos") + xlab("Casos totales")+
  geom_line(data=predexp, aes(x=pred, y = preddia), size = .5, color= "blue") + coord_cartesian(xlim=c(9,max(predexp$pred)))+ 
  annotate(geom="text", x=1000, y=1, label="Fuente @msalnacion\n by @larysar",color="red",size=2)

ggsave("/home/leonardo/Academicos/GitProjects/covid19/coronaArNuevosVsTotales.jpg",width=6,height=6,units="in",dpi=600)

#  geom_line(data=predexp, aes(x=fecha,y = pred), size = .5, color= "blue") 
  


#
# Contactos e Importados Comunitarios by group usando tidyverse
#
require(tidyr)
cor1 <- cor %>% gather(tipo,N,casos:comunitarios,importados) %>% filter(tipo %in% c("contactos","importados","comunitarios")) %>% mutate(N = ifelse(N==0,NA,N))

ggplot(cor1 ,aes(x=dias,y=N,color=tipo)) + geom_point() + theme_bw() + stat_smooth(method=lm,se=F) + scale_y_log10() + scale_color_viridis_d()

ggplot(cor1,aes(x=dias,y=N,color=tipo)) + geom_point() + theme_bw() + scale_color_viridis_d() + scale_color_viridis_d() + scale_y_log10() + ylab("Casos")

mod <- cor1 %>% filter(N>0) %>% group_by(tipo) %>% do(mod=nls(N~ alpha*exp(dias*beta),start=c(alpha=1.4,beta=0.6),data=.) )
mod  %>% do(data.frame(
  var = names(coef(.$mod)),tau = log(2)/coef(.$mod)[2],
  coef(summary(.$mod))) 
)
newdat <- data.frame(dias = 0:26)
library(tidyverse)  
predexp <- cor1 %>%
  group_by(tipo) %>%
  nest %>%
  mutate(mod  = purrr::map(.x = data, .f = ~ nls(N~ alpha*exp(dias*beta),start=c(alpha=1.4,beta=0.6),data=.))) %>%
  mutate(pred = purrr::map(.x = mod, ~ predict(., newdat))) %>% 
  dplyr::select(tipo,pred) %>% unnest %>% cbind(newdat = newdat) %>% mutate(fecha = min(cor1$fecha)+dias)


ggplot(cor1,aes(x=fecha,y=N,color=tipo)) + geom_point() + theme_bw() + scale_color_viridis_d() + scale_color_viridis_d() + ylab("Casos") + geom_line(data=predexp, aes(x=fecha,y = pred,color=tipo), size = .5) 
ggsave("/home/leonardo/Academicos/GitProjects/covid19/coronaArComparacion.jpg",width=6,height=6,units="in",dpi=600)

ggplot(cor1,aes(x=fecha,y=N,color=tipo)) + geom_point() + theme_bw() + scale_color_viridis_d() + scale_color_viridis_d() + scale_y_log10() + ylab("Casos") + geom_line(data=predexp, aes(x=fecha,y = pred,color=tipo), size = .5) 
ggsave("/home/leonardo/Academicos/GitProjects/covid19/coronaArComparacionLog.jpg",width=6,height=6,units="in",dpi=600)



# 
# Comparacion Casos GLobales Tasa de crecimiento vs Casos Totales 
#
# Fuente:  Johns Hopkins University Center for Systems Science and Engineering.
#          https://systems.jhu.edu/research/public-health/ncov/
#
#

corg <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 

corg <- corg %>% rename(country="Country/Region",province="Province/State")  %>% filter( country %in% c("US","Italy","Germany", "Korea, South"))
require(tidyr)

# Umbral de casos a partir de los cuales se calcula la curva
#
umbral <- 100

cor1 <- corg %>% gather(date,N,5:ncol(corg) ) %>% arrange(country) %>% mutate(casosdia = N - lag(N)) %>%  filter(N>umbral, casosdia>0 ) %>% group_by(country) %>% mutate(fecha=mdy(date), dias =as.numeric( fecha - min(fecha))) 

# Para argentina usa los datos de @minsal 
#
cor1 <- bind_rows( cor1, cor %>% filter(casos>umbral) %>%dplyr::select(casos,casosdia,fecha) %>% mutate(country="Argentina",dias =as.numeric( fecha - min(fecha))) %>% rename(N=casos))

require(ggplot2)

ggplot(cor1, aes(x = N, y = casosdia, colour=country) ) + scale_y_log10() +  scale_x_log10() + 
  geom_point() +  theme_bw() +  guides(fill=FALSE) + scale_color_viridis_d() + geom_line() + xlab("Casos Totales") + ylab( "Casos por Día") + 
  annotate("text",x=1850, y=10, label="Fuente https://systems.jhu.edu/research/public-health/ncov/\n by @larysar",color="red",size=2) 
ggsave("/home/leonardo/Academicos/GitProjects/covid19/coronaGlobalNuevosVsTotales.jpg",width=6,height=6,units="in",dpi=600)

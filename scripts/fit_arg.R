# Comparación de distintos modelos utilizando el criterio de Akaike (lineal y exponencial nomas)
#
mod_exp <- lm(log(casos)~ dias,data=arg)
# summary(mod_exp)
mod_lin <- lm(casos~ dias,data=arg)
# summary(mod_lin)
AIC(mod_lin, mod_exp)

# Ajuste no-lineal del los parametros del modelo exponencial
#
mod_exp1 <- nls(casos ~ alpha * exp(beta * dias),
                data =arg,
                start=list(alpha=0.6,beta=0.4))

# Ajuste del modelo logístico
require(drc)
#
# f(x) = d / (1+exp(b(x - e)))
#
mod_logis <- drm(casos ~ dias, fct=L.3() , data = arg_alia)
# summary(mod_logis)
AIC(mod_exp1, mod_logis)

# Despues del dia 21 El ajuste del modelo logístico es mejor
# En el dia 21 todavia ajustaba mejor el modelo exponencial

# Extraigo los coeficientes para ponerlos en el gráfico
model <- nls(casos ~ alpha * exp(beta * dias) ,
             data = filter(arg_alia, dias<22),
             start=list(alpha=0.6,beta=0.4))

a <- round(coef(model)[1],2)
b <- round(coef(model)[2],2)
# summary(model)
# Tiempo de duplicacion
#
tau <-  round(log(2)/b,2)

ldia <- as.numeric(ymd("2020-04-12") - min(arg_alia$fecha))
predmod <- data.frame(exponencial=predict(model, newdata=data.frame(dias=0:ldia)),
                      logistico = predict(mod_logis, newdata=data.frame(dias=0:ldia))) %>%
  mutate(dias=0:ldia, fecha=min(arg_alia$fecha)+dias)
predmod

# Estimación de R0
# Generation time from
# https://github.com/midas-network/COVID-19/tree/master/parameter_estimates/2019_novel_coronavirus

require(R0)
mGT<-generation.time("gamma", c(5.2, 1.5))
est.R0.EG(arg_alia$new_positivos, mGT, begin = 1, end=22)

tl <- est.R0.ML(arg_alia$new_positivos, mGT, begin = 1, end=22)
# plotfit(tl)
r <- round(tl$R,2)
r0 <- round(tl$conf.int,2)

col <- viridisLite::viridis(2)
# Casos totales
devtools::source_url("https://github.com/juanchiem/R-sources/blob/master/theme_juan.R?raw=TRUE")

model_comparison <- ggplot(arg_alia, aes(x = fecha, y = casos) ) +
  geom_point() +
  geom_line(data=predmod %>%
              pivot_longer(-(dias:fecha), names_to = "Modelo", values_to = "predicted"),
            aes(x=fecha, y = predicted, color=Modelo), size = .5) +
  scale_color_discrete( labels = c("Exponencial (AIC = 321.6)",
                                   "Logístico (AIC = 248.3)"))+
  scale_y_log10() +
  annotate("text", x=ymd("2020-03-16"), y=1600,label=paste("R0 =", r, "[", r0[1], ",", r0[2],"]"),size=3) +
  annotate("text", x=ymd("2020-03-16"), y=900,label=paste("Tiempo de duplicación =", tau),size=3)+
  scale_x_date(breaks = "5 days", minor_breaks = "1 day", expand = c(0.1,0.1),
               labels=scales::date_format("%d/%m")) +
  labs(y="Infectados COVID19 (escala logaritmica)", x="") +
  theme_juan(9, "top")+
  geom_vline(xintercept = ymd("2020-03-24"), linetype=2)
model_comparison
ggsave(here::here("plots", "models_ARG.jpg"), width=6, height=4, units="in", dpi=300)


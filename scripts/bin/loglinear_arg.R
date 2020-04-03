library(tidyverse)
library(lubridate)
# https://www.dummies.com/education/economics/econometrics/econometrics-and-the-log-linear-model/

url <- "https://docs.google.com/spreadsheets/d/175RVn2cHCK8nOJPJiXR-iuFP4taC-WbE-DIR5xJSj74/edit?usp=sharing"
arg <- gsheet::gsheet2tbl(url)

# arg %>% ggplot(aes(confirmados)) + geom_histogram()

ARG_dat <- arg %>% 
  mutate(date=as_date(fecha),
         dia = row_number(), 
         Lconf = log(confirmados))

ARG_dat1  <-  arg %>% filter(muertos>0) %>% 
  mutate(fecha=as_date(fecha),
         dia = row_number(), 
         Ldec = log(muertos))

arg %>% 
  pivot_longer(-date, names_to = "type", values_to = "val") %>% 
  ggplot(aes(x=as_date(date), y = val))+
  geom_point()+ labs(x="", y ="")+
  facet_wrap(~type, ncol=1, scales="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_x_date(breaks = "1 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) 
  
## Fitting Log-linear model

#Scatter Plot
p1 <- ARG_dat %>% 
  ggplot(aes(x=as_date(fecha), 
             y = cumsum(confirmados), 
             label = paste0("+", confirmados)))+
  scale_x_date(breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) +
  labs(x="", y="", title="Casos reportados en Argentina al 22/3") +
  geom_point() + geom_smooth(se=F)+
  # hrbrthemes::theme_ipsum_rc()+
  ggrepel::geom_label_repel(
    force = 2,
    nudge_y = 3,
    direction = "y", 
    hjust = 0, 
    show.legend = FALSE, 
    size = 3, min.segment.length = 0.2) 
p1

mod1 = lm(Lconf ~ dia, data=ARG_dat)
summary(model1)

library(tidypredict)
tidypredict_fit(mod1)
library(modelr)

# 28% 
ARG_dat <- ARG_dat %>% 
  add_predictions(mod1) %>% 
  mutate(conf_pred = exp(pred))

p1+geom_point(data = ARG_dat, aes(date, conf_pred), col = "red") +
  geom_line(data = ARG_dat, aes(date, conf_pred), col = "red")

library(ggpmisc)
my.formula <- y ~ x

ARG_dat %>% 
  ggplot(aes(x = dia, y = Lconf)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()

p2 <- ARG_dat %>% 
  ggplot(aes(x=as_date(date), y = deceased))+ geom_point() + 
  scale_x_date(breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) +
  labs(x="", y="Casos reportados")
p2

mod2 = lm(deceased ~ dia, data=ARG_dat1)
mod3 = lm(Ldec ~ dia, data=ARG_dat1)

ARG_dat1 <- ARG_dat1 %>% 
  add_predictions(mod2) %>% 
  mutate(conf_pred = exp(pred))

library(modelr)
rmse(mod2, ARG_dat1)
rmse(mod3, ARG_dat1)

rsquare(mod2, ARG_dat1)
rsquare(mod3, ARG_dat1)

# tidypredict::tidypredict_fit(model2)
# 28% 

ARG_dat1 <- ARG_dat1 %>% 
  add_predictions(mod2) %>% 
  mutate(pred_dec = exp(pred))

ARG_dat1 <- ARG_dat1 %>% 
  add_predictions(mod3) %>% 
  mutate(pred_Ldec = exp(pred))

ARG_dat1 %>% 
  ggplot(aes(x = dia, y = deceased)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point() +
  labs(y = "Difuntos", title = "Modelo linear CME = 0.28")

ARG_dat1 %>% 
  ggplot(aes(x = dia, y = Ldec)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_point()+
  labs(y = "Ln Difuntos", title = "Modelo log-linear CME = 0.16")


p2+geom_point(data = ARG_dat, aes(date, dec_pred), col = "red") +
  geom_line(data = ARG_dat, aes(date, dec_pred), col = "red")

p2+geom_point(data = ARG_dat, aes(date, dec), col = "red") +
  geom_line(data = ARG_dat, aes(date, dec), col = "red")

tidypredict::tidypredict_fit(model1)

# El incremento instantaneo por cada dia adicional desde el inicio de la epidemia es: 
# B1=31,5%
coef(model1)[2]*100  
# El retorno compuesto es = (e^B1) – 1 = 37% 
(exp(coef(model1)[2])-1)*100 

pacman::p_load(forecast, zoo)
ts_conf = ts(ARG$confirmed, start=1, frequency=1)
fit_c = ets(ts_conf, model="AAN", damped=FALSE)
fore_c = forecast(fit_c, h=14) # forecast 7 days
plot(fore_c, main = "Pronostico confirmados 2 semanas = 508 (327;689")

ts_d = ts(ARG$deceased, start=1, frequency=1)
fit_d = ets(ts_d, model="AAN", damped=FALSE)
fore_d = forecast(fit_d, h=14) # forecast 7 days
plot(fore_d, main = "Pronostico difuntos 2 semanas = 5.4 (4.7;6)")

library(tidyverse)
library(lubridate)

df <- 
  tibble::tribble(
    ~n0, ~efic_inter,       ~fecha,
    1,        "0%", "09-05-2020",
    4,        "0%", "27-04-2020",
    16,        "0%", "15-04-2020",
    1,       "25%", "03-06-2020",
    4,       "25%", "16-05-2020",
    16,       "25%", "27-04-2020",
    1,       "50%", "29-03-2021",
    4,       "50%", "25-11-2020",
    16,       "50%", "21-07-2020"
  )


devtools::source_url("https://github.com/juanchiem/R-sources/blob/master/theme_juan.R?raw=TRUE")

df %>% 
  mutate_at(vars(n0), as.numeric)  %>% 
  mutate_at(vars(efic_inter), as.factor)  %>% 
  mutate_at(vars(fecha), dmy) %>% 
  ggplot(aes(x = fecha, y = n0, col=efic_inter)) +  
  geom_line()+geom_point()+labs(x = "Mes a saturación", 
                                y = "Nro de casos al inicio de la epidemia", 
                                col = "Eficiencia de\nla intervención") +
  theme_juan(12, "top")+
  scale_x_date(breaks = "1 month", minor_breaks = "1 week", 
               labels=scales::date_format("%m/%y")) +
  theme(axis.text.x = element_text(angle = 270)) 

https://gabgoh.github.io/COVID/index.html?
  CFR=0.02&
  D_hospital_lag=5&
  D_incbation=5.2&
  D_infectious=2.9&
  D_recovery_mild=11.1&
  D_recovery_severe=28.6&
  I0=1&
  InterventionAmt=0.5&
  InterventionTime=489.274&
  P_SEVERE=0.2&
  R0=2.2&
  Time_to_death=32&
  logN=10.61
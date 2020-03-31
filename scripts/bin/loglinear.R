library(tidyverse)
# https://www.dummies.com/education/economics/econometrics/econometrics-and-the-log-linear-model/

url <- "https://docs.google.com/spreadsheets/d/175RVn2cHCK8nOJPJiXR-iuFP4taC-WbE-DIR5xJSj74/edit?usp=sharing"
ARG <- gsheet::gsheet2tbl(url)

# ARG <- 
all_data  %>%  
  filter(`Country/Region` == "Argentina", 
         date > "2020-03-03") %>% ungroup %>% 
  select(date:value) %>%
  pivot_wider( names_from = "type", values_from = "value") %>% 
  add_row(date ="2020-03-20", confirmed = 128, deceased = 3) %>% 
  datapasta::df_paste() 

ARG_dat <- ARG %>% 
  mutate(date=as_date(date),
         dia = row_number(), 
         Lconf = log(confirmed, base = exp(1)))

ARG_dat1  <-  ARG %>% filter(deceased>0) %>% 
  mutate(date=as_date(date),
         dia = row_number(), 
         Ldec = log(deceased, base = exp(1)))

ARG %>% 
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
  ggplot(aes(x=as_date(date), y = confirmed))+ geom_point() + 
  scale_x_date(breaks = "2 days", minor_breaks = "1 day", 
               labels=scales::date_format("%d/%m")) +
  labs(x="", y="Casos reportados")
p1

mod1 = lm(Lconf ~ dia, data=ARG_dat)
summary(model1)

tidypredict::tidypredict_fit(model1)
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

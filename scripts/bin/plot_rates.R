library(directlabels)

data_start <- dglob %>% pull(date) %>% min()
# data_end <- dglob %>% pull(date) %>% max()

dglob %>%   
  filter(country %in% c("Argentina", "Italy", "Korea, South")) %>% 
  mutate(days = as.numeric((date - data_start))) %>% 
  dplyr::filter(confirmed > 0) -> dglob1

start_dataset <- dglob1 %>%   
  mutate(days = as.numeric((date - data_start))) %>% 
  group_by(country) %>% 
  summarise(onset = min(days))

dglob1 <- dglob1 %>% 
  dplyr::left_join(start_dataset) %>% 
  mutate(matched_days = days - onset, 
         country = factor(country)) 
  
p <- ggplot(data = data.frame(days = 0:30), 
            mapping = aes(x = days))+
  labs(x="Días desde inicio de la epidemia")

exp.2 <- function(x) (exp(log(2)/2))^x # duplica cada 2 dias   
exp.4 <- function(x) (exp(log(2)/4))^x # duplica cada 4 dias
exp.8 <- function(x) (exp(log(2)/8))^x # duplica cada 8 dias

# eje y en escala lineal - curvas exponenciales
(p + 
  stat_function(fun = exp.1, aes(color = "fun.1"), linetype=2) +
  stat_function(fun = exp.2, aes(color = "fun.2"),linetype=2) +
  stat_function(fun = exp.3, aes(color = "fun.3"), linetype=2)+
  scale_color_manual(name = "Duplica cada:", 
                     values = c("red", "orange", "green"), # Color specification
                     labels = c("2 dias", "4 dias", "8 dias"))->p1)

# eje y en escala log natural - rectas
(p1+ scale_y_continuous(trans = scales::log_trans(), 
                     labels = scales::unit_format(unit = "k", scale = 1e-1)))

d1 2 4 8 16


# lin.2 <- function(x) 2/1*x  
# lin.4 <- function(x) 1/3*x
# lin.8 <- function(x) 4*x

dglob1 %>%     
  ggplot(aes(x=matched_days, y=confirmed))+ 
  geom_line(aes(group=country))+
  geom_dl(aes(label=country),
          # method=list("last.points",rot=30))+
  method = list(dl.trans(x = x - 0.3, y = y + 0.4),
                dl.combine("last.points"), cex = 0.8)) +
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale = 1e-1),#)+
                      trans = scales::log_trans())+
  labs(x="Días desde inicio de la epidemia")+
  # stat_function(fun = exp.2, aes(color = "exp.2"), linetype=2) +
  stat_function(fun = exp.4, aes(color = "exp.4"), linetype=2) +
  stat_function(fun = exp.8, aes(color = "exp.8"), linetype=2)+
  scale_color_manual(name = "Duplica cada:", 
                     values = c("red", "orange", "green"), # Color specification
                     labels = c("2 dias", "4 dias", "8 dias"))


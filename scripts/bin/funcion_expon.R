# Linealizacion de la funcion exponencial 

p <- ggplot(data = data.frame(days = 0:30), 
            mapping = aes(x = days))+
  labs(x="Días desde inicio de la epidemia")

exp.2 <- function(x) (exp(log(2)/2))^x # duplica cada 2 dias   
exp.4 <- function(x) (exp(log(2)/4))^x # duplica cada 4 dias
exp.8 <- function(x) (exp(log(2)/8))^x # duplica cada 8 dias

# eje y en escala lineal - curvas exponenciales
(p + 
    stat_function(fun = exp.2, aes(color = "exp.2"), linetype=2) +
    stat_function(fun = exp.4, aes(color = "exp.4"),linetype=2) +
    stat_function(fun = exp.8, aes(color = "exp.8"), linetype=2)+
    scale_color_manual(name = "Duplica cada:", 
                       values = c("red", "orange", "green"), # Color specification
                       labels = c("2 dias", "4 dias", "8 dias"))->p1)

# eje y en escala log natural - rectas
(p1+ scale_y_continuous(trans = scales::log_trans(), 
                        labels = scales::unit_format(unit = "k", scale = 1e-1)))




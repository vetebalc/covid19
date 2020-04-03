# COVID-19
#
# Estimación de los parametros de una exponecial y el factor de crecimiento
# También para los casos de cont  acto directo
#
# Fuente @msalnacion 
#

require(dplyr)
require(readr)
require(lubridate)
cor <- read_csv("/home/leonardo/Academicos/GitProjects/covid19/coronavirus_ar.csv")
cor <- cor %>% mutate(fecha=ymd(fecha), dias =as.numeric( fecha - min(fecha))) 

d <- cor %>% dplyr::select(fecha,dias,comunitarios) %>%  rename(N = comunitarios) %>% filter(N>0)

mod = nls(N ~ a * exp(b * dias), data = d, start = list(a = 1, b = 0.05))

# Extraigo los coeficientes para ponerlos en el gráfico
#
a <- round(coef(mod)[1],2)
b <- round(coef(mod)[2],2)
summary(mod)

preds <-  data.frame(dias = seq(13, 26,by = 0.1))  
preds$N <-  predict(mod, newdata = preds)

library(boot)
set.seed(42)
myboot <- boot(d, function(d, ind, mod, preds) {
  d$N <- fitted(mod) + residuals(mod)[ind]
  tryCatch(predict(nls(N ~ a * exp(b * dias), data = d, start = list(a = 1, b = 0.05)), newdata = preds),
           error = function(e) preds$x * NA)
}, mod = mod, preds = preds, R = 1e4)

CI <- t(sapply(seq_len(nrow(preds)), function(i) boot.ci(myboot, type = "bca", index = i)$bca[4:5]))
colnames(CI) <- c("lwr", "upr")
preds <- cbind(preds, CI) %>% mutate(fecha = min(cor$fecha)+dias)

library(ggplot2)
col <- viridisLite::viridis(3)

ggplot(d, aes(fecha, N)) + 
  geom_ribbon(aes(ymin = lwr, ymax = upr), data = preds, color = NA, fill = "grey80") +
  geom_line(mapping = aes(fecha, N), data = preds,colour=col[1]) + theme_bw()
  geom_point() #+ scale_y_log10()

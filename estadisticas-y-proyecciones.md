# Estadísticas


## Balcarce y zona

<img src="plots/mapa.jpg" width="600px">

## Argentina y países limítrofes

<img src="plots/p_latam.jpg" width="600px">

## Global

<img src="plots/global_log.jpg" width="600px">

### Fuentes de datos

- Argentina: [Sistemas Mapache](https://github.com/SistemasMapache/Covid19arData)
- Global: [Johns Hopkins CSSE](https://github.com/CSSEGISandData/COVID-19)

---

# Proyecciones

### Modelado epidemiológico 

Estamos trabajando para realizar proyecciones específicas para el partido de Balcarce, utilizando modelos epidemiógicos. Estos modelos permiten explicar y predecir el comportamiento de agentes infecciosos y potencialmente dañinos para las poblaciones humanas (en este caso). 

En este caso haremos uso de un modelo clasico de epidemiologia de enfermedades infecto-contagiosas conocido com "SEIR", de sus siglas: "S" individuos - "E" expuestos - "I" infectados - "R" recuperados.

<img src="images/SEIR.png" width="600px">

S-Susceptible, E-expuesto (infectado), I-Infeccioso (que contagia) y R-Recuperado
H-hospitalizados, C-críticos (UCI), y D - difuntos.  
S -> I modelado por tasa de infección (R0)(R0 afectado por las intervenciones “Int” y la estacionalidad “∿”). Ld: período de latencia; Id:periodo infeccioso. 

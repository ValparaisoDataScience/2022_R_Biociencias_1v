# ----------------------------------------------------------
# Clase 03 - Script graficas simples con ggplot2
# Dr. José Gallardo
# 19 enero 2022
# Curso R para Biociencias
# ----------------------------------------------------------

# habilitar librería ggplot2
library(ggplot2)

# Intento de gráfica con función ggplot.
# La gráfica queda vacía pues falta indicar el tipo de gráfica que deseamos
ggplot(CO2, aes(uptake))

# Histrograma con ggplot.
ggplot(CO2, aes(uptake))+
 geom_histogram()

# Agregamos titulo y nombre de los ejes
ggplot(CO2, aes(uptake))+
  geom_histogram()+
  labs(title="Histograma", x="Consumo de CO2", 
       y="Frecuencia")

# Gráfica con dos ejes y tratamiento pero incompleta
ggplot(CO2, aes(x=Treatment, y=uptake))

# Gráfica de boxplot
ggplot(CO2, aes(x=Treatment, y=uptake))+
geom_boxplot()

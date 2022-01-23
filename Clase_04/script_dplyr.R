# ----------------------------------------------------------
# Clase 04 - Script Manipulación de datos con dplyr
# Dra. María Angélica Rueda
# 20 enero 2022
# Curso R para Biociencias
# ----------------------------------------------------------

# Habilita librerías
library(readxl) # Para importar datos a R

library(dplyr) # Para manipular datos

library(ggplot2) # Para hacer gráficos

library(tidyr) # Para manipular datos

# LIBRERÍA DPLYR: EL OPERADOR PIPE (TUBERÍA).

# dplyr usa el operador pipe %>% como una tubería para enlazar un data.frame con una o más funciones.

x <- rnorm(5)
y <- rnorm(5)
dat <- data.frame(x,y)
dat
max(dat) 
dat %>% max
dat %>% arrange(y) # Ordena filas de un data.frame por el valor de alguna columna

# ESTUDIO DE CASO: MUESTREO DE PECES
peces <- read_excel("Peces.xlsx") # Carga el set de datos
head(peces)  # Muestra los primeros datos del data set
summary(peces)

# FUNCIÓN REPLACE_NA() CON LIBRERÍA TIDYR
# Permite reemplazar datos faltante (NA) por algún valor.
peces <- peces %>% 
  replace_na(list(Peso= 120, Parasitos= 25))

# FUNCIÓN SELECT()

# Permite extraer o seleccionar variables/columnas específicas de un data.frame.
select(peces, Especie, Sexo)

# FUNCIÓN SELECT() CON PIPE
peces %>% select(Especie, Sexo)

# FUNCIÓN FILTER() CON PIPE
# **filter()**: Para filtrar desde una tabla de datos un subconjunto de filas.
# Ej. solo un nivel de de un factor, observaciones que cumplen algún criterio (ej. > 20).
peces %>% filter(Sexo == "Macho")

# MÚLTIPLES FUNCIONES Y TUBERÍAS
peces %>% select(Especie, Sexo, Peso) %>% 
  filter(Sexo == "Macho")

# FUNCIÓN SUMMARIZE()

peces %>% select(Especie, Sexo, Peso, Parasitos) %>% 
          summarize(n = n(), 
                    Promedio_Peso = mean(Peso), 
                    Maximo_Parasitos = max(Parasitos))


# FUNCIÓN SUMMARIZE() + GROUP_BY()
# Permite agrupar filas con base a los niveles de alguna variable o factor.

peces %>% group_by(Especie) %>% 
          summarize(n = n(), 
            Promedio_Peso = mean(Peso), 
            Maximo_Parasitos = max(Parasitos))

# FUNCIÓN MUTATE()
# Permite calcular nuevas variables "derivadas". Util para calcular proporciones, tasas.

peces %>% select(Especie, Peso, Parasitos) %>% 
  mutate(Densidad_parasitos = Parasitos/Peso)

# GENERAR NUEVA BASE DE DATOS + FUNCIÓN MUTATE()

datos<- peces %>% select(Especie, Sexo, Peso, Parasitos)%>% 
  mutate(Densidad_parasitos = Parasitos/Peso)

# HACER PLOT CON GGPLOT2
ggplot(datos, aes(x=Peso, y=Parasitos))+
  geom_point(size= I(2))+
  labs(x= "Peso(g)", y= "Cantidad de parásitos")+
  theme_bw()

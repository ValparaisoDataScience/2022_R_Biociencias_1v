### ---------------------------------------
### Script Regresión Logística
### Dra. María Angélica Rueda Calderón
### maria.rueda.c@pucv.cl
### 27 de enero 2022
### Curso Análisis de datos con R para Biociencias
### ----------------------------------------

### Librerías
library(car)
library(lmtest)
library(psych)
library(readxl)
library(nlme)
library(lme4)
library(stats)
library(boot)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(knitr)
library(car)
library(lmtest)
library(psych)
library(gridExtra)
library(nortest)
library(sjPlot)
library(lme4)
library(reshape2)

#===== Comparación entre regresión lineal simple y regresión polinómica =======

# Importa datos estudio de caso alimentación moluscos filtradores
# Fuente: Willer and Aldridge 2017](https://royalsocietypublishing.org/doi/10.1098/rsos.171142)
clearance <-  read_excel("ParticleClearance.xlsx", sheet = 1)

# Generar subsets
mussel <- filter(clearance, sample == "mussel")
control <- filter(clearance, sample == "control")

# Gráfica tasa de aclaración (Proxy de consumo de partículas).
ggplot(data = mussel, aes(x = time, y = microparticle_concentration)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(Concentration~microparticles~ml^-1)) +
  scale_shape_manual(values=c(1,2)) +
  stat_smooth(method='loess',formula=y~x, se=T)+
  scale_color_brewer(palette="Set1") + 
  theme(legend.position="none") +
  theme(panel.border=element_blank(), axis.line=element_line())

# Ajusta modelo de regresión lineal simple (Modelo 1)
reg_mussel <- lm(log_microparticle_concentration ~ time, data=mussel)

# Muestra los resultados del modelo
summary(reg_mussel)

# Ajusta modelo de regresión polinomial (Modelo 2)
reg_mussel_2 <- lm(log_microparticle_concentration ~ poly(time,2), data=mussel)

# Muestra los resultados del modelo
summary(reg_mussel_2)

# Comparación de modelos
anova(reg_mussel,reg_mussel_2)

#============================== Regresión logística============================ 

# Estudio de caso Maduración en Salmón del Atlántico

# Carga el set de datos
maduracion <- read_excel("Maturation.xlsx")

# Transforma la variable genotipo en factor
maduracion$Genotype <- as.factor(maduracion$Genotype)

# Genera subset con dyplr
maduracion <- maduracion%>% 
  select("Fish","Genotype","Gonad","GSI","Maturation")

# Genera objeto externo para el gráfico 
formula1 <- y ~ x

# Ajusta modelo de regresión lineal simple
mod_lineal <- lm(Maturation ~ Gonad, data = maduracion)

# Muestra los resultados del modelo de regresión lineal simple
summary(mod_lineal)

# Hace el gráfico del modelo de regresión simple
ggplot(data = maduracion, aes(x = Gonad, y = Maturation)) +
  geom_point(aes(color = as.factor(Maturation)), shape = 1) + 
  theme(axis.text.x = element_text(size = 10,face="bold",colour="black"))+
  theme(axis.text.y = element_text(size = 10,face="bold",colour="black"))+
  theme_bw()  +
  labs(x= "Peso de gónada", y = "Maduración")+
  theme(legend.position = "none")+ 
  theme(panel.border=element_blank(), axis.line=element_line())

# Ajusta modelo de regresión logística
mod_logit <- glm(Maturation ~ Gonad, 
                 family= binomial, data = maduracion)

# Muestra los resultados del modelo de regresión logística
summary(mod_logit)

# Hace el gráfico del modelo de regresión logística
ggplot(data = maduracion, aes(x = Gonad, y = Maturation)) +
  geom_point(aes(color = as.factor(Maturation)), shape = 1) + 
  theme(axis.text.x = element_text(size = 10,face="bold",colour="black"))+
  theme(axis.text.y = element_text(size = 10,face="bold",colour="black"))+
  stat_function(fun = function(x){predict(mod_logit,
                                          newdata = data.frame(Gonad = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística", x="Peso de gónada",
       y = "Probabilidad de Maduración") +
  theme(legend.position = "none")+
  theme(panel.border=element_blank(), axis.line=element_line())

#Ajusta modelo de regresión logística (MODELO NULO)
mod_nulo <- glm(Maturation ~ 1, 
                family= binomial, data = maduracion)

summary(mod_nulo)


# Ajusta un modelo de regresión logística simple
mod_logit <- glm(Maturation ~ Gonad, 
                 family= binomial, data = maduracion)

summary(mod_logit)

# Comparación de modelos por el criterio de AIC

AIC(mod_nulo,mod_logit)

# Comparación de modelos usando Anova
anova(mod_nulo,mod_logit, test ='Chisq')

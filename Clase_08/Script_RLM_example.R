### ---------------------------------------
### Script Regresión lineal múltiple
### Dr. José A. Gallardo.
### Mjose.gallardo@pucv.cl
### 26 de enero 2022
### Curso Análisis de datos con R para Biociencias
### ----------------------------------------

### Librerías
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(knitr)
library(car)
library(lmtest)
library(psych)


# Importa datos estudio de caso alimentación moluscos filtradores
# Fuente: Willer and Aldridge 2017](https://royalsocietypublishing.org/doi/10.1098/rsos.171142)
clearance <-  read_excel("ParticleClearance.xlsx", sheet = 1)

# Create data filters
mussel <- filter(clearance, sample == "mussel")
control <- filter(clearance, sample == "control")

# Gráfica tasa de aclaración (Proxy de consumo de partículas).
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

microplot <- ggplot(data = mussel, aes(x = time, y = microparticle_concentration)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(Concentration~microparticles~ml^-1)) +
  scale_shape_manual(values=c(1,2)) +
  stat_smooth(method='loess',formula=y~x, se=T)+
  scale_color_brewer(palette="Set1") + 
  theme(legend.position="none") +
  theme(panel.border=element_blank(), axis.line=element_line())
microplot+My_Theme

# Crea modelos de regresión por tipo de muestra con variable transformada a logaritmo
reg_mussel <- lm(log_microparticle_concentration ~ time, data=mussel)
reg_control <- lm(log_microparticle_concentration ~ time, data=control)

## Microparticle concentration vs time
# Plotting microparticle concentration vs time

microplot <- ggplot(data = clearance, aes(x = time, y = log_microparticle_concentration, color = sample, shape = sample)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(log[10]~(Concentration~microparticles~ml^-1))) +
  stat_smooth(method='lm',formula=y~x, se=F) +
  scale_shape_manual(values=c(1,2)) +
  scale_color_brewer(palette="Set1") + 
  theme(legend.position= c(0.2, 0.2)) +
  theme(panel.border=element_blank(), axis.line=element_line())
microplot+My_Theme

# Crea modelo de regresión múltiple con función lm()
lm.full <- lm(log_microparticle_concentration 
              ~ time*sample + time + sample, 
              data = clearance)

# Imprime resultado del modelo de regresión múltiple con función summary()
summary(lm.full)$coef %>% kable()

# Imprime tabla de ancova del modelo lineal
anova(lm.full) %>% kable()

# Crea dos modelos de regresión lineal simple para comparar con regresión multiple
reg_mussel <- lm(log_microparticle_concentration 
                 ~ time, data=mussel)

reg_control <- lm(log_microparticle_concentration 
                  ~ time, data=control)

# Crea un set de datos simulados con una variable respuesta Y
# y dos variables predictoras correlaciodas X1 y X2
set.seed(50)
X1=rnorm(100,0,1)
X2=rnorm(100,0,1)+(3.1*X1)
Y= 2 + 0.5 * X1 + 0.1 * X2 + rnorm(100,0,0.4)
lm1<- lm(Y~X1+X2)
lm2<- lm(Y~X1)
sim_dat<-cbind(Y,X1,X2)
head(sim_dat) %>% kable(digits=2)

# Crea un panel de correlación para evaluar multicolinealidad
pairs.panels(sim_dat)

# Crea un modelo de regresión múltiple
lm1<- lm(Y~X1+X2)

# Calcula Factor de inflación de la varianza para evaluar multicolinealidad.
vif(lm1) %>% 
  kable(digits=2, col.names = c("VIF"))


# Crea e imprime modelo de regresión múltiple
lm0<- lm(Y~X1+X2)
summary(lm0)$coef %>% kable()

# Crea e imprime modelo de regresión simple con variable predictora X1
lm1<- lm(Y~X1)
summary(lm1)$coef %>% kable()

# Crea e imprime modelo de regresión simple con variable predictora X2
lm2<- lm(Y~X2)
summary(lm2)$coef %>% kable()

# Compara modelos usando diferentes criterios
anova(lm0, lm1, lm2) %>% kable()
AIC(lm0, lm1, lm2) %>% kable()
BIC(lm0, lm1, lm2) %>% kable()


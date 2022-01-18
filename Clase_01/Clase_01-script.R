# ----------------------------------------------------------
# Clase 1 - Programación con R
# Dr. José Gallardo
# 17 enero 2022
# Curso R para Biociencias
# ----------------------------------------------------------

# R funciona con comando matemáticos
29+29
29*29
29==29

# Error en R
29 + diez

# Version R
version
R.version.string

# ¿Como citar R?
citation()

# En que directorio estoy
getwd()

# Listar librerías o packages disponibles en mi entorno de trabajo
search()

# Listar archivos en el directorio actual
list.files()

# Listar objetos 
ls()

# Crear un objeto
Nombres <- c("José Gallardo", "Maria Rueda")

# Características de un objeto
class(Nombres)
colnames(Nombres)

# Remover objetos de la sesión de trabajo
rm(list = ls())

Nombres <- c("José Gallardo", "Maria Rueda")
Sexo <- c(1,2) # Codificados 1= varón ; 2= Mujer
Estatura <- c(173, 178) 
Edad <- c(47, 36)
Profesores <- data.frame(Nombre,Sexo,Estatura)
View(Profesores)
dim(Profesores)
class(Profesores)
Profesores[1,1]
Profesores[1,]
Profesores[2,4] # da NULL

# Obtener ayuda de un comando
help("dim")
help("mean")
mean(Profesores$Nombres) # da error porque nombres no es humérico.
mean(Profesores$Estatura)
Profesores$Sexo <- as.factor(Profesores$Sexo)
mean(Profesores$Sexo)
class(Profesores$Sexo)
Profesores$Sexo

# Librerías, gráficas y funciones
help("datasets")
help(BOD)
summary(BOD)
hist(BOD$demand, main = "Demanda bioquimica de oxígeno", col = "red")
plot(BOD$Time, BOD$demand)
cor(BOD$Time, BOD$demand)

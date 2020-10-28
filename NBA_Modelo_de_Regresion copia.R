rm(list=ls())
library(readr)
DATOS_NBA <- read.csv("nba.csv", sep= ",",  fileEncoding="latin1")
View(DATOS_NBA)

# Empleamos la funcion lm() para generar un modelo de regresión lineal por 
# minimos cuadrados en el que la variable de respuesta son los jugadores en el
# que la variable de respuesta es..... y el predictor.....

modelo_regresion <- lm(Salary ~ Age, data=DATOS_NBA)

# Esta función lm() genera un objeto, modelo_regresion, que almacena toda la 
# informacion del modelo.

# Podemos ver sun contenido y los principales parámetros del modelo con las 
# funciones names() y summary() respectivamente.

names(modelo_regresion)
summary(modelo_regresion)

# En la informacion que nos proporciona summary, observamos que el p-valor del 
# estadistico es. muy pequeño, lo que significa que al menos uno de los predictores 
# del modelo está significativamente relacionado con la variable respuesta. 

# La estimacion de los coeficientes de regresion tiene su correspondiente 
# intervalo de confianza.
confint(modelo_regresion, level = 0.95)
# el p-value del predictor Age ha resultado significativo para un α = 0.05,
# su intervalo de confianza del 95% no contiene el valor 0.


# Graficamente:
attach(DATOS_NBA)
plot(x = Age, y = Salary, main = "Salarios Vs. Edad", pch = 5, col = "grey")
abline(modelo_regresion, lwd = 5, col = "light blue")
# Muestra que la relación entre las variables estudiadas no es del todo lineal,
# lo que apunta a que otro tipo de modelo podría explicar mejor la relación.

# Para comprobar si se cumplen las condiciones necesarias para un modelo de 
# regresion lineal simple, estudiamos los residuos:
par(mfrow = c(1,2))
plot(modelo_regresion)
par(mfrow = c(1,1))
# Los residuos confirman que los datos no se distribuyen de forma lineal.


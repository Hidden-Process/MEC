# ----------------------------------------------
# Ejemplo 3:
#-----------------------------------------------

# Cargamos librerias necesarias:

library(tidyverse)
library(zoo)

# Datos originales:

zz <- c(178.2,153.2,185.9,163.6,196.3,156.9,197.9,166.4,197.3,159.7,
        202.6,175.6,209.5,169.5,202.4,179.8,200.0,168.6,216.1,178.5)

#Formato de serie temporal:

ej3 <- ts(zz, start=c(1980, 1), frequency=4)

# Eliminamos el factor TC (Tendencia - Ciclo) de la serie original:
# Al ser estaciones, y cada año tener 4 estaciones, usaremos medias moviles de orden 4.
# Tenemos 2 formas de calcularlo

tend <- stats::filter(ej3, c(.5, 1, 1, 1, .5) / 4)
tend2 <- rollmean(rollmean(zz,4),2)

# Graficamos la serie original con la tendencia en rojo

plot(ej3)
lines(tend,col=2)

# Siguiendo la hipotesis multiplicativa calculamos la componente Estacional-Aleatoria:
# X = TECA ==> X/TC = EA

est_Ale <- ej3 / tend

# Componente Estacional sin normalizar:
# Eliminando la componente aleatoria, calculando el indice de cada estación (Sin normalizar)

est1M <- colMeans(matrix(est_Ale, ncol=4, byrow=T), na.rm=T)

# Normalizamos dichos indices en el caso de que su media no sea 1.
estM  <- est1M / mean(est1M)

# Estos valores representan la componente estacional, podemos multiplicarlo por 100 para verlo en porcentaje.
estM*100

# Podemos ver el porcentaje de variación que representa la estacionalidad.
(estM-1)*100

# Podemos ver la serie original desestacionalizada de la siguiente manera:
desestM <- ej3 / estM

# Gráfico de la serie desestacionalizada
plot(ej3)
lines(desestM, col=4)

# Todo esto lo podemos ver de forma automática con comando decompose.

descM <- decompose(ej3, type="mul")
plot(descM)

# Una vez tenemos la serie desestacionalizada, podemos recalcular la tendencia real y obtener una mejor
# aproximación de la trayectoria de la serie utilizando una regresion lineal de la serie desestacionalizada


# Podemos usar los datos originales en formato vector o transformarlos desde la representacion en serie
# temporal con la función seq_along.

xx <- seq_along(ej3)
model <- lm(desestM ~ xx)

# Valores predichos por la recta
regL <- ts(predict.lm(model), start=c(1980, 1), frequency=4)

# Graficamos la tendencia real sobre la grafica desestacionalizada
plot(desestM)
lines(regL, col=2)

# Utilizamos la autocorrelacion como método para buscar ciclos en la serie temporal.
# Usaremos una función con la serie temporal como dato de entrada y la frecuencia del ciclo que buscamos. 

autoCorrel <- function(data, n) {
  xx1 <- head(data, -n)
  xx2 <- tail(data, -n)
  summary(lm(xx1 ~ xx2))$r.squared
  #cor(xx1,xx2)
}

# Autocorrelación de frecuencia 4:
autoCorrel(ej3, 4)

# Calculamos las autocorrelaciones para frecuencias 1 a 12
acValues <- map_dbl(1:12, autoCorrel, data=ej3)

# Y vemos donde se alcanza el máximo que nos indicará la frecuencia correcta.
acValues
which.max(acValues)


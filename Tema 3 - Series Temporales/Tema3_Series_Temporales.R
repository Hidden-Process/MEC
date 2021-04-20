library(tidyverse)

# La carpeta dónde tengas los datos

localFolder='C:\data'

# leemos los datos y le indicamos la frecuencia de la componente estacional

xx <- scan(str_c(localFolder, 'ts01.dat'))
uk <- ts(xx, start=c(1969, 1), frequency=4)

# Ejemplo del primer valor de la media movil de orden 4.

(uk[1] * .5 + uk[2] + uk[3] + uk[4] + uk[5] * .5) / 4

# Componente tendencia, mediante una media movil de orden 4.

tend <- stats::filter(uk, c(.5, 1, 1, 1, .5) / 4)

# Gráfico de la serie con la tendencia en rojo

plot(uk)
lines(tend, col=2)


##############################################################
# Descomposición multiplicativa
##############################################################

# Componente estacional * aleatoria
est_aleM <- uk / tend

# Componente estacional sin normalizar
est1M <- colMeans(matrix(est_aleM, ncol=4, byrow=T), na.rm=T)

# Normalizo la componente estacional
estM  <- est1M / mean(est1M)

# Componente estacional como serie temporal
estMC <- ts(rep(estM, 20), start=c(1969, 1), frequency=4)

# Componente aleatoria
aleM  <- est_aleM / estM

# Serie desestacionalizada
desestM <- uk / estM

# Gráfico de la serie desestacionalizada
plot(uk)
lines(desestM, col=4)


##############################################################
# Descomposición aditiva
##############################################################

# Componente estacional * aleatoria
est_aleA <- uk - tend

# Componente estacional sin normalizar
est1A <- colMeans(matrix(est_aleA, ncol=4, byrow=T), na.rm=T)

# Normalizo la componente estacional
estA  <- est1A - mean(est1A)

# Componente estacional como serie temporal
estAC <- ts(rep(estA, 20), start=c(1969, 1), frequency=4)

# Componente aleatoria
aleA  <- est_aleA - estA

# Serie desestacionalizada
desestA <- uk - estA

# Gráfico de la serie desestacionalizada
plot(uk)
lines(desestA, col=4)


##############################################################
# Calculamos una regresión lineal de la serie desestacionalizada
##############################################################

# Esto mete en xx un vector 1:n, donde n es el tamaño de uk
xx <- seq_along(uk)

# regresión lineal
model <- lm(desestM ~ xx)

# Calculamos los valores predichos por la recta
regL <- ts(predict.lm(model), start=c(1969, 1), frequency=4)

# Hacemos un gráfico de la serie desestacionalizada con la recta
plot(desestM)
lines(regL, col=2)


##############################################################
# Uso del comando decompose. Debemos obtener los mismos resultados.
##############################################################

descM <- decompose(uk, type="mul")
descA <- decompose(uk, type="add")
plot(descM)


##############################################################
# Autocorrelacion
##############################################################

# Función que calcula la autocorrelación para una frecuencia n
autoCorrel <- function(data, n) {
  # Quita los n últimos datos
  xx1 <- head(data, -n)
  # Quita los n primeros datos
  xx2 <- tail(data, -n)
  # correlación
  summary(lm(xx1 ~ xx2))$r.squared
}

# Calculamos la autocorrelación para frecuencia 4
autoCorrel(uk, 4)

# Calculamos las autocorrelaciones para frecuencias 1 a 12
acValues <- map_dbl(1:12, autoCorrel, data=uk)

# Y vemos que el máximo se alcanza con autocorrelación 4, así que es la frecuencia correcta.
acValues
which.max(acValues)

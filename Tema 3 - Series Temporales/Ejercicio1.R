# Ejercicio 1 - Relación de Ejercicios 3

# Apartado A:Calculo de la tendencia haciendo uso de medias móviles

# Datos:

data <- c(9.47,9.26,8.86,8.25,7.81,8.01,7.55,7.24,7.01,6.88,7.03)
tiempo <- c(1973:1983)

# Medias Móviles:
tend1 <- stats::filter(data, c(.5, 1, 1, 1, .5) / 4)
tend2 <- stats::filter(data, c(1, 1, 1, 1, 1) / 5)

# Representación gráfica:
plot(data)
lines(data)
lines(tend1,col=2)
lines(tend2,col=4)

# Apartado B: Calculo de la tendencia con modelo de regresión lineal

model <- lm (data ~ tiempo)
regL <- predict.lm(model)
plot(data)
lines(regL,col=2)


# Apartado C: Comparación de ambos métodos de forma visual.

plot(data)

lines(tend1,col=2)
lines(tend2, col=4)
lines(regL, col=3)



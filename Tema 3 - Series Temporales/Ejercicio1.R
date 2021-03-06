# Ejercicio 1 - Relaci�n de Ejercicios 3

# Apartado A:Calculo de la tendencia haciendo uso de medias m�viles

# Datos:

data <- c(9.47,9.26,8.86,8.25,7.81,8.01,7.55,7.24,7.01,6.88,7.03)
tiempo <- c(1973:1983)

# Medias M�viles:
tend1 <- stats::filter(data, c(.5, 1, 1, 1, .5) / 4)
tend2 <- stats::filter(data, c(1, 1, 1, 1, 1) / 5)

# Representaci�n gr�fica:
plot(data)
lines(data)
lines(tend1,col=2)
lines(tend2,col=4)

# Apartado B: Calculo de la tendencia con modelo de regresi�n lineal

model <- lm (data ~ tiempo)
regL <- predict.lm(model)
plot(data)
lines(regL,col=2)


# Apartado C: Comparaci�n de ambos m�todos de forma visual.

plot(data)

lines(tend1,col=2)
lines(tend2, col=4)
lines(regL, col=3)



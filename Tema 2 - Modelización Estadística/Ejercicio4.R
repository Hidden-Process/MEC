# Ejercicio 4 - Relación 2:

# Datos de entrada:

densidad <- c(43,55,40,52,39,33,50,33,44,21)
velocidad <- c(27,23.8,30.7,24,34.8,41.4,27,40.4,31.7,51.2)

# Apartado A: Grafico de Dispersión:

plot(densidad,velocidad)

model <- lm(densidad~velocidad)
summary(model)

# Apartado C: Calcular la r o Coeficiente de Correlación Lineal del modelo.

CovXY <- cov(densidad, velocidad) * (length(densidad)-1) / length(densidad)

DesvDen <- sqrt(mean(densidad ^2) - mean(densidad)^2)
DesvVel <- sqrt(mean(velocidad ^2) - mean(velocidad)^2)

r <- CovXY / (DesvDen * DesvVel)

# Una forma mucho más rapida de calcular ese coeficiente es usar la función cor.

r <- cor(densidad,velocidad)


# -------------------------------------------------------------------------------------
# Explicaciones del apartado b y d:

# Apartado b:

# El único valor posible de entre los 3 a elegir al ver el grafico de dispersión es el de -0.968 ya que
# vemos como sigue un modelo linealmente inverso por tanto el r debe ser negativo y ademas parece bastante 
# lineal por lo que deberia estar cercano a -1.

# Apartado d:

# Son muy pocos datos para poder afirmar que existe tal asociacion, pero quitando ese detalle, y centrandonos
# en los datos que tenemos y a la vista del coeficiente de correlación obtenidos podemos afirmar que si
# que existe dicha asociacion, a menor velocidad de circulación, mayor densidad de vehiculos en dicho tunel.



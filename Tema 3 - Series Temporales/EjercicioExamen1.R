# Lectura de datos

data <- c(2.2,2.3,3.1,3.4,3.3,3.8,4.4,4.6,5.2)
serie <- ts(data, start = c(2014,1), frequency = 3);

# Nos preguntan por la componente aleatoria de la serie en el tercer periodo de 2015

# Respuesta inmediata:
decompose(serie,type = "mul")$random[[6]]

# Respuesta Elaborada: Hipotesis Multiplicativa


# Tendencia / Componente TC
tend <- stats::filter(serie, c(1, 1, 1) / 3)
plot(serie)
lines(tend,col = 2)

decompose(serie,type = "mul")$trend


# Componenete Estacional Aleatoria
est_aleM <- serie / tend

# Componente Estacional sin normalizar
est1M <- colMeans(matrix(est_aleM, ncol=3, byrow=T), na.rm=T)

# Normalización de la componente estacional (Indices Normalizados)
estM  <- est1M / mean(est1M)
decompose(serie,type = "mul")$figure

# Componente Aleatoria
aleM  <- est_aleM / estM
decompose(serie,type = "mul")$random

# Respuesta a la pregunta que nos piden
aleM[[6]]

# Serie desestacionalizada
desestM <- serie / estM

# Gráfico de la serie desestacionalizada
plot(serie)
lines(desestM, col=4)

# Componente Estacional
serie/desestM
decompose(serie,type = "mul")$seasonal

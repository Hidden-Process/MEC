# Dato de entrada:
data <- c(2,2,3,3,3,4,5,4,2,4,5,4,4,5,7,3,5,6,8,5)

# La serie disminuye un 20.99% en el primer trimestre de cada año
data[c(1,5,9,13,17)] <- data[c(1,5,9,13,17)] * (79.01/100)

# Pregunta: Calcular la componente desestacionalizada del primer trimestre de 1996

# Paso a paso

serie <- ts(data, start = c(1995,1), frequency = 4)

tend <- stats::filter(serie, c(.5, 1, 1, 1, .5) / 4)
est_aleM <- serie / tend
est1M <- colMeans(matrix(est_aleM, ncol=4, byrow=T), na.rm=T)
estM  <- est1M / mean(est1M)
aleM  <- est_aleM / estM
desestM <- serie / estM
desestM[[5]]

# Decompose

(serie / decompose(serie, type = "mul")$seasonal)[[5]]

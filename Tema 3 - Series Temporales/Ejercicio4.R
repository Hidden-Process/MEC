# Datos y Formato de serie temporal:

energia <- c(178.2,156.7,164.2,153.2,157.5,172.6,185.9,185.8,165.0,163.6,169.0,183.1,
             196.3,162.8,168.6,156.9,168.2,180.2,197.9,195.9,176.0,166.4,166.3,183.9,
             197.3,173.7,173.2,159.7,175.2,187.4,202.6,205.6,185.6,175.6,176.3,191.7,
             209.5,186.3,183.0,169.5,178.2,186.7,202.4,204.9,180.6,179.8,177.4,188.9,
             200.0,188.7,187.5,168.6,175.7,189.4,216.1,215.4,191.5,178.5,178.6,195.6,
             205.2,179.6,185.4,172.4,177.7,202.7,220.2,210.2,186.9,181.4,175.6,195.6)

serie <- ts(energia, start=c(1976, 1), frequency=12)

#Apartado A:

# tend = TC 
tend <- stats::filter(serie, c(.5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, .5) / 12)

plot(serie)
lines(tend,col=2)

# Asumiendo la hipotesis multiplicativa:
# X = TECA ==> X/TC = EA

est_Ale <- serie / tend

# Indices de Variación estacional

est1M <- colMeans(matrix(est_Ale, ncol=12, byrow=T), na.rm=T)

# Normalización de índices:
estM  <- est1M / mean(est1M)

# Apartado B:

# Serie desestacionalizada
desest <- serie/estM

#Apartado C:

# Comparacion entre serie original y desestacionalizada.
plot(serie)
lines(desest,col=2)

# Comprobamos lo calculado hasta el momento.
desM <- decompose(serie,type="mul")
plot(desM)

# Apartado D:

xx <- seq_along(serie)
model <- lm(desest ~ xx)
regL <- ts(predict.lm(model), start=c(1976, 1), frequency=12)


plot(desest)
lines(regL, col=2)

# Apartado E:

# Obtenemos los ciclos y la aletoriedad, eliminando la tendencia de los datos desestacionalizados de 
# la serie de datos desestacionalizados. (Dividiendo)

desest / regL

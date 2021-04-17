# Datos y formato de serie temporal:

# Tomamos frecuencia 3 ya que hablamos de cuatrimestres,y hay 3 cuatrimestres en un año.
produccion <- c(3.9,4,4.8,5.1,5,5.5,6.1,6.3,6.9)
serie <- ts(produccion, start=c(2016,1), frequency=3)

# Tendencia: Calculamos las medias móviles de orden 3 ya que estamos trabajando con frencuencia 3.
tend <- stats::filter(serie, c(1, 1, 1) / 3)

plot(serie)
lines(tend,col=2)

# Estacionalidad: Tomamos hipotesis multiplicativa.

est_Ale <- serie / tend
est1M <- colMeans(matrix(est_Ale, ncol=3, byrow=T), na.rm=T)

# Indices de variación estacional normalizados:
estM  <- est1M / mean(est1M)
(estM-1)*100

# Serie desestacionalizada
desest <- serie / estM

plot(serie)
lines(desest,col=4)

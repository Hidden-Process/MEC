# Ejercicio 19 Relación de Ejercicios Tema 1:

# Ejercicio de Filtrado de Datos Anomalos o outliers de una lista de valores

# Entrada:

data <- c(1.17,1.61,1.16,1.38,3.53,1.23,3.76,1.94,0.96,4.75,0.15,2.41,0.71,0.02,1.59,0.19,0.82,0.47,2.16,2.01,0.92,0.75,2.59,3.07,1.40)

# Cuantiles
q1 <- quantile(data,0.25)
q3 <- quantile(data,0.75)

# Cotas Interiores
Ii <- q1 - 1.5*(q3-q1)
Is <- q1 + 1.5*(q3-q1)

# Cotas Superiores
Ei <- q1 - 3*(q3-q1)
Es <- q1 + 3*(q3-q1)

# Filtrado de posibles valores fuera del intervalo
data2 <-  data[ data >= Ii & data <= q1]
data3 <-  data[data >= q3 & data <= Is]

# Filtrado de valores muy probables fuera del intervalo
data4 <-  data[ data >= Ei & data <= Ii]
data5 <-  data[data >= Is & data <= Es]

# Valores anomalos de las distintas cotas:
anomalos <- sort(c(data2,data3,data4,data5))

# Datos una vez filtrado todos los valores anomalos

ListaResultante <- sort(setdiff(data,anomalos))


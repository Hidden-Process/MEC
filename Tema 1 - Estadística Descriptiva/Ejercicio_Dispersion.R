# ¿Qué muestra presenta más dispersión?

# Para comparar la dispersión entre muestras distintas utilizamos el coeficiente de variación de Pearson:

peso <- c(65,60,65,63,68,70,66,71)
altura <- c(170,150,168,170,175,171,160,180)

CV_Peso <- (sqrt(mean(peso ^2) - mean(peso)^2) / mean(peso))
CV_Altura <- (sqrt(mean(altura ^2) - mean(altura)^2) / mean(altura))

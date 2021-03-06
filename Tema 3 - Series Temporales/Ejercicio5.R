# Datos: Evoluci�n del precio del azucar en esos a�os.
precio <- c(25,29,34,38,42,45,70,77)
tiempo <- c(1975:1982)

# Funci�n para calcular la relaci�n de precios tomando el primer dato como referencia, correspondiente
# a 1975 (Esto tambien se conoce como n�meros �ndices)

func <- function(vector){
  for(i in 1:length(vector)){
    vec2 <- vector[i] / vector[1]
    cat(sprintf("El valor del indice %i es %#.2f, lo que en porcentaje equivale a  %#.2f \n",i,vec2,(vec2-1)*100))
  }
}

func(precio)

# Vemos los valores indices indican un porcentaje de variaci�n creciente constante que aumenta en mayor 
# proporci�n cada a�o que pasa, lo que quiere decir que los precios han subido todos los a�os, aunque ese
# incremento es especialmente considerable en los �ltimos a�os llegando a subir un 180 o 208% en los 2
# ultimos a�os observados respecto al per�odo de referencia.
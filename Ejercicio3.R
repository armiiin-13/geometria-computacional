##################################################################################
# Clase 3: Algoritmos de ordenación 
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 09/02/2026 
# Propósito: Practicar el intercambio y ordenación de argumentos
##################################################################################
library(ggplot2)


# Definimos el primer algoritmo burbuja dado en clase 

#Intercambiar 2 posiciones
intercambio<-function(pair,i,j){
  aux=pair[i]
  pair[i]=pair[j]
  pair[j]=aux
  return(pair)
}
#comparar que elemento es mayor
larger <- function(pair)
{
  return(pair[1] > pair[2])
}
#intercambiar si se cumple que los elementos son uno mayor que el otro

swap_if_larger = function(pair)
{
  if(larger(pair))
  {
    return(rev(pair))
  }
  else
  {
    return(pair)
  }
}
#Ordenar el vector usando las anteriores
swap_pass = function(vec)
{
  for(i in seq(1, length(vec)-1))
  {
    vec[i:(i+1)] = swap_if_larger(vec[i:(i+1)])
  }
  return(vec)
}
#Algoritmo de la burbuja 
bubble_sort = function(vec)
{
  new_vec = swap_pass(vec)
  if(isTRUE
     (all.equal(vec, new_vec)))
  {
    return(new_vec)
  }
  else {
    return(bubble_sort(new_vec))
  }
}





##################################################################################
#Defino el algorimo de insercción 
#Algoritmo de la inserción
Insertion_sort = function(vec)
{
   for (i in seq(2,length(vec))) # le puse que empieza en dos para evitar una iteracion trivial 
  {
    actual= vec[i];
    j = i-1;
    #Desplazamiento de los elementos de lamatriz
    while((j>0) && (vec[j]>actual))
    {
      vec[j+1]=vec[j];
      j = j-1;
    }
    #insertarelelemento ensulugar
    vec[j+1]=actual;
  }
  return(vec);
}
test_vec = round(runif(10, 0, 100))
a<-Insertion_sort(test_vec)
test_vec
a[1]



#Defino el algortimo de seleccion 

Selection_sort = function(vec)
{
  for (i in 1:(length(vec)-1))
  {
    minimo= i;
    temp= vec[i];
    for(j in (i:length(vec)))
    {
      if(vec[j]<temp) {
        minimo= j;
        temp= vec[j];
      }
    }
    vec[minimo] = vec[i];
    vec[i] = temp;
  }
  return(vec);
}

 

#Defino un metodo de la burbuja más claro 
#Mi propia burbuja
miBur<- function(vec){
  for(i in seq(1, length(vec)-1)){
    cambio<-FALSE
    for (j in seq(1, length(vec)-i)) {
      if(vec[j]>vec[j+1]){
        aux=vec[j]
        vec[j]=vec[j+1]
        vec[j+1]=aux
        cambio<-TRUE
      }
      
    }
    if (!cambio){
      break
    }
  }
  return(vec)
}

#mini prueba de que funciona
a<-c(round(runif(100, 0, 100)))
miBur(a)

#############################################################################################################

#Ya tenemos los algoritmos, ahora vamos a hacer gráfica de tiempos
# de los 4 algortimos (buble, inserccion, seleccion , buble bueno)


options(expressions = 50000) #Me he visto obligado a introducir este comando 
#Sin el por la recursividad no se podría analizar bien el incremento



# Vectores vacíos para guardar los tiempos (rellenos de ceros al principio)
incremento<-c(10,100,500,1000,2000)
tiempos_b1 <- numeric(length(incremento))
tiempos_se <- numeric(length(incremento))
tiempos_in <- numeric(length(incremento))
tiempos_b2 <- numeric(length(incremento))
#bucle que nos de los tiempos 
for(i in 1:length(incremento)){
  #vector desordenado que irá aumentando tamaño
  vector_desordenado<-round(runif(incremento[i], 0, 100))
  #ordenamos el vector con los diferentes algoritmos y medimos el tiempo 
  tiempos_b1[i]<- system.time(bubble_sort(vector_desordenado))["elapsed"]
  tiempos_se[i]<- system.time(Selection_sort(vector_desordenado))["elapsed"]
  tiempos_in[i] <- system.time(Insertion_sort(vector_desordenado))["elapsed"]
  tiempos_b2[i] <- system.time(miBur(vector_desordenado))["elapsed"]
  
}

# Primero calculamos el tiempo máximo para mejorar la visibilidad
max_y <-  max(c(tiempos_b1,tiempos_b2, tiempos_se, tiempos_in))
max_x <- max(incremento)
# Creamos el plot
plot(incremento, tiempos_b1, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")

# Añadimos los otros tres
lines(incremento, tiempos_b1, type="b", col="red") 
lines(incremento, tiempos_b2, type="b", col="blue")
lines(incremento, tiempos_se, type="b", col="green")
lines(incremento, tiempos_in, type="b", col="orange")

# La leyenda
legend("topleft", legend=c("b1", "b2", "se", "in"),
       col=c("red", "blue", "green", "orange"), lty=1, pch=1)



###############################################################################################


#Como vemos en el anterior, no se nota mucha diferencia entre los algoritmos 
#que no son la versión mala de la burbuja, vamos a seguir sin la burbuja1.

# Vectores vacíos para guardar los tiempos (rellenos de ceros al principio)
incremento<-c(500,1000,2000,3000,4000,4250,4500,5000)
tiempos_se <- numeric(length(incremento))
tiempos_in <- numeric(length(incremento))
tiempos_b2 <- numeric(length(incremento))
#bucle que nos de los tiempos 
for(i in 1:length(incremento)){
  #vector desordenado que irá aumentando tamaño
  vector_desordenado<-round(runif(incremento[i], 0, 100))
  #ordenamos el vector con los diferentes algoritmos y medimos el tiempo 
  tiempos_se[i]<- system.time(Selection_sort(vector_desordenado))["elapsed"]
  tiempos_in[i] <- system.time(Insertion_sort(vector_desordenado))["elapsed"]
  tiempos_b2[i] <- system.time(miBur(vector_desordenado))["elapsed"]
  
}

# Primero calculamos el tiempo máximo para mejorar la visibilidad
max_y <-  max(c(tiempos_b2, tiempos_se, tiempos_in))
max_x <- max(incremento)
# Creamos el plot
plot(incremento, tiempos_b2, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos 2", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")
# Añadimos los datos
lines(incremento, tiempos_b2, type="b", col="blue")
lines(incremento, tiempos_se, type="b", col="green")
lines(incremento, tiempos_in, type="b", col="orange")

# La leyenda
legend("topleft", legend=c( "b2", "se", "in"),
       col=c( "blue", "green", "orange"), lty=1, pch=1)

################################################################################################



#Ahora juguemos con los rangos de los numeros y por ende el numero de repeticiones

#Disminuyendo las repeticiones 

# Vectores vacíos para guardar los tiempos (rellenos de ceros al principio)
incremento<-c(500,1000,2000,3000,4000,4250,4500,5000)
tiempos_se <- numeric(length(incremento))
tiempos_in <- numeric(length(incremento))
tiempos_b2 <- numeric(length(incremento))
#bucle que nos de los tiempos 
for(i in 1:length(incremento)){
  #vector desordenado que irá aumentando tamaño
  vector_desordenado<-round(runif(incremento[i], 0, 1000)) #se aumenta el rango de 100 a 1000
  #ordenamos el vector con los diferentes algoritmos y medimos el tiempo 
  tiempos_se[i]<- system.time(Selection_sort(vector_desordenado))["elapsed"]
  tiempos_in[i] <- system.time(Insertion_sort(vector_desordenado))["elapsed"]
  tiempos_b2[i] <- system.time(miBur(vector_desordenado))["elapsed"]
  
}
# Primero calculamos el tiempo máximo para mejorar la visibilidad
max_y <-  max(c(tiempos_b2, tiempos_se, tiempos_in))
max_x <- max(incremento)
# Creamos el plot
plot(incremento, tiempos_b2, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos menos repeticiones", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")
# Añadimos los datos
lines(incremento, tiempos_b2, type="b", col="lightblue")
lines(incremento, tiempos_se, type="b", col="lightgreen")
lines(incremento, tiempos_in, type="b", col="purple")

# La leyenda
legend("topleft", legend=c( "b2", "se", "in"),
       col=c( "lightblue", "lightgreen", "purple"), lty=1, pch=1)

#####################################################################################################################



#Aumentando las repeticiones 

# Vectores vacíos para guardar los tiempos (rellenos de ceros al principio)
incremento<-c(500,1000,2000,3000,4000,4250,4500,5000)
tiempos_se <- numeric(length(incremento))
tiempos_in <- numeric(length(incremento))
tiempos_b2 <- numeric(length(incremento))
#bucle que nos de los tiempos 
for(i in 1:length(incremento)){
  #vector desordenado que irá aumentando tamaño 
  vector_desordenado<-round(runif(incremento[i], 0, 20)) #se disminuye el rango 
  #ordenamos el vector con los diferentes algoritmos y medimos el tiempo 
  tiempos_se[i]<- system.time(Selection_sort(vector_desordenado))["elapsed"]
  tiempos_in[i] <- system.time(Insertion_sort(vector_desordenado))["elapsed"]
  tiempos_b2[i] <- system.time(miBur(vector_desordenado))["elapsed"]
  
}

# Primero calculamos el tiempo máximo para mejorar la visibilidad
max_y <-  max(c(tiempos_b2, tiempos_se, tiempos_in))
max_x <- max(incremento)
#Creamos el plot
plot(incremento, tiempos_b2, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos más repeticiones", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")
# Añadimos los datos
lines(incremento, tiempos_b2, type="b", col="darkblue")
lines(incremento, tiempos_se, type="b", col="darkgreen")
lines(incremento, tiempos_in, type="b", col="darkred")

# La leyenda
legend("topleft", legend=c( "b2", "se", "in"),
       col=c( "darkblue", "darkgreen", "darkred"), lty=1, pch=1)





#####################################################################################################


#Ahora vamos a hacer una verisón más profesional de almacenar los tiempos,
#aunque la idea de los gráficos es la misma.
#Como vamos a proceder, primero usaremos una mejor estructura para las gráficas con ggplot y dataframe
#Por otro lado mejoraremos la medida al graficar la media.
incremento<-c(2,500,1000,2000,3000,4000,4250,4500,4750,5000)
media<-function(algoritmo,incremento){
  tiempos<-numeric(length(incremento))
  for(i in 1:length(incremento)){
    total<-0
    for(j in 1:5){
      vector_desordenado<-round(runif(incremento[i], 0, 100))
      total<- total + system.time(algoritmo(vector_desordenado))["elapsed"]

    }
    tiempos[i]<-total/5
  }
  return(tiempos)
}

tiempos_se<- media(Selection_sort,incremento)
tiempos_in <- media(Insertion_sort,incremento)
tiempos_b2 <- media(miBur,incremento)

tiempos <- data.frame(
  Tamaño = rep(incremento, 3),
  Tiempo = c(tiempos_se, tiempos_in , tiempos_b2),
  Algoritmo = factor(rep(c("Seleccion","Insercion","Mi_Burbuja"),
                         each = length(incremento)))
)
ggplot(tiempos, aes(x=Tamaño, y=Tiempo, color=Algoritmo)) + # x e y me definen los ejes y coolor=Algoritmos me separa los datos y les pone colores distintos
  geom_line() + #conecta las medias entre ellas
  geom_point() + #Me marca los puntos
  labs(title="Comparación de algoritmos de ordenación",
       x="Tamaño del vector",
       y="Tiempo (segundos)") +
  # base de la gráfica
  theme_bw() + 
  # Pero quiero ajustarlo al medio el título
  theme(
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  )





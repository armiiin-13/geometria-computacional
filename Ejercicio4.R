##################################################################################
# Ejercicio 4: Algoritmos de ordenación 2
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 09/02/2026 
# Propósito: Seguir analizando los tipos de algoritmo de ordenación 
##################################################################################
library(ggplot2)
# The semantics of the variables are different, as are the comments, since they are from the previous exercise
# In the new version, we have also prioritized English
#only the grafics are in spanish for the memory
###########################Slow Algorithms#######################################################
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

#############Implementation of new algorithms#####################
mmerge<-function(vector1,vector2) { #in charge os combining ordered vectors
  combined<-numeric(length(vector1)+length(vector2)) #total length of the combined vectors
  vector1i<-1; vector2i<-1; j<-1;
  for(j in 1:length(combined)) {
    if((vector1i<=length(vector1) && vector1[vector1i]<vector2[vector2i]) || vector2i>length(vector2)) {
      combined[j] <- vector1[vector1i]
      vector1i <- vector1i+1
    } else {
      combined[j] <- vector2[vector2i]
      vector2i <- vector2i+1
    }
    
  }
  combined
}

mmergesort<-function(vector) {
  if(length(vector)>1) {
    half <- ceiling(length(vector)/2)
    primera <- mmergesort(vector[1:half])
    segunda <- mmergesort(vector[(half+1):length(vector)])
    mmerge( primera,segunda)
  } else {
    vector
  }
}

x<-c(18, 16, 8, 7, 23, 3, 11, 9, 15, 1)
mmergesort(x)

quickshort<-function(vector){
  if(length(vector)<=1){
    return(vector)
  }
  else{
    #i choose the pivot
    positionp <-ceiling(length(vector)/2)
    pivot<-vector[positionp]
    #i splice it
    minors<-vector[vector<pivot]
    equals<-vector[vector==pivot]
    mayors<-vector[vector>pivot]
    return(c(quickshort(minors),equals,quickshort(mayors)))
  }
}
a<-c(2,3,51,32,1)
quickshort(a)


# How we do the exchanges
exchanger <- function(vector, n, node) {
  #we start the root and its sons
  largest <- node        
  left <- 2 * node       
  right <- 2 * node + 1  
  
  # we check if we have to exchange it with any of its sons
  if (left <= n && vector[left] > vector[largest]) {
    largest <- left
  }
  
  if (right <= n && vector[right] > vector[largest]) {
    largest <- right
  }
  
  #In the case in which the root was not the largest we make a change and continue 
  if (largest != node) {
    temp <- vector[node]
    vector[node] <- vector[largest]
    vector[largest] <- temp
    vector <- exchanger(vector, n, largest) #just to make sure that all the branches are following our strategy
  }
  return(vector)
}

#  Heapsort
heapsort <- function(vector) {
  n <- length(vector)
  if (n <= 1) return(vector)
  
  # we order the vector and start our strategy
  for (node in floor(n / 2):1) {
    #we have used floor insted of ceiling for cases like n=5 in which with ceiling we would take the node 3
    # wich in this case, it would not have any sons
    vector <- exchanger(vector, n, node)
  }
  
  # we take the elements one by one, placing the root to the end and reducing the vector search
  for (i in n:2) {
    temp <- vector[1]
    vector[1] <- vector[i]
    vector[i] <- temp
    vector <- exchanger(vector, i - 1, 1)
  }
  return(vector)
}
############################################New comparisons#################################################################


#Firstly we are going to compare the slow algorithms with merge which is suposed to be the slowest from the new ones


options(expressions = 50000) #just in case for the recursivity 

# Vectores vacíos para guardar los time (rellenos de ceros al principio)
increment<-c(10,100,500,1000,2000)
time_merge <- numeric(length(increment))
time_se <- numeric(length(increment))
time_in <- numeric(length(increment))
time_b2 <- numeric(length(increment))



#loop for the time
for(i in 1:length(increment)){
  vector_disordered<-round(runif(increment[i], 0, 100))
  time_merge[i]<- system.time(mmergesort(vector_disordered))["elapsed"]
  time_se[i]<- system.time(Selection_sort(vector_disordered))["elapsed"]
  time_in[i] <- system.time(Insertion_sort(vector_disordered))["elapsed"]
  time_b2[i] <- system.time(miBur(vector_disordered))["elapsed"]
  
}

# max time
max_y <-  max(c(time_merge,time_b2, time_se, time_in))
max_x <- max(increment)
# Creation of the plot
plot(increment, time_merge, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")

lines(increment, time_merge, type="b", col="red") 
lines(increment, time_b2, type="b", col="blue")
lines(increment, time_se, type="b", col="green")
lines(increment, time_in, type="b", col="orange")

legend("topleft", legend=c("merge", "b2", "se", "in"),
       col=c("red", "blue", "green", "orange"), lty=1, pch=1)










#############################################################################################################


###############################################################################################


#as you will see the fastest is the merge
#So now we are going to compare the three new ones and the fastest in R of the slow ones, which was selection

increment<-c(500,1000,2000,3000,4000,4250,4500,5000)
time_se <- numeric(length(increment))
time_merge <- numeric(length(increment))
time_he <- numeric(length(increment))
time_qu <- numeric(length(increment))
#bucle que nos de los time 
for(i in 1:length(increment)){
  vector_disordered<-round(runif(increment[i], 0, 100))
  time_se[i]<- system.time(Selection_sort(vector_disordered))["elapsed"]
  time_merge[i] <- system.time(mmergesort(vector_disordered))["elapsed"]
  time_qu[i] <- system.time(quickshort(vector_disordered))["elapsed"]
  time_he[i] <- system.time(heapsort(vector_disordered))["elapsed"]
  
}

# Primero calculamos el tiempo máximo para mejorar la visibilidad
max_y <-  max(c(time_se,time_qu,time_he,time_merge))
max_x <- max(increment)
# Creamos el plot
plot(increment, time_se, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos 2", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")
# Añadimos los datos
lines(increment, time_se, type="b", col="green")
lines(increment, time_merge, type="b", col="blue")
lines(increment, time_qu, type="b", col="orange")
lines(increment, time_he, type="b", col="red")

# La leyenda
legend("topleft", legend=c( "se", "merge", "qu","he"),
       col=c(  "green","blue", "orange","red"), lty=1, pch=1)


#after this we will see that the selecction increments much faster 
#so now we are comparing only the 3 new ones
increment<-c(500,1000,2000,3000,4000,4250,4500,5000)
time_se <- numeric(length(increment))
time_merge <- numeric(length(increment))
time_he <- numeric(length(increment))
time_qu <- numeric(length(increment))
for(i in 1:length(increment)){
  vector_disordered<-round(runif(increment[i], 0, 100))
  time_merge[i] <- system.time(mmergesort(vector_disordered))["elapsed"]
  time_qu[i] <- system.time(quickshort(vector_disordered))["elapsed"]
  time_he[i] <- system.time(heapsort(vector_disordered))["elapsed"]
  
}
max_y <-  max(c(time_qu,time_he,time_merge))
max_x <- max(increment)

plot(increment, time_merge, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos 3", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")
lines(increment, time_merge, type="b", col="blue")
lines(increment, time_qu, type="b", col="orange")
lines(increment, time_he, type="b", col="red")

# La leyenda
legend("topleft", legend=c( "merge", "qu","he"),
       col=c(  "blue", "orange","red"), lty=1, pch=1)
################################################################################################

#now we are going to play with the repetitions
#less repetitions
increment<-c(500,1000,2000,3000,4000,4250,4500,5000)
time_merge <- numeric(length(increment))
time_qu <- numeric(length(increment))
time_he <- numeric(length(increment))
for(i in 1:length(increment)){
  vector_disordered<-round(runif(increment[i], 0, 1000)) #range from 100 to 1000
  time_merge[i] <- system.time(mmergesort(vector_disordered))["elapsed"]
  time_qu[i] <- system.time(quickshort(vector_disordered))["elapsed"]
  time_he[i] <- system.time(heapsort(vector_disordered))["elapsed"]
  
}
max_y <-  max(c(time_qu,time_he,time_merge))
max_x <- max(increment)

plot(increment, time_merge, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos menos repeticiones", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")

lines(increment, time_merge, type="b", col="lightblue")
lines(increment, time_qu, type="b", col="lightgreen")
lines(increment, time_he, type="b", col="purple")

legend("topleft", legend=c( "merge", "qu", "he"),
       col=c( "lightblue", "lightgreen", "purple"), lty=1, pch=1)

#####################################################################################################################



#More repetitions
increment<-c(500,1000,2000,3000,4000,4250,4500,5000)
time_merge <- numeric(length(increment))
time_qu <- numeric(length(increment))
time_he <- numeric(length(increment))
for(i in 1:length(increment)){

  vector_disordered<-round(runif(increment[i], 0, 20)) #range from 100 to 20
  time_merge[i] <- system.time(mmergesort(vector_disordered))["elapsed"]
  time_qu[i] <- system.time(quickshort(vector_disordered))["elapsed"]
  time_he[i] <- system.time(heapsort(vector_disordered))["elapsed"]
  
}

max_y <-  max(c(time_qu,time_he,time_merge))
max_x <- max(increment)
plot(increment, time_merge, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos más repeticiones", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")

lines(increment, time_merge, type="b", col="darkblue")
lines(increment, time_qu, type="b", col="darkgreen")
lines(increment, time_he, type="b", col="darkred")

legend("topleft", legend=c( "merge", "qu", "he"),
       col=c( "darkblue", "darkgreen", "darkred"), lty=1, pch=1)





#####################################################################################################


#Now we are going to do it in a more profesional way, by taking the media of time of several tries and using ggplot instead of plot
increment<-c(2,500,1000,2000,3000,4000,4250,4500,4750,5000)
media<-function(algoritmo,increment){
  time<-numeric(length(increment))
  for(i in 1:length(increment)){
    total<-0
    for(j in 1:5){
      vector_disordered<-round(runif(increment[i], 0, 100))
      total<- total + system.time(algoritmo(vector_disordered))["elapsed"]
      
    }
    time[i]<-total/5
  }
  return(time)
}

time_merge<- media(mmergesort,increment)
time_qu <- media(quickshort,increment)
time_he <- media(heapsort,increment)

time <- data.frame(
  Tamaño = rep(increment, 3),
  Tiempo = c(time_merge, time_qu , time_he),
  Algoritmo = factor(rep(c("Merge","Quick","Heap"),
                         each = length(increment)))
)
ggplot(time, aes(x=Tamaño, y=Tiempo, color=Algoritmo)) +
  geom_line() + 
  geom_point() + 
  labs(title="Comparación de algoritmos de ordenación",
       x="Tamaño del vector",
       y="Tiempo (segundos)") +
  theme_bw() + 
  theme(
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  )

############################################################################################################################################


#As we can see heap here in R increases its time much faster than the other ones, probablye due to the same reason as insertion and selection in the last practice
#That is why we are going to repeat some of the grafics only with quich and merge


#less repetitions
increment<-c(500,1000,2000,3000,4000,4250,4500,5000)
time_merge <- numeric(length(increment))
time_qu <- numeric(length(increment))
for(i in 1:length(increment)){
  vector_disordered<-round(runif(increment[i], 0, 1000)) #range from 100 to 1000
  time_merge[i] <- system.time(mmergesort(vector_disordered))["elapsed"]
  time_qu[i] <- system.time(quickshort(vector_disordered))["elapsed"]
  
}
max_y <-  max(c(time_qu,time_merge))
max_x <- max(increment)

plot(increment, time_merge, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos menos repeticiones", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")

lines(increment, time_merge, type="b", col="darkred")
lines(increment, time_qu, type="b", col="orange")

legend("topleft", legend=c( "merge", "qu"),
       col=c( "darkred", "orange"), lty=1, pch=1)


#More repetitions
increment<-c(500,1000,2000,3000,4000,4250,4500,5000)
time_merge <- numeric(length(increment))
time_qu <- numeric(length(increment))
for(i in 1:length(increment)){
  vector_disordered<-round(runif(increment[i], 0, 10)) #range from 100 to 10
  time_merge[i] <- system.time(mmergesort(vector_disordered))["elapsed"]
  time_qu[i] <- system.time(quickshort(vector_disordered))["elapsed"]
  
}
max_y <-  max(c(time_qu,time_merge))
max_x <- max(increment)

plot(increment, time_merge, type="n",xlim = c(0, max_x), ylim=c(0, max_y),
     main="Comparación de Algoritmos más repeticiones", xlab="Tamaño n", ylab="Segundos")
grid(nx = NULL, ny = NULL, col = "gray", lty = "solid")

lines(increment, time_merge, type="b", col="darkred")
lines(increment, time_qu, type="b", col="orange")

legend("topleft", legend=c( "merge", "qu"),
       col=c( "darkred", "orange"), lty=1, pch=1)




#Now we are going to do it in a more profesional way, by taking the media of time of several tries and using ggplot instead of plot
increment<-c(2,500,1000,2000,3000,4000,4250,4500,4750,5000)
media<-function(algoritmo,increment){
  time<-numeric(length(increment))
  for(i in 1:length(increment)){
    total<-0
    for(j in 1:5){
      vector_disordered<-round(runif(increment[i], 0, 100))
      total<- total + system.time(algoritmo(vector_disordered))["elapsed"]
      
    }
    time[i]<-total/5
  }
  return(time)
}

time_merge<- media(mmergesort,increment)
time_qu <- media(quickshort,increment)

time <- data.frame(
  Tamaño = rep(increment, 2),
  Tiempo = c(time_merge, time_qu),
  Algoritmo = factor(rep(c("Merge","Quick"),
                         each = length(increment)))
)
ggplot(time, aes(x=Tamaño, y=Tiempo, color=Algoritmo)) +
  geom_line() + 
  geom_point() + 
  labs(title="Comparación de algoritmos de ordenación",
       x="Tamaño del vector",
       y="Tiempo (segundos)") +
  theme_bw() + 
  theme(
    
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  )

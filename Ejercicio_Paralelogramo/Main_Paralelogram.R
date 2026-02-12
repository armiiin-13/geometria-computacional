##################################################################################
# Ejercicio 1: Paralelogramo
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 05/02/2026 
# Propósito: Calcular el área de un paralelogramo descomponiéndolo en triángulos 
##################################################################################

######### ALGORITMO 1: 4 PUNTOS ######################
#Compruebo que no haya puntos iguales 
isnotequals<- function(p1,p2,p3,p4){
  if(all(p1==p2)|all(p1==p3)|all(p1==p4)|all(p2==p3)|all(p2==p4)|all(p4==p3)){
    s<-FALSE
  }
  else{
    s<-TRUE
  }
  return(s)
}
#Compruebo que es un paralelogramo 
isParalelogram<- function(p1,p2,p3,p4){
  #ordeno bien los puntos para que no haya errores y asegurar tomar los lados correctos
  j<-matrix(c(p1,p2,p3,p4),nrow = 4,ncol=2,byrow = TRUE)
  mpuntos<- j[order(j[,1],j[,2]),]
  a<-mpuntos[1,]
  b<-mpuntos[2,]
  c<-mpuntos[3,]
  d<-mpuntos[4,]
  v1<-c(a-b)
  v2<-c(c-d)
  v3<-c(a-c)
  v4<-c(b-d)
  if((all(v1 == v2) || all(v1 == -v2)) && (all(v3 == v4) || all(v3 == -v4))){
    s<-TRUE
  }
  else{
    s<-FALSE
  }
  return(s)
}

#Altura de un triangulo
altura<- function(p1,p2,p3){
  v<-c(p2-p1)
  v_ort<-c(v[2],-v[1])
  A <- matrix(c(v, -v_ort), nrow=2, ncol=2, byrow=FALSE)
  B<-p3-p1
  solucion <- solve(A, B)
  pcorte<-v_ort *solucion[2] + p3
  valtura<-pcorte-p3
  return(sqrt(sum(valtura^2))) 
}

#Calculo el área 

area<- function(p1,p2,p3,p4){
  
  if(isnotequals(p1,p2,p3,p4)&isParalelogram(p1,p2,p3,p4)){
    #una vez sabemos que es paralelogramo da igual que 3 puntos tomemos
    h<-altura(p1,p2,p3)
    vbase<-p2-p1
    base<-sqrt(sum((vbase)^2))
    print(paste("El area del triangulo es base*h/2, lo cual es ", base*h/2  ))
    print(paste("Por tanto el area del paralelogramo es 2 veces eso ", base*h ))
    #solo los ordeno para la función plot
    j<-matrix(c(p1,p2,p3,p4),nrow = 4,ncol=2,byrow = TRUE)
    mpuntos<- j[order(j[,1],j[,2]),]
    a<-mpuntos[1,]
    b<-mpuntos[2,]
    c<-mpuntos[3,]
    d<-mpuntos[4,]
    x <- c(a[1], b[1],d[1],c[1])
    y <- c(a[2], b[2],d[2],c[2])
    t1<-c(p1[1],p2[1],p3[1])
    t2<-c(p1[2],p2[2],p3[2])
    plot(-5:5, -5:5, type = "n" ,main = paste("Poligono"))
    polygon(x, y, col = "yellow", lty = 1, lwd = 2, border = "black")
    polygon(t1, t2, col = "lightpink", lty = 1, lwd = 2, border = "lightblue")
    abline(h = -5:5, v = -5:5, col = "lightgray", lty = "dotted")
    return (base*h)
    
  }
  else{
    a<-"No es un paralelogramo"
  }
}

# Testing A1
##Ejemplo 0
p1<-c(0,0)
p2<-c(1, 0)
p3<-c(1,1)
p4<-c(0,1)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)

##Ejemplo 1 
p1<-c(1,0)
p2<-c(3.5 , 1)
p3<-c(2.5,2)
p4<-c(2,-1)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)

##Ejemplo 2 
p3<-c(4.5,-1)
p2<-c(0 , 0)
p1<-c(4.5,1)
p4<-c(0,2)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)


##Ejemplo 3
p3<-c(-1,2)
p2<-c(1, 2)
p1<-c(0,0)
p4<-c(0,4)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)

#ejemplo 4
p1<-c(-1,0)
p2<-c(1, 0)
p3<-c(2,4)
p4<-c(0,4)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)

##Ejemplo 5 no funciona por puntos iguales
p1<-c(1,0)
p2<-c(1, 0)
p3<-c(2,4)
p4<-c(2,4)
isnotequals(p1,p2,p3,p4)
isParalelogram(p1,p2,p3,p4) 
a<-area(p1,p2,p3,p4)
a

##Ejemplo 6 no funciona por no ser paralelogramo 4 puntos alineados
p3<-c(1,0)
p2<-c(2, 0)
p1<-c(4,0)
p4<-c(6,0)
isnotequals(p1,p2,p3,p4)
isParalelogram(p1,p2,p3,p4) 
a<-area(p1,p2,p3,p4)
a

##Ejemplo 7 no funciona por no ser paralelogramo
p1<-c(1,0)
p2<-c(0, 0)
p3<-c(1,1)
p4<-c(1,-1)
isnotequals(p1,p2,p3,p4)
isParalelogram(p1,p2,p3,p4) 
a<-area(p1,p2,p3,p4)
a

######### ALGORITMO 2: 2 VECTORES ######################
module <- function(v){
  # Comprobar que el argumento de entrada es un vector
  if (!is.vector(v)){
    stop("El argumento de la función debe ser un vector")
  }
  
  # es lo mismo que hacer sqrt(sum(v^2))
  counter <- 0
  n <- length(v)
  for (i in 1:n){
    counter <- counter + v[i]^2
  }
  
  return(sqrt(counter))
}

are_colineal <- function(v1, v2){
  if (v1[1] != 0 && v2[1] == 0){
    return(FALSE)
  }
  if (v1[1] == 0 && v2[1] == 0){
    if (length(v1) > 1){
      return(are_colineal(v1[-1], v2[-1]))
    } else {
      return(TRUE) # v1 y v2 son el vector nulo
    }
  }
  lambda <- v1[1]/v2[1]
  n = length(v1)
  for (i in 2:n){
    if (v1[i] != lambda * v2[i]){
      return(FALSE)
    }
  }
  return(TRUE)
}

dot_product <- function(v1, v2){
  # Ya se ha comprobado si son vectores de la misma longitud
  suma <- 0
  n <- length(v1)
  for (i in 1:n){
    suma <- suma + (v1[i] * v2[i])
  }
  # Código equivalente a sum(v1 * v2)
  return(suma)
}

area_paralelogram <- function(v1, v2){
  # Se comprueba que los dos argumentos son vectores de la misma longitud (no nulos)
  if (!is.vector(v1) || !is.vector(v2)){
    stop("Alguno de los argumentos no es un vector")
  }
  
  # Se comprueba que los vectores tienen la misma dimensión
  if (length(v1) != length(v2)){
    stop("Los vectores deben ser de la misma longitud")
  }
  
  # Se comprueba que los vectores no son nulos
  if (module(v1) == 0 || module(v2) == 0){
    stop("Alguno de los vectores es nulo")
  }
  
  # Se comprueba que los vectores no son colineales
  if (are_colineal(v1,v2)){
    stop("Los vectores son colineales")
  }
  
  # Los dos vectores forman un triangulo de área 1/2(|a||b| sen \alpha)
  # Se sabe que el ángulo \alpha se puede obtener como arccos(|a·b|/(|a||b|))
  alpha <- acos(abs(dot_product(v1, v2))/ (module(v1) * module(v2)))
  area_triangulo <- 0.5 * (module(v1) * module(v2) * sin(alpha))
  
  # El paralelogramo que determinan v1 y v2 son dos triángulos de los anteriores
  # unidos por uno de sus lados
  
  area_paralelogramo <- 2 * area_triangulo
  return(area_paralelogramo)
}

distance <- function(p1,p2){
  difference <- p2-p1
  n <- length(difference)
  sum <- 0
  for (i in 1:n){
    sum <- sum + difference[i]^2
  }
  return(sum)
}

get_triangles <- function(p1,p2,p3,p4){
  points <- list(p1,p2,p3,p4)
  
  # Encontrar los puntos que conforman la diagonal (los más alejados entre si)
  best_i <- 1
  best_j <- 2
  best_distance <- distance(p1,p2)
  
  for (i in 1:3){
    for (j in (i+1):4){
      value <- distance(points[[i]], points[[j]])
      if (value > best_distance){
        best_i <- i
        best_j <- j
        best_distance <- value
      }
    }
  }
  
  # best_i y best_j son los puntos de la diagonal
  others <- setdiff(1:4, c(best_i, best_j))
  
  triangle_1 <- cbind(points[[best_i]], points[[others[[1]]]], points[[best_j]])
  triangle_2 <- cbind(points[[best_i]], points[[others[[2]]]], points[[best_j]])
  
  return(list(t1 = triangle_1, t2 = triangle_2))
}

draw_triangle <- function(t, n){
  x <- c(t[1, ], t[1, 1])
  y <- c(t[2, ], t[2, 1])
  if (n == 1){ # Primer triángulo
    polygon(x, y, col='lightgoldenrod', border='navy', lwd=2)
  } else if (n == 2){ # Segundo Triángulo
    polygon(x, y, col='palegreen', border='navy', lwd=2)
  }
}

draw_paralelogram <- function(p, v1,v2){
  # p es el punto de origen de los dos vectores que componen el paralelogramo
  
  # Se comprueba que los vectores y el punto están en la misma dimensión
  if (!(length(v1) == length(v2) && length(v2) == length(p))){
    stop("Algún elemento no está en la dimensión correcta")
  }
  
  # Se comprueba que los vectores sean vectores
  if (!is.vector(v1) || !is.vector(v2)){
    stop("Alguno de los argumentos no es un vector")
  }
  
  # Se comprueba que los vectores no sean nulos
  if (module(v1) == 0 || module(v2) == 0){
    stop("Alguno de los vectores es nulo")
  }
  
  # Se comprueba que los vectores no son colineales
  if (are_colineal(v1,v2)){
    stop("Los vectores son colineales")
  }
  
  # Hallar los 4 vértices (p es uno de ellos)
  p1 = p + v1
  p2 = p + v2
  p3 = p + v1 + v2
  
  # Se obtiene los vectores de las x y de las y (para definir los límites del gráfico)
  matrix <- cbind(p, p1, p2, p3)
  x_vector <- matrix[1, ]
  y_vector <- matrix[2, ]
  
  # Se genera el plano del gráfico
  plot(0,0,type="n", xlim = c(min(x_vector) -1, max(x_vector)+1), ylim = c(min(y_vector) -1, max(y_vector)+1), main = paste("Paralelogramo"))
  
  # Se obtienen los dos triangulos que conforman el paralelogramo
  triangles <- get_triangles(p,p1,p2,p3)
  
  # Dibujar el paralelogramo (los dos triángulos)
  draw_triangle(triangles$t1,1)
  draw_triangle(triangles$t2,2)
}

# Testing 
v1 = c(3,2)
v2 = c(0,0) # vector nulo
v3 = c(-1, -5)
v4 = c(9,1)
v5 = c(-2,4)
v6 = c(6,4) # colineal con v1
p_origin = c(0,0)
p = c(3,2)

## Areas
area_paralelogram(v3,v2)
area_paralelogram(v1,v6)
area_paralelogram(v4,v5)
area_paralelogram(v1,v3)

## Drawings
draw_paralelogram(p_origin,v3,v2)
draw_paralelogram(p_origin,v1,v6)
draw_paralelogram(p_origin, v4, v5)
draw_paralelogram(p,v1,v3)
draw_paralelogram(p, c(5,2), c(-2,5)) # rombo
draw_paralelogram(p_origin, c(2,0), c(0,3)) #rectángulo
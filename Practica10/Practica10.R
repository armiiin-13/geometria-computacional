#############################################################################
# Práctica 10: Triangulación Denaulay
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 22/04/2026 
# Propósito: Triangular una nave de star wars siguien el nuevo método
##############################################################################
install.packages("readxl")
install.packages("deldir")
install.packages("tripack")
install.packages("plotrix")

library(deldir)
library(ggplot2)
library(plotrix)
library(readxl) 

###Lectura nave
datos <- read_excel("Practica10/navestarwars.xlsx")
head(datos)
x <- datos$x
y <- datos$y
plot(datos$x, datos$y)
V <- as.matrix(datos[, c("x", "y")])
V



###Triangulación vieja####
orientacion <- function(a, b, c) {
  return((b[1] - a[1]) * (c[2] - a[2]) - (b[2] - a[2]) * (c[1] - a[1]))
}

#función que verifica si un punto p está dentro del triángulo abc
punto_en_triangulo <- function(a, b, c, p) {
  o1 <- orientacion(a, b, p)
  o2 <- orientacion(b, c, p)
  o3 <- orientacion(c, a, p)
  
  return((o1 >= 0 && o2 >= 0 && o3 >= 0) || 
           (o1 <= 0 && o2 <= 0 && o3 <= 0))
}

#función que comprueba si tenemos una oreja
es_oreja <- function(V, i) {
  n <- nrow(V)
  
  prev <- V[(i - 2) %% n + 1, ]
  curr <- V[i, ]
  sig  <- V[i %% n + 1, ]
  #Debe ser convexo
  if (orientacion(prev, curr, sig) <= 0) return(FALSE)
  #Ningún otro punto dentro del triángulo
  for (j in 1:n) {
    if (j == i || j == ((i - 2) %% n + 1) || j == (i %% n + 1)) next
    if (punto_en_triangulo(prev, curr, sig, V[j, ])) return(FALSE)
  }
  
  return(TRUE)
}

#Algoritmo principal
triangulacion <- function(V) {
  V <- as.matrix(V)
  triangulos <- list()
  while (nrow(V) > 3) {
    n <- nrow(V)
    encontrado <- FALSE
    for (i in 1:n) {
      if (es_oreja(V, i)) {
        prev_i <- (i - 2) %% n + 1
        next_i <- i %% n + 1
        triangulos[[length(triangulos) + 1]] <- 
          rbind(V[prev_i, ], V[i, ], V[next_i, ])
        
        V <- V[-i, , drop = FALSE]
        encontrado <- TRUE
        break
      }
    }
    
    if (!encontrado) {
      stop("El polígono no es simple o hay error numérico")
    }
  }
  
  triangulos[[length(triangulos) + 1]] <- V
  return(triangulos)
}

# Funciones de Cálculo de Áreas
area_triangle <- function(tri) {
  x1 <- tri[1, 1]; y1 <- tri[1, 2]
  x2 <- tri[2, 1]; y2 <- tri[2, 2]
  x3 <- tri[3, 1]; y3 <- tri[3, 2]
  
  return(abs(
    x1 * (y2 - y3) +
      x2 * (y3 - y1) +
      x3 * (y1 - y2)
  ) / 2)
}

areas_triangles <- function(triangles) {
  sapply(triangles, area_triangle) # obtener todas las areas de todos los triangulos pasados
}

area_triangulation <- function(triangles) {
  sum(areas_triangles(triangles)) # suma de areas
}

#Vamos a plasmar la triangulación resultante del algoritmo
dibujar_triangulacion <- function(V, triangulos) {
  # Dibujar polígono original
  plot(V, type = "n", asp = 1, xlab = "", ylab = "", main = "Triangulación")
  polygon(V, border = "black", col = rgb(0.9, 0.9, 0.9, 0.3))
  colores <- rainbow(length(triangulos))
  for (i in seq_along(triangulos)) {
    tri <- triangulos[[i]]
    polygon(tri, col = adjustcolor(colores[i], alpha.f = 0.4), border = "blue")
    #los bordes
    lines(rbind(tri, tri[1, ]), col = "blue", lwd = 2)
  }
  #Los vertices
  points(V, pch = 19, col = "red")
  text(V, labels = 1:nrow(V), pos = 3, col = "red")
}

resultado<-triangulacion(V)
area_total <- area_triangulation(resultado)
print(area_total)
dibujar_triangulacion(V, resultado)


####Nueva triangulación

x <- datos$x
y <- datos$y
dxy1 <- deldir(x,y,dpl=NULL, rw=NULL, plotit=TRUE)
#Extraer triangulación
tri <- triang.list(dxy1)
#Mostrar resultados
print(tri)
#Graficar puntos marcados
plot(dxy1)
points(x, y, col = "red", pch = 19)
#Sin los puntos marcados
plot(dxy1)



# centroides de la triangulacion
l<-tile.list(dxy1)
g<-tile.centroids(l)
plot(l,close=TRUE)
points(g,pch=20,col="red")

triangles_deldir <- lapply(tri, function(t) {
  matrix(
    c(t$x, t$y),
    ncol = 2
  )
})

areas_deldir <- areas_triangles(triangles_deldir)
area_total_deldir <- sum(areas_deldir)
area_total_deldir

# Nave P1 -> Delauney
datos_p1 <- read_excel("Practica_1_StarWars/DataSet.xlsx")
head(datos_p1)
x <- datos_p1$x
y <- datos_p1$y
V <- as.matrix(datos_p1[, c("x", "y")])

## Delauney Triangulacion
x <- datos_p1$x
y <- datos_p1$y
dxy1_p1 <- deldir(x,y,dpl=NULL, rw=NULL, plotit=TRUE)
tri_p1 <- triang.list(dxy1_p1)
print(tri_p1)
plot(dxy1_p1)
points(x, y, col = "red", pch = 19)
plot(dxy1_p1)
l_p1<-tile.list(dxy1_p1)
g_p1<-tile.centroids(l_p1)
plot(l_p1,close=TRUE)
points(g_p1,pch=20,col="red")
triangles_deldir_p1 <- lapply(tri_p1, function(t) {
  matrix(
    c(t$x, t$y),
    ncol = 2
  )
})
areas_deldir_p1 <- areas_triangles(triangles_deldir_p1)
area_total_deldir_p1 <- sum(areas_deldir_p1)
area_total_deldir_p1
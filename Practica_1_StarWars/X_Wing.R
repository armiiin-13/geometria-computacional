##################################################################################
# Práctica 1: Nave de Star Wars
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 22/02/2026 
# Propósito: Calcular el área de una nave de Star Wars descomprimiéndola en triángulos
##################################################################################

# ---- PACKAGE INSTALLATION ----
## Only execute these lines if the are not loaded

install.packages("readxl")
library(readxl) # to use the library methods

# ---- PROCESS FIGURE FUNCTIONS ----
## The process functions creates the points of the figure and calculates its area and
## draws the figure on the plot. It returns the area calculated to be able to use
## in the main function

#Previus methos for using measument functions

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
############################################################################





# ---- PROCESS FIGURE FUNCTIONS ----
process_triangle <- function(vector){
  
}

process_paralelogram <- function(vector){
  
}

# ---- AREA CALCULATION FUNCTIONS ----
area_triangle <- function(p1,p2,p3){
    #Asumimos que se nos da un triangulo
    h<-altura(p1,p2,p3)
    vbase<-p2-p1
    base<-sqrt(sum((vbase)^2))
    
    return (base*h/2)
  
}

area_paralelogram<- function(p1,p2,p3,p4){
  
  if(isnotequals(p1,p2,p3,p4)&isParalelogram(p1,p2,p3,p4)){
    #una vez sabemos que es paralelogramo da igual que 3 puntos tomemos
    h<-altura(p1,p2,p3)
    vbase<-p2-p1
    base<-sqrt(sum((vbase)^2))
    
    return (base*h)
    
  }
  else{
    a<-"No es un paralelogramo"
  }
}
area_circle<- function(n,r){ # r radio y n divisiones
  total<-0
  for(i in 1:n){
    p1<-c(0,0)
    p2<-c(r*cos((i-1)*2* pi /(n)),r*sin((i-1)*2* pi /(n)))
    p3<-c(r*cos(i*2* pi /(n)),r*sin(i*2* pi /(n)))
    total=total+area_triangle(p2,p3,p1)
  }
  return(total)
}

# ---- DRAW FIGURES FUNCTIONS ----
draw_triangle <- function(p1,p2,p3){
    #una vez sabemos que es paralelogramo da igual que 3 puntos tomemos
    #ordeno para la función plot
    j<-matrix(c(p1,p2,p3),nrow = 3,ncol=2,byrow = TRUE)
    mpuntos<- j[order(j[,1],j[,2]),]
    t1<-c(p1[1],p2[1],p3[1])
    t2<-c(p1[2],p2[2],p3[2])
    polygon(t1, t2, col = "lightgreen", lty = 1, lwd = 2, border = "purple")
}

draw_paralelogram <- function(p1,p2,p3,p4){
  if(isnotequals(p1,p2,p3,p4)&isParalelogram(p1,p2,p3,p4)){
    #una vez sabemos que es paralelogramo da igual que 3 puntos tomemos
    #ordeno para la función plot
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
    polygon(x, y, col = "yellow", lty = 1, lwd = 2, border = "black")
    polygon(t1, t2, col = "lightpink", lty = 1, lwd = 2, border = "lightblue")
    
  }
  else{
    print("No es un paralelogramo")
  }
}

draw_circle <- function(n,r){
  for(i in 1:n){
    p1<-c(0,0)
    p2<-c(r*cos((i-1)*2* pi /(n)),r*sin((i-1)*2* pi /(n)))
    p3<-c(r*cos(i*2* pi /(n)),r*sin(i*2* pi /(n)))
    t1<-c(p1[1],p2[1],p3[1])
    t2<-c(p1[2],p2[2],p3[2])
    polygon(t1, t2, col = "lightpink", lty = 1, lwd = 2, border = "lightblue")
  }
}

process_triangle <- function(vector,i){
  # positions 1 to 6 need to be numeric
  if (anyNA(vector[1:6])){
    stop(paste("Error on line ",i,": missing triangle coordinates"))
  }
  
  ## Create points
  p1 <- c(vector[1], vector[2])
  p2 <- c(vector[3], vector[4])
  p3 <- c(vector[5], vector[6])
  
  ## Draw triangle
  draw_triangle(p1,p2,p3)
  
  ## Returns the area of the triangle
  return(area_triangle(p1,p2,p3))
  
}

process_paralelogram <- function(vector,i){
  # positions 1 to 8 need to be numeric
  if (anyNA(vector[1:8])){
    stop(paste("Error on line ",i,": missing paralelogram coordinates"))
  }

  ## Create points
  p1 <- c(vector[1], vector[2])
  p2 <- c(vector[3], vector[4])
  p3 <- c(vector[5], vector[6])
  p4 <- c(vector[7], vector[8])
  
  ## Draw paralelogram
  draw_paralelogram(p1,p2,p3,p4)
  
  ## Returns the area of the paralelogram
  return(area_paralelogram(p1,p2,p3,p4))
}

# ---- AREA CALCULATION FUNCTIONS ----
area_triangle <- function(p1,p2,p3){

}

area_paralelogram <- function(p1,p2,p3,p4){

}

# ---- DRAW FIGURES FUNCTIONS ----
draw_triangle <- function(p1,p2,p3){

}

draw_paralelogram <- function(p1,p2,p3,p4){

}

# ---- MAIN FUNCTION ----
area_spaceship <- function(path){ 
  # path = path to the xlsx file containing the spaceship reference points
  df <- suppressMessages(read_xlsx(path, col_names = FALSE, col_types = "numeric"))
  
  # check if data is empty
  if (nrow(df) == 0){
    stop("Dataframe is empty")
  }
  
  # create empty plot for the figure
  plot(0, 0,
       type = "n",        
       xlim = c(0, 100),  
       ylim = c(0, 100),
       asp = 1, 
       xlab = "X", ylab = "Y")
  
  # initialize area --> accumulates the calculated area of the spaceship
  area <- 0
  
  # read each row of data and process it
  for (i in (1:nrow(df))){ # for each row
    df_row <- unlist(df[i, 1:8]) # creates the vector of the i row (8 elements maximum)
    
    # stablish case triangle vs paralelogram
    if (is.na(df_row[7])){ # could be a triangle 
      area <- area + process_triangle(df_row,i)
    } else { # could be a paralelogram
      area <- area + process_paralelogram(df_row,i)
    }
  }
  print(paste("The X-wing has an area of ", area, " m^2"))
}

area_spaceship("Practica_1_StarWars/DataTest.xlsx")

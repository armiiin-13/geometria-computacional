##################################################################################
# Práctica 1: Paralelogramo
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 05/02/2026 
# Propósito: Calcular el área de un paralelogramo descomponiéndolo en triángulos
##################################################################################

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


#ejemplo 1 
p1<-c(1,0)
p2<-c(3.5 , 1)
p3<-c(2.5,2)
p4<-c(2,-1)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)

#ejemplo 2 
p3<-c(4.5,-1)
p2<-c(0 , 0)
p1<-c(4.5,1)
p4<-c(0,2)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)


#ejemplo 3
p3<-c(-1,2)
p2<-c(1, 2)
p1<-c(0,0)
p4<-c(0,4)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)

#ejemplo 4
p3<-c(-1,0)
p2<-c(1, 0)
p1<-c(2,4)
p4<-c(0,4)
isParalelogram(p1,p2,p3,p4)
area(p1,p2,p3,p4)









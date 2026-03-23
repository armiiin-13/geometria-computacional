##################################################################################
# Ejercicio 5: Transformaciones Afines
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 09/02/2026 
# Propósito: Trabajar con conceptos de geometría afín en R
##################################################################################
library("plot3D")
#install.packages("rgl")
library(rgl)

x=c(0:2) 
y=c(0:2)
plot(x,y, main="Sistema de referencia", col="white") 
text(0,0,'o') 
arrows(0,0,1,0.5,col="green" ,lwd=3) 
arrows(0,0,0.5,1,col="blue" , lwd=3)

CambioReferCanonico <-function(p,q,v1,v2)
{
  p_new<-c(0,0)
  p_new[1]=(p[1]*v2[2]-p[2]*v2[1]+q[2]*v2[1]-q[1]*v2[2])/(v1[1]*v2[2]-v1[2]*v2[1])
  p_new[2]=(p[1]*v1[2]-p[2]*v1[1]+q[2]*v1[1]-q[1]*v1[2])/(v1[1]*v2[2]-v1[2]*v2[1])
  p_new
}

CambioReferCanonico(c(2,3),c(1,1),c(0.5,1),c(1,-0.5))





#Traslacion 2d#########################
Traslacion<-function(p,q)
{
  p_new<-c(0,0)
  p_new[1]=p[1]+q[1]
  p_new[2]=p[2]+q[2]
  x=c(p[1],q[1])
  y=c(p[2],q[2])
  a1=min(p[1],q[1],p_new[1]) - 1
  a2=max(p[1],q[1],p_new[1]) +1
  b1=min(p[2],q[2],p_new[2]) -1
  b2=max(p[2],q[2],p_new[2]) +1 
  #El plot
  plot3d(0,0,type="n",xlim=c(a1,a2),ylim=c(b1,b2),
       main="Traslacion",xlab="x",ylab="y")
  
  #punto original
  points(p[1],p[2],col="black",pch=19)
  text(p[1],p[2],"p",pos=3)
  
  #Trasladado
  points(p_new[1],p_new[2],col="red",pch=19)
  text(p_new[1],p_new[2],"p trasladado",col="red",pos=3)
  
  #Dibujo el vector de la traslación
  arrows(p[1],p[2],p_new[1],p_new[2],col="blue",lwd=2)
  p_new
}
Traslacion(c(2,3),c(1,1))


#Traslacion 3d#########################
Traslacion3d1<-function(p,q)
{
  p_new<-c(0,0,0)
  p_new[1]=p[1]+q[1]
  p_new[2]=p[2]+q[2]
  p_new[3]=p[3]+q[3]
  
  x=c(p[1],q[1])
  y=c(p[2],q[2])
  z=c(p[3],q[3])
  a1=min(p[1],q[1],p_new[1]) - 1
  a2=max(p[1],q[1],p_new[1]) +1
  b1=min(p[2],q[2],p_new[2]) -1
  b2=max(p[2],q[2],p_new[2]) +1 
  c1=min(p[3],q[3],p_new[3]) -1
  c2=max(p[3],q[3],p_new[3]) +1
  #The plot
  scatter3D(0,0,0,type="n",xlim=c(a1,a2),ylim=c(b1,b2),zlim=c(c1,c2),
       main="Traslación",xlab="x",ylab="y",zlab="z")
  
  #punto original
  points3D(p[1],p[2],p[3],col="black",pch=19)
  text3D(p[1],p[2],p[3],"p",pos=3)
  
  #trasladado
  points3D(p_new[1],p_new[2],p_new[3],col="red",pch=19)
  text3D(p_new[1],p_new[2],p_new[3],"p trasladado",col="red",pos=3)
  
  #vector de traslación
  arrows3D(p[1],p[2],p[3],p_new[1],p_new[2],p_new[3],col="blue",lwd=2)
  p_new
}
Traslacion3d1(c(2,1,-1),c(2,1,-1))

#Traslacion 3d 2 ###############
Traslacion3d <- function(p, q) {
  p_new <- p + q
  
  #límites
  a1 = min(p[1], q[1], p_new[1]) - 1
  a2 = max(p[1], q[1], p_new[1]) + 1
  b1 = min(p[2], q[2], p_new[2]) - 1
  b2 = max(p[2], q[2], p_new[2]) + 1 
  c1 = min(p[3], q[3], p_new[3]) - 1
  c2 = max(p[3], q[3], p_new[3]) + 1
  
  #entorno
  open3d()
  #fijamos los límites
  plot3d(0, 0, 0, type="n", xlim=c(a1, a2), ylim=c(b1, b2), zlim=c(c1, c2),
         xlab="x", ylab="y", zlab="z")
  
  #Punto original
  points3d(p[1], p[2], p[3], col="black", size=10)
  text3d(p[1], p[2], p[3], texts="p", adj=c(0.5, -1))
  
  #Punto trasladado
  points3d(p_new[1], p_new[2], p_new[3], col="red", size=10)
  text3d(p_new[1], p_new[2], p_new[3], texts="p trasladado", col="red", adj=c(0.5, -1))
  
  lines3d(x = c(p[1], p_new[1]), 
          y = c(p[2], p_new[2]), 
          z = c(p[3], p_new[3]), 
          col="blue", lwd=3)
  
  title3d(main="Traslacion movible")
  
  return(p_new)
}

Traslacion3d(c(2, 1, -1), c(2, 1, -1))

# Simetria respecto a plano suponemos que c=1  z= ax+by +d y movible#################################
Simetria<-function(p,r)
{
  d = r(0,0)
  a= r(1,0)-d
  b=r(0,1)-d
  c=-1 #Porque en implicito 0= ax+by-z +d
  vu = c(a,b,c)/sqrt(a^2+b^2+c^2) #normalizo el vector normal
  R = matrix(c(c(1-2*vu[1]^2,-2*vu[2]*vu[1],-2*vu[1]*vu[3],0),c(-vu[1]*2*vu[2],1-2*vu[2]^2,-2*vu[2]*vu[3],0),c(-2*vu[1]*vu[3],-2*vu[2]*vu[3],1-2*vu[3]^2,0),c(0,0,0,1)), nrow=4, ncol=4, byrow=TRUE)
  p0   = c(0,0,r(0,0)) # ax+by+cz+d=0 punto en el plano
  T2 = matrix(c(c(1,0,0,p0[1]),c(0,1,0,p0[2]),c(0,0,1,p0[3]),c(0,0,0,1)), nrow=4, ncol=4, byrow=TRUE)
  T1 = matrix(c(c(1,0,0,-p0[1]),c(0,1,0,-p0[2]),c(0,0,1,-p0[3]),c(0,0,0,1)), nrow=4, ncol=4, byrow=TRUE)
  M = T2%*%R%*%T1
  p_ = M%*%c(p[1],p[2],p[3],1) #aplico la transformación
  p_new=c(p_[1],p_[2],p_[3])
  a1=min(p[1],p_new[1])
  a2=max(p[1],p_new[1])
  b1=min(p[2],p_new[2]) #primero tomo estas referencias para las franjas x, y 
  b2=max(p[2],p_new[2])
  x=c(a1-2:a2+2)
  y=c(b1-2:b2+2)
  z<-r(x,y)
  a1 = min(p[1], p_new[1]) - 1
  a2 = max(p[1], p_new[1]) + 1
  b1 = min(p[2], p_new[2]) - 1 #las amplio e incluyo el eje z para el dibujo 
  b2 = max(p[2], p_new[2]) + 1 
  c1 = min(p[3],p_new[3]) - 1
  c2 = max(p[3], p_new[3]) + 1
  open3d()
  plot3d(0, 0, 0, type="n", xlim=c(a1, a2), ylim=c(b1, b2), zlim=c(c1, c2),
         xlab="x", ylab="y", zlab="z")
  #Punto original
  points3d(p[1], p[2], p[3], col="black", size=10)
  text3d(p[1], p[2], p[3], texts="p", adj=c(0.5, -1))
  
  #Punto simetrico
  points3d(p_new[1], p_new[2], p_new[3], col="red", size=10)
  text3d(p_new[1], p_new[2], p_new[3], texts="p simetrico", col="red", adj=c(0.5, -1))
  
  lines3d(x = c(p[1], p_new[1]), 
          y = c(p[2], p_new[2]), 
          z = c(p[3], p_new[3]), 
          col="blue", lwd=3)
  
  # Secuencias
  x_seq <- seq(a1, a2, length.out = 20)
  y_seq <- seq(b1, b2, length.out = 20)
  z_matrix <- outer(x_seq, y_seq, Vectorize(r))
  
  #trasposicion para interpretar bien la orientación
  z_matrix <- t(z_matrix)
  
  #el plano
  surface3d(x_seq, y_seq, z_matrix, color="lightblue", alpha=0.4)
  surface3d(x_seq, y_seq, z_matrix, color="blue", front="lines", back="lines", alpha=0.8)
  title3d(main="Simetria movible")

  
  #para verlo sin el plano
  open3d()
  plot3d(0, 0, 0, type="n", xlim=c(a1, a2), ylim=c(b1, b2), zlim=c(c1, c2),
         xlab="x", ylab="y", zlab="z")
  #Punto original
  points3d(p[1], p[2], p[3], col="black", size=10)
  text3d(p[1], p[2], p[3], texts="p", adj=c(0.5, -1))
  
  #Punto simetrico
  points3d(p_new[1], p_new[2], p_new[3], col="red", size=10)
  text3d(p_new[1], p_new[2], p_new[3], texts="p simetrico", col="red", adj=c(0.5, -1))
  
  lines3d(x = c(p[1], p_new[1]), 
          y = c(p[2], p_new[2]), 
          z = c(p[3], p_new[3]), 
          col="blue", lwd=3)
  

  title3d(main="Simetria movible sin plano")
  p_new
}

Simetria(c(1,1,1),function(x,y) 2*x-2*y+4)












#Rotacion 3d y se mueve#########################



Rotacion<-function(p,theta,eje) #respecto al origen
{  #matriz del movimiento
  
  if(eje!= 1 & eje != 2& eje != 3){
    stop("Eje no válido")
  }
  else{
    if(eje==1){#rotación eje x
    M = matrix(c(
      c(1, 0, 0),
      c(0, cos(theta), -sin(theta)),
      c(0, sin(theta), cos(theta))
    ), nrow=3, ncol=3, byrow=TRUE)
    
  }
  if (eje==2){#eje y
    M= matrix(c(
      c(cos(theta), 0, sin(theta)),
      c(0, 1, 0),
      c(-sin(theta), 0, cos(theta))
    ), nrow=3, ncol=3, byrow=TRUE)
    }
  if (eje==3){#eje z
    
    M = matrix(c(
      c(cos(theta), -sin(theta), 0),
      c(sin(theta), cos(theta), 0),
      c(0, 0, 1)
    ), nrow=3, ncol=3, byrow=TRUE)
    }
    p_new=as.vector(M%*%p)
    #límites
    a1 = min(p[1],p_new[1]) - 1
    a2 = max(p[1],  p_new[1]) + 1
    b1 = min(p[2], p_new[2]) - 1
    b2 = max(p[2],  p_new[2]) + 1 
    c1 = min(p[3], p_new[3]) - 1
    c2 = max(p[3],  p_new[3]) + 1
    
    #entorno
    open3d()
    #fijamos los límites
    plot3d(0, 0, 0, type="n", xlim=c(a1, a2), ylim=c(b1, b2), zlim=c(c1, c2),
           xlab="x", ylab="y", zlab="z")
    
    #Punto original
    points3d(p[1], p[2], p[3], col="black", size=10)
    text3d(p[1], p[2], p[3], texts="p", adj=c(0.5, -1))
    
    #Punto trasladado
    points3d(p_new[1], p_new[2], p_new[3], col="red", size=10)
    text3d(p_new[1], p_new[2], p_new[3], texts="p rotado", col="red", adj=c(0.5, -1))
    
    lines3d(x = c(p[1], p_new[1]), 
            y = c(p[2], p_new[2]), 
            z = c(p[3], p_new[3]), 
            col="blue", lwd=3)
    
    title3d(main="Rotacion movible")
  p_new
  }
}

Rotacion(c(1,1,1),pi/2,2)


#Homotecia 3d y movible######################################

Homotecia<-function(p,centro,k){
  p1=c(p,1)
  M = matrix(c(
    c(k, 0, 0, (1 - k)*centro[1]),
    c(0, k, 0, (1 - k)*centro[2]),
    c(0, 0, k, (1 - k)*centro[3]),
    c(0, 0, 0, 1)
  ), nrow = 4, ncol = 4, byrow = TRUE)
  p_ = as.vector(M%*%p1)
  p_new= c(p_[1],p_[2],p_[3])
  #límites
  a1 = min(p[1],p_new[1],centro[1]) - 1
  a2 = max(p[1],  p_new[1],centro[1]) + 1
  b1 = min(p[2], p_new[2],centro[2]) - 1
  b2 = max(p[2],  p_new[2],centro[2]) + 1 
  c1 = min(p[3], p_new[3],centro[3]) - 1
  c2 = max(p[3],  p_new[3],centro[3]) + 1
  
  #entorno
  open3d()
  #fijamos los límites
  plot3d(0, 0, 0, type="n", xlim=c(a1, a2), ylim=c(b1, b2), zlim=c(c1, c2),
         xlab="x", ylab="y", zlab="z")
  
  #Punto original
  points3d(p[1], p[2], p[3], col="black", size=10)
  text3d(p[1], p[2], p[3], texts="p", adj=c(0.5, -1))
  #centro
  points3d(centro[1], centro[2], centro[3], col="green", size=10)
  text3d(p_new[1], p_new[2], p_new[3], texts="pcentro", col="green", adj=c(0.5, -1))
  
  #Punto trasladado
  points3d(p_new[1], p_new[2], p_new[3], col="red", size=10)
  text3d(p_new[1], p_new[2], p_new[3], texts="p aplicada homecia", col="red", adj=c(0.5, -1))
  
  lines3d(x = c(p[1], p_new[1]), 
          y = c(p[2], p_new[2]), 
          z = c(p[3], p_new[3]), 
          col="blue", lwd=3)
  
  title3d(main="Homotecia movible")
  p_new
}
Homotecia(c(1,1,1),c(1,5,1),4)




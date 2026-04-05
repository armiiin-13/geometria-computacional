##################################################################################
# Practica: Análisis de Factorial
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 05/02/2026 
# Propósito: aprender lo que no se nos enseño en estadistica
##################################################################################

# Instalar las librerías necesarias
install.packages("psych")
install.packages("EFAtools")
install.packages("GPArotation")
library(psych)
library(EFAtools)
library(readxl)

leer <- read_excel("Estudiantes.xlsx")
M <- leer[, -1]  # quita la primera columna

#Resumen de datos
summary(M)
#Busquemos indicios de posibles correlaciones
cor_M <- cor(M)
round(cor_M,2) #Meramente estetico por simplificar 



# Test de esfericidad de Bartlett
BARTLETT(M)
#Nos dará el valor de chi cuadrado y del p valor, en base a eso podremos decidir
#con los datos de nuestro excel nos dará un valor de chi extremo y un p valor significante por lo que podremos continuar con el análisis

#Vamos a comprobar los valores KMO
KMO(M)
#Nos sale ademas un KMO mediocre por lo que vamos a intentar mejorar

#Análisis sin rotar ni comprimir 
#princomp( ): análisis de componentes principales sin rotar
Modelo1 <- princomp(M, cor=TRUE)
# Varianza de cada factor
summary(Modelo1 ) 


#Nos sale  que tenemos 4 autovalores mayores que 1 por lo que vamos a considerar 4 componentes

# Puntuaciones en los factores
loadings(Modelo1)
# Gráfico de sedimentación
plot(Modelo1,type="lines")
fa.parallel(M, fa="fa")

#Vamos a seguir con un modelo con rotación y reduciendo dimensiones a las 4 componentes
library(GPArotation) 
Modelo2 <- principal(M, nfactors=4, rotate="varimax")
# print results
Modelo2
# Varianza de cada factor
summary(Modelo2)
# Puntuaciones en los factores
loadings(Modelo2)
# Puntuaciones de los casos
Modelo2 $scores
biPlot(Modelo2)

Modelo34<- factanal(M, 4, rotation="varimax")
print(Modelo34, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- Modelo34$loadings[,1:2]
# Configurar panel de 3 filas x 2 columnas para 6 combinaciones
par(mfrow=c(3,2))

# 1. Factor1 vs Factor2
plot(Modelo34$loadings[,1:2], type="n", main="Factor 1 vs Factor 2")
text(Modelo34$loadings[,1:2], labels=names(M), cex=0.7)

# 2. Factor1 vs Factor3
plot(Modelo34$loadings[,c(1,3)], type="n", main="Factor 1 vs Factor 3")
text(Modelo34$loadings[,c(1,3)], labels=names(M), cex=0.7)

# 3. Factor1 vs Factor4
plot(Modelo34$loadings[,c(1,4)], type="n", main="Factor 1 vs Factor 4")
text(Modelo34$loadings[,c(1,4)], labels=names(M), cex=0.7)

# 4. Factor2 vs Factor3
plot(Modelo34$loadings[,c(2,3)], type="n", main="Factor 2 vs Factor 3")
text(Modelo34$loadings[,c(2,3)], labels=names(M), cex=0.7)

# 5. Factor2 vs Factor4
plot(Modelo34$loadings[,c(2,4)], type="n", main="Factor 2 vs Factor 4")
text(Modelo34$loadings[,c(2,4)], labels=names(M), cex=0.7)

# 6. Factor3 vs Factor4
plot(Modelo34$loadings[,c(3,4)], type="n", main="Factor 3 vs Factor 4")
text(Modelo34$loadings[,c(3,4)], labels=names(M), cex=0.7)

#Vamos ahora a ver que pasa si reducimos a 3 
library(GPArotation) 
Modelo23 <- principal(M, nfactors=3, rotate="varimax")
# print results
Modelo23
# Varianza de cada factor
summary(Modelo23)
# Puntuaciones en los factores
loadings(Modelo23)
# Puntuaciones de los casos
Modelo23 $scores
biPlot(Modelo23)

# Modelo con 3 factores
Modelo33 <- factanal(M, 3, rotation="varimax")
print(Modelo33, digits=2, cutoff=.3, sort=TRUE)

# Configurar panel de 3 filas x 1 columna para 3 gráficos
par(mfrow=c(3,1))  # también puedes usar c(1,3) para una fila

# 1. Factor 1 vs Factor 2
plot(Modelo33$loadings[,1:2], type="n", main="Factor 1 vs Factor 2")
text(Modelo33$loadings[,1:2], labels=names(M), cex=0.7)

# 2. Factor 1 vs Factor 3
plot(Modelo33$loadings[,c(1,3)], type="n", main="Factor 1 vs Factor 3")
text(Modelo33$loadings[,c(1,3)], labels=names(M), cex=0.7)

# 3. Factor 2 vs Factor 3
plot(Modelo33$loadings[,c(2,3)], type="n", main="Factor 2 vs Factor 3")
text(Modelo33$loadings[,c(2,3)], labels=names(M), cex=0.7)




#ahora tengo en cuenta el KMO

M2 <- M[, !(names(M) %in% c("Expect", "Expert", "Friendly"))]
KMO(M2)


#Resumen de datos
summary(M2)
#Busquemos indicios de posibles correlaciones
cor_M2 <- cor(M2)
round(cor_M2,2) #Meramente estetico por simplificar 


BARTLETT(M2)

#Análisis sin rotar ni comprimir 
#princomp( ): análisis de componentes principales sin rotar
Modelo12 <- princomp(M2, cor=TRUE)
# Varianza de cada factor
summary(Modelo12 ) 

# Puntuaciones en los factores
loadings(Modelo12)
# Gráfico de sedimentación
plot(Modelo12,type="lines")
fa.parallel(M2, fa="fa")

#lo realizo sobre las dos componentes con autovalores mayores
ModeloKMO <- principal(M2, nfactors=2, rotate="varimax")
# print results
ModeloKMO
# Varianza de cada factor
summary(ModeloKMO)
# Puntuaciones en los factores
loadings(ModeloKMO)
# Puntuaciones de los casos
ModeloKMO $scores
biPlot(ModeloKMO)


Modelo3KMO<- factanal(M2, 2, rotation="varimax")
print(Modelo3KMO, digits=2, cutoff=.3, sort=TRUE)
# plot factor 1 by factor 2
load <- Modelo3KMO$loadings[,1:2]
# 1. Factor1 vs Factor2
plot(load, type="n", main="Factor 1 vs Factor 2")
text(load,labels=names(M2),cex=.7) # add variable names


# packages instalation
install.packages("readxl")
library(readxl) # to use the library methods

# FUNCTIONS
draw_triangle <- function(p1, p2, p3) { # basic (just to try)
  polygon(
    x = c(p1[1], p2[1], p3[1]),
    y = c(p1[2], p2[2], p3[2]),
    border = "blue",
    col = NA
  )
}

draw_paralelogram <- function(p1, p2, p3, p4) { # basic (just to try)
  polygon(
    x = c(p1[1], p2[1], p3[1], p4[1]),
    y = c(p1[2], p2[2], p3[2], p4[2]),
    border = "red",
    col = NA
  )
}

area_triangle <- function(p1,p2,p3){
  return(1)
}

area_paralelogram <- function(p1,p2,p3,p4){
  return(2)
}

# import data
data <- read_xlsx("Practica_1_StarWars/DataTest.xlsx", col_names = FALSE, col_types = "numeric")

# work with data
if (nrow(data) == 0){
  stop("Dataframe is empty")
}

area <- 0 #accumulates the area calculated 

# create plot (empty)
plot(0, 0,
     type = "n",        # no dibuja puntos
     xlim = c(0, 10),  # ajusta a tu rango real
     ylim = c(0, 10),
     asp = 1,           # escala igual en ejes
     xlab = "X", ylab = "Y")

for (i in (1:nrow(data))){ # for each row
  row <- unlist(data[i, 1:8]) # creates the vector of the i row (8 elements maximum)
  
  # stablish case triangle vs paralelogram
  if (is.na(row[7])){ # could be a triangle 
    # positions 1 to 6 need to be numeric
    if (anyNA(row[1:6])){
      stop(paste("Error on line ",i,": missing triangle coordinates"))
    }
    
    # Case: Triangle
    ## Create points
    p1 <- c(row[1], row[2])
    p2 <- c(row[3], row[4])
    p3 <- c(row[5], row[6])
    
    ## Add area to global calculation
    area <- area + area_triangle(p1,p2,p3)
    
    ## Draw triangle
    draw_triangle(p1,p2,p3)
    
  } else { # could be a paralelogram
    # positions 1 to 8 need to be numeric
    if (anyNA(row[1:8])){
      stop(paste("Error on line ",i,": missing paralelogram coordinates"))
    }
    
    # Case: Paralelogram
    ## Create points
    p1 <- c(row[1], row[2])
    p2 <- c(row[3], row[4])
    p3 <- c(row[5], row[6])
    p4 <- c(row[7], row[8])
    
    ## Add area to global calculation
    area <- area + area_paralelogram(p1,p2,p3,p4)
    
    ## Draw paralelogram
    draw_paralelogram(p1,p2,p3,p4)
  }
}
print(area)
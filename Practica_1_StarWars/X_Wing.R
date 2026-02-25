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
##################################################################################
# Clase 5: Transformaciones Lineales Afines a Figuras 2D 
# Autores: Rodrigo López Torres y Arminda García Moreno
# Fecha : 12/03/2026 
# Propósito: Realizar transformaciones lineales afines a figuras 2D y comprobar el resultado
##################################################################################

# Source - https://stackoverflow.com/a/22266105
# Posted by Gregor Thomas, modified by community. See post 'Timeline' for change history
# Retrieved 2026-03-12, License - CC BY-SA 4.0

# initialize a plot
plot(c(-5, 5), c(-5, 5), type = "n", asp = 1)

# prepare "circle data"
radius = 1
center_x = 0
center_y = 0
theta = seq(0, 2 * pi, length = 200) # angles for drawing points around the circle

# draw the circle
lines(x = radius * cos(theta) + center_x, y = radius * sin(theta) + center_y)

img <- readPNG(system.file("img", "Rlogo.png", package="png"))

plot(NA, xlim = c(0, 2), ylim = c(0, 4), type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rasterImage(img, 1, 3, 2, 4)
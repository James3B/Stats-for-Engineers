###Sample - Solving Differential Equation using Euler's method

library(deSolve)
Proceso <- function(t,y, params)
  
  {
 
  dy1 <- 0.5 * y[1] + 0.5 * y[2]
  dy2 <- (3.14 - y[1] - 0.4)* 3.924
  list(c(dy1, dy2))

}


yini <- c(y1 = 2, y2 = 0)
tiempo <- seq(0,1000,0.1)
out <- ode(times = tiempo, y =yini, func = Proceso, parms = NULL)

plot(out)
###inicia  de tiempo y la Y - lista
### resultatos - usa funccion ode
###usa el plot - para grafica de los resultados

###Tambien puedes usas el scatterplot 3D

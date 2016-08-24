##- Solving Differential Equation using Euler's method

library(deSolve)
Proceso <- function(t,y, params)
  
{
  
  dy1 <- A * y[1] + B * y[2]
  
  
  list(c(dy1, dy2))
  
}


yini <- c(y1 = 2, y2 = 0, e = 0, sumE = 0)
t <- 0.1
tiempo <- seq(0,1000,t)
out <- ode(times = tiempo, y =yini, func = Proceso, parms = c(m=1, k=10, c= 0.2, A= matrix(data = 1,0,k/m,-c/m, nrow = 2,ncol = 2,byrow= TRUE), B=c(0,1/m),C=c(1,0),y1=y1+dy1*t,oldE = err, differr = e - oldE, sumE = e+sumE))

plot(out)


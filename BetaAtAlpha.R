

BetaAtAlpha<- function(x,y,z,zeta){
  y - sqrt((1-zeta)*(y-x)*(y-z))
  
}

BetaFracAlpha <- function(Lq,y,z,zeta){
  
  z-(((1-zeta)/Lq)* (z-y))
}
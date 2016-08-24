
##returns the expected value of a Triangle distribution 

F5ExpValue <- function(x,y,z)
{
  (x+y+z)/3
}

F5ExpFract<- function(abcs,segs)
{
  EX <- 0
  for(i in 1:length(segs))
  {
EX <- segs[i] *(abcs[i]+((abcs[i+1]-abcs[i])/2))+ EX
  }
  EX
}

##

hist.Returns <- function(hData)
{
#x <- dim(hData)[1]
# x <- x-1
  for(i in 1:dim(hData)[1]) ## each row
  {
    for(j in 1:dim(hData)[2]) ## each column
    {
      hData[i,j] = (hData[i+1, j] - hData[i,j])/hData[i,j]
    }
  }
   hData <- hData[-i,]
   hData 
  }

No.ShortsReturns <- function(Markovitz){
    x<- length(Markovitz)
    No.Shorts <- matrix(nrow = 1, ncol = x)
    for (i in 1:dim(Markovitz)[2]) 
     {
      No.Shorts[1,i] <- (Markovitz[1,i]- Markovitz[x,i])/Markovitz[x,i]
     }
   No.Shorts
}


ShortsReturns <- function(Markovitz){
  x<- length(Markovitz)
  Shorts <- matrix(nrow = 1, ncol = x)
  for (i in 1:dim(Markovitz)[2]) 
  {
    Shorts[1,i] <- (Markovitz[x,i] - Markovitz[1,i])/Markovitz[1,i]
  }
  Shorts
}


Cov.Matrix <- function(hMdata)
  {
  x <- ncol(hMdata)
  y <- x
  Mtrx <- matrix(nrow = x, ncol = y)
  
    for(i in 1:x) ## each row
  {
   for(j in y:1)## each column
    {
      if (j == i){
       Mtrx[i,j]<- var(hMdata[,j])
     }else
     if (j > i){
          Mtrx[i,j]<- cov(hMdata[,j],hMdata[,j])
        } 
     else{
             Mtrx[i,j]<- 0
        }
    }
  }
  Mtrx
}

ERR <- function(St,r)
{
  Er <- matrix(nrow = 1, ncol = dim(r)[2])
  Er <- 0
  for(i in 1:dim(r)[1]) ## each row
  {
    for(j in 1:dim(r)[2]) ## each column
    {
      Er[i,1] = r[i,j]*St[1,j] +  Er[i,1]
    } 
  }
}
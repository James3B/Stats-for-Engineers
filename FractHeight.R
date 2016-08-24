

FractHeight<- function(abcs,segs)
{
  qf <- segs
  for(i in 1:length(segs))
    {
    qf[i] <- (segs[i])/(abcs[i+1]-abcs[i])
  }
  qf
}

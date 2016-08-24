
readinteger <- function(String)
{ 
  n <- readline(prompt= String)
  if(!grepl("^[0-9]+$",n))
  {
    return(readinteger())
  }
  
  return(as.integer(n))
}


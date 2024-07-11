### ONLY past your predict.y function here
predict.y <- function(x)
{
  load("fit_params.Rdata")
  x <- as.matrix(x)
  beta <- as.matrix(beta)
  #print(dim(x))
  #print(dim(beta))
  #print(beta)
  if(dim(x)[2] == 1){
    x <- t(x)
  }
  f.x <- x%*%beta
  return(f.x)
}

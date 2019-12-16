LogLiklihoodGivenSigmaAndAlpha = function(data,sigma,alpha){
  b= c()
  for(i in 1:nrow(data)){
  a = exp(-sigma*sum(data[i,]))*prod(data[i,]^((alpha/4)-1))
  b = append(b,a)
  }
  log_liklihood = prod(b)
  log_liklihood

  return(log_liklihood)
}




GaussianDensity = function(X, mu, cov){
#@description
#evaluates the liklihood of x[j] for an ith component distrbution
#@
#@parameters
#n: number, alpha_vector: vector of probabilities
#@function_body
  n = ncol(X)
  diff = t(sweep(X,2,mu))
  
  
  A = 1 / ((2 * pi) ^ (n / 2) %*% det(cov) ^ 0.5) 
  B = (-0.5 * ((t(diff) %*% solve(cov)) %*% diff))
  x = exp(B) * as.vector(A)
  x = diag(x)
  x = as.matrix(x,nrow = nrow(x),ncol = 1)
  
  return(x)
}

#End GaussianDensity.r

#test case
#X = data[1:5,]

#X= matrix(c(0.05, 1.413, 0.212, 0.85, -0.3, 1.11, 11.1, 0.4, 1.5, 0.27, 0.12, 1.44, 88, 12.33, 1.44),nrow = 5,ncol=3,byrow = TRUE)
#mu = colMeans(X)
#cov= (t(sweep(X,2,mu))) %*% (sweep(X,2,mu)) / (nrow(X) - 1)

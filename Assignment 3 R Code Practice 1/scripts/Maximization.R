Maximization = function(X, components) {
  #@description 
  #
  #@function_body
  N = nrow(data)
  number_of_components = length(components$pi)
  
  for (i in 1:number_of_components){
    gamma_nk = components$gamma_nk[[i]]
    cov_k = matrix(0,nrow = ncol(data),ncol = ncol(data))
    N_k = sum(gamma_nk)
    
    pi_k = N_k / N
    mu_k = colSums(sweep(X,1,gamma_nk,FUN = '*')) / N_k
    
    for (j in 1:nrow(data)){
      diff = (X[j,] - mu_k)
      cov_k = cov_k + (gamma_nk[j] * (diff %*% t(diff)))
    }
    
    cov_k = cov_k / N_k
    
    components$pi[[i]] = pi_k
    components$mu[[i]] = mu_k
    components$covariance[[i]] = cov_k
  }
  return(components)
}
#END Maximization.r
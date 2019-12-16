Expectation = function(X, components) {
#@description 
#Takes the expectaton of pi1,pi2,pi3,pi4 given the data the the current pi vector
#Uses Dirichlet-Multinomial pmf from wikipedia
#assumes that x_i > 0{x_i > 0} and x_1 + ... + x_k = 1, and zero otherwise. 
#@function_body
  
  denominator = matrix(0, nrow = nrow(data),ncol=1)
  gamma_K = list()
  number_of_components = length(components$pi)
  
  for (i in 1:number_of_components){
    pi_k = components$pi[[i]]
    mu_k = components$mu[[i]]
    cov_k = components$covariance[[i]]
    
    gamma_nk = (pi_k * GaussianDensity(X, mu_k, cov_k))
    
    for (j in 1:nrow(X)){
      denominator[j] = denominator[j] + gamma_nk[j]
    }
    
    components$gamma_nk[[i]] = gamma_nk
    components$denominator[[1]] = denominator
  }
  
  
  for (k in 1:number_of_components){
    components$gamma_nk[[k]] = components$gamma_nk[[k]] / components$denominator[[1]]
  }
  
  return(components)
}
#END Expectation.r


DirichletCMF = function(alpha_vector,n){
#@description
#Simulates exponentials using the inverse CDF function
#@
#@parameters
#n: scalar, sample_size: numeric, lambda: scalar
#@function_body
  unique_x_vectors = GenerateUniqueXVectors(n, alpha_vector)
  cmf = c()
  for (i in 1:length(unique_x_vectors)) {
    pmf = DirichletMultinomialPMF(n, alpha_vector, unique_x_vectors[[i]])
    cmf = append(cmf, sum(pmf))
  }
  
  
  return(cmf)  
}

#End DirichletCMF.r
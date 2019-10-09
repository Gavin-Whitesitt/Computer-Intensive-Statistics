DirichletCMF = function(alpha_vector,n){
#@description
#Simulates exponentials using the inverse CDF function
#@
#@parameters
#n: scalar, sample_size: numeric, lambda: scalar
#@function_body
  unique_x_vectors = GenerateUniqueXVectors(n, alpha_vector)
  cmf = c()
  pmf_vector = c()
  for (i in 1:length(unique_x_vectors)) {
    pmf = DirichletMultinomialPMF(n, alpha_vector, unique_x_vectors[[i]])
    pmf_vector = append(pmf_vector, pmf)
    cmf = append(cmf, sum(pmf_vector))
  }
  
  lookup_table = list("cmf" = cmf, "x_vector" = unique_x_vectors)
  
  return(lookup_table)  
}

#End DirichletCMF.r
DirichletCMF = function(alpha_vector,n){
#@description
#Generates the cmf for a Multinomial Dirchlet distribution for a given alpha vector and n
#@
#@parameters
#n: number, alpha_vector: vector of probabilities
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
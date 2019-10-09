RDirchletMultinomial = function(n,alpha_vector){
  #@description
  #Generates a random variable from the Dirchlet Multimonial Distribution 
  #@
  #@parameters
  #lookup_table(A list with 2 entries of equal size: lookup_table$x_vector: a list of vectors of x variables assosiated with
  #the cmf of the corresponding entry, lookup_table$cmf: a vector of probabilities assosiated with a particular cmf )
  #@function_body
  lookup_table = DirichletCMF(alpha_vector, n)
  RV = LookUpToRV(lookup_table)
  return(RV)
}

#End RDirchletMultinomial.r
LookUpToRV = function(lookup_table){
  #@description
  #Creates a random variable from a lookup table using a uniform random variable
  #@
  #@parameters
  #lookup_table(A list with 2 entries of equal size: lookup_table$x_vector: a list of vectors of x variables assosiated with
  #the cmf of the corresponding entry, lookup_table$cmf: a vector of probabilities assosiated with a particular cmf )
  #@function_body
  u = runif(1,0,1)
  RV_index = min(which(lookup_table$cmf >= u))
  RV = lookup_table$x_vector[RV_index]
  RV = RV[[1]]
  return(RV)  
}

#End LookUpToRV.r
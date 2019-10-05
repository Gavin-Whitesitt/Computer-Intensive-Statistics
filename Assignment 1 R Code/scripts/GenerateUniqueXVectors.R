GenerateUniqueXVectors = function(n, alpha_vector) {
  #@description 
  #Generates all unique x vectors that sum to 100 through brute force
  #@function_body
  
  x_vector = c(0,0,0)
  unique_x_vectors = list()
  cmf = c()
  for(k in 0:n){
    for (j in 0:n) {
      for (i in 0:n){
        x_vector[1] = j
        x_vector[2] = i
        x_vector[3] = k
        x_vector_list = list(x_vector)
        if (sum(x_vector) == n){
          unique_x_vectors = append(unique_x_vectors, x_vector_list)
        }
      }
    }
  }
  
  #attempt to vectorize for k alphas
  
  
  
  
  
  return(unique_x_vectors)
}
#END GenerateUniqueXVectors.r
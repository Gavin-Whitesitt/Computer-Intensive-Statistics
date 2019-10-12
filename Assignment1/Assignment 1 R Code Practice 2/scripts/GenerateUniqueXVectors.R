GenerateUniqueXVectors = function(n, alpha_vector) {
  #@description 
  #Generates all unique x vectors that sum to a given n through brute force
  #@function_body
  
  #x_vector = c(0,0,0)
  #unique_x_vectors = list()
  #cmf = c()
  #for(k in 0:n){
  #  for (j in 0:n) {
  #    for (i in 0:n){
  #      x_vector[1] = j
  #      x_vector[2] = i
  #      x_vector[3] = k
  #      x_vector_list = list(x_vector)
  #      if (sum(x_vector) == n){
  #        unique_x_vectors = append(unique_x_vectors, x_vector_list)
  #      }
  #    }
  #  }
  #}
  
  #Vectorized version of above, works for any size alpha
  
  
  l <- rep(list(0:n), length(alpha_vector))
  unique_x_vectors = expand.grid(l)
  unique_x_vectors = unique_x_vectors[which(rowSums(unique_x_vectors) == n),]
  
  unique_x_vectors <- as.list(as.data.frame(t(unique_x_vectors)))
  
  return(unique_x_vectors)
}
#END GenerateUniqueXVectors.r
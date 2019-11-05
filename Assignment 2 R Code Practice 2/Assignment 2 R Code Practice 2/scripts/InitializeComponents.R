InitializeComponents = function(mu_matrix, data) {
  #@description 
  #
  #@function_body
  current_pi_vector <- c(.25,.25,.25,.25) #start with equal probabilites of membership to each component
  
  #randomly assigns current_mu_matrix a uniform value between the minimum and maximum observed value for
  #that particular dimension
  current_mu_matrix <- matrix(NA,nrow = nrow(mu_matrix),ncol = ncol(mu_matrix))
  for (i in 1:nrow(mu_matrix)){
    for (j in 1:ncol(mu_matrix)){
      current_mu_matrix[i,j] = runif(1, min(data[,i]), max(data[,i]))
    }
  }
  
  #assigns the covariance matrix for each  component to the identity matrix 
  current_covariance_matrix = list()
  for (i in 1:ncol(mu_matrix)){
    current_covariance_matrix[[i]] <- diag(nrow(mu_matrix))
  }
  
  components = list()
  for (i in 1:4){
    components$pi[[i]] = current_pi_vector[i]
    components$mu[[i]] = current_mu_matrix[,i]
    components$covariance[[i]] = current_covariance_matrix[[i]]
  }
  
  return(components)
}
#END InitializeComponents.r
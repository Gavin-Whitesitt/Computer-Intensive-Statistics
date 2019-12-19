GenerateDataFromProposedThetas = function(theta_proposal){
  
  data_from_theta = matrix(NA,nrow = 26, ncol =64)
  row.names(data_from_theta)=c('A', 'B', 'C', 'D', 'E', 'F', 'G' ,'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O','P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')
  for(i in 1:nrow(data_from_theta)){
    data_from_theta[i,] = c(rep(1,theta_proposal[i]),rep(0,ncol(data_from_theta)-theta_proposal[i]))
  }
  return(data_from_theta)
}

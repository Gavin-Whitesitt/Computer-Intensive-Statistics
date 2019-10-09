DirichletMultinomialPMF = function(n, alpha_vector,x_vector) {
#@description 
#Evaluates the Dirichlet probability mass function for a given x_vector, alpha_vector and n
#Uses Dirichlet-Multinomial pmf from wikipedia
#assumes that x_i > 0{x_i > 0} and x_1 + ... + x_k = 1, and zero otherwise. 
#@function_body
  
  #Broken down into form (A/B)*prod(C/D) for readability 
  A <- factorial(n)*gamma(sum(alpha_vector))
  B <- gamma(n + sum(alpha_vector))
  C <- gamma(x_vector+alpha_vector)
  D <- factorial(x_vector)*gamma(alpha_vector)
  
  pmf = (A/B) * prod(C/D)
  
  return(pmf)
}
#END DirichletMultinomialPMF.r

#@Gavin's first two attempts at implementing directly from the density equation on wikipedia: for documentation 
#both were unsucessfull, not sure why
#pmf = gamma(sum(alpha_vector)) * ((gamma(alpha_vector))^-1) * x_vector^(alphas -1)
#pmf = (factorial(n) * gamma(sum(alpha_vector))/gamma(n+sum(alpha_vector)))* prod(gamma((unique_x_vectors[[i]]+alpha_vector)/(factorial(unique_x_vectors[[i]])*gamma(alpha_vector))))

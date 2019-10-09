#@description
#STAT 565, Assignment 1
#@author
#Gavin Whitesitt: gavinw@uidaho.edu, Colby Bland: cbland@uidaho.edu, Dylan Hull-Nye dhullnye@uidaho.edu
#@param NONE
#@return NONE
#@last_change 10\5\2019
#@paths
base_dir <- "C:\\Users\\gwhit\\Desktop\\Gavin\\School\\Graduate\\3rd Semester\\Student\\STAT 565\\Assignment1\\Assignment 1 R Code"
script_dir <- paste0(base_dir,"\\scripts")
output_dir <- paste0(base_dir,"\\output")

#----------
#@libraries_and_functions
source(paste0(script_dir, "\\DirichletCMF.r")) #Inverse CDF method to simulate 
source(paste0(script_dir,"\\DirichletMultinomialPMF.r"))
source(paste0(script_dir,"\\GenerateUniqueXVectors.r"))
#@seeds
set.seed(42) #for obvious reasons :)
#----------
#input parameters
n = 100
alpha_vector= c(1,5,2)
#output parameters


#----------
#code_body

lookup_table = DirichletCMF(alpha_vector, n)

lookup_table$cmf[1]
lookup_table$x_vector[2]    

LookUpToRV = function(lookup_table){
  u = runif(1,0,1)
  RV_index = min(which(lookup_table$cmf >= u))
  RV = lookup_table$x_vector[RV_index]
  RV = RV[[1]]
  return(RV)
}

n_simulations = 10000
simulated_x_vectors = matrix(nrow = n_simulations,ncol = 3)
for(i in 1:n_simulations){
  simulated_x_vector = LookUpToRV(lookup_table)
  simulated_x_vectors[i,1] = simulated_x_vector[[1]]
  simulated_x_vectors[i,2] = simulated_x_vector[[2]]
  simulated_x_vectors[i,3] = simulated_x_vector[[3]]
}

library(gtools)
library(DirichletReg)
which(rowSums(simulated_x_vectors) != 100)

#function says that some rows don't sum up to 1 (that is because they all sum to 100)
plot(DR_data(simulated_x_vectors), a2d = list(colored = TRUE, c.grid = FALSE, col.scheme = c("entropy")))


library(plotly)
plot_ly(x = simulated_x_vectors[,1],y = simulated_x_vectors[,2], z = simulated_x_vectors[,3], type = 'mesh3d')
add_surface(p) 


hist(cmf)
max(cmf)
mean(cmf)
min(cmf)

parameters[[1]] = N
parameters[[2]] = n
parameters[[3]] = lambda_max

data[[1]] = lambda
data[[2]] = x
data[[3]] = z
data[[4]] = p 

save(parameters, file = paste0(outputDir,"\\parameters.csv"))

save(data, file = paste0(outputDir, "\\datafile"))

#END main.r
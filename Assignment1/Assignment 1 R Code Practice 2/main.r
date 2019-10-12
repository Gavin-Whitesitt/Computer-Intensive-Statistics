#@description
#STAT 565, Assignment 1
#@author
#Gavin Whitesitt: gavinw@uidaho.edu, Colby Bland: cbland@uidaho.edu, Dylan Hull-Nye dhullnye@uidaho.edu
#@param NONE
#@return NONE
#@last_change 10\5\2019
#@paths
base_dir <- "C:\Users\gwhit\Desktop\Gavin\School\Graduate\3rd Semester\Student\STAT 565\Assignment1\Assignment 1 R Code Practice 2"
script_dir <- paste0(base_dir,"\\scripts")
output_dir <- paste0(base_dir,"\\output")

#----------
#@libraries_and_functions
source(paste0(script_dir, "\\DirichletCMF.r")) #Inverse CDF method to simulate 
source(paste0(script_dir,"\\DirichletMultinomialPMF.r"))
source(paste0(script_dir,"\\GenerateUniqueXVectors.r"))
source(paste0(script_dir,"\\LookUpToRV.r"))
source(paste0(script_dir,"\\RDirchletMultinomial.r"))

#these are for plotting results of 2a
library(gtools)
library(DirichletReg)
#@seeds
set.seed(42) #for obvious reasons :)
#----------
#input parameters
n = 100
alpha_vector= c(1,5,2)
#output parameters


#----------
#code_body



#Problem 2 a) ------------------------------------------------------------------------

#generates a RV from the Multivariate Polya Urn (Multivariate Dirchlet) but
#creates lookup table each time (don't run thousands of times, unless you like to wait)
RDirchletMultinomial(n,alpha_vector) 


#Creates one lookup table for a particular pair of n and a unique alpha vector 
#and then generates n_simulations from that particular distribution
n_simulations = 100000
simulated_x_vectors = matrix(nrow = n_simulations,ncol = 3)
lookup_table = DirichletCMF(alpha_vector, n)
for(i in 1:n_simulations){
  simulated_x_vector = LookUpToRV(lookup_table)
  simulated_x_vectors[i,1] = simulated_x_vector[[1]]
  simulated_x_vectors[i,2] = simulated_x_vector[[2]]
  simulated_x_vectors[i,3] = simulated_x_vector[[3]]
}

which(rowSums(simulated_x_vectors) != n) #just a check to make sure all generated random variables sum to n

#function says that some rows don't sum up to 1 (that is because they all sum to 100, we can then think of
#the sides of the triangle as proportions of balls drawn of each respective color)
plot(DR_data(simulated_x_vectors), a2d = list(colored = TRUE, c.grid = FALSE, col.scheme = c("entropy")))

#Problem 2 b) ------------------------------------------------------------------------
#RDirchletMultinomial works for any size n and any size alpha vector as long as you don't run into a memory error
#a few test cases are provided below to demonstrate capability
#If you want additional simulation proof I recommend calling the DirichletCMF for a given alpha vector and n
#outside of the simulation loop and then using the LookUpToRV within to funciton to draw from given distribution

n = 50 #A different n is provided here than above
alpha_vector = c(.3,.8,.4) #notice each alpha does not have to be a whole number

RDirchletMultinomial(n,alpha_vector) 

n = 20 #once again a different n
alpha_vector = c(1,2,3,4,5) #this time with 5 alphas given

RDirchletMultinomial(n,alpha_vector) 

n = 42
alpha_vector = c(1.1,2.4,5,10) #this time 4 alphas are given with some that are not integers and some that are

RDirchletMultinomial(n,alpha_vector) 

#END main.r
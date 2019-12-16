#@description
#STAT 565, Assignment 3
#@author
#Gavin Whitesitt: gavinw@uidaho.edu, Colby Bland: cbland@uidaho.edu, Dylan Hull-Nye dhullnye@uidaho.edu
#@param NONE
#@return NONE
#@last_change 12\13\2019
#@paths
base_dir <- "C:\\Users\\gwhit\\Desktop\\Gavin\\School\\Graduate\\3rd Semester\\Student\\STAT 565\\Assignment3\\Assignment 3 R Code Practice 1"
script_dir <- paste0(base_dir,"\\scripts")
output_dir <- paste0(base_dir,"\\output")

#----------
#@libraries_and_functions
source(paste0(script_dir, "\\LogLiklihoodGivenSigmaAndAlpha.r")) #Inverse CDF method to simulate 
source(paste0(script_dir,"\\Target.r"))
source(paste0(script_dir,"\\ConstantOfIntegration.r"))
source(paste0(script_dir,"\\LookUpToRV.r"))
source(paste0(script_dir,"\\RDirchletMultinomial.r"))

#these are for plotting results of 2a
library(gtools)
#library(MCMCpack)
library(DirichletReg)
#@seeds
set.seed(42) #for obvious reasons :)
#----------
#input parameters
load(paste0(base_dir,"//Assignment3PracticeProblem.RData"))
data = x
t = 0
X_t = 0
N = 10000
burn_in = 1000
approximate_sample = list()
k = 4
#output parameters


#----------
#code_body
sigma_current <- 2.474922 #Initial known stable values to prevent numerical instability
alpha_current <- 5.214308 #Initial known stable values to prevent numerical instability
alpha_vector <- rep(alpha_current/k,k)

for (i in 0:burn_in){
  sigma_proposal <- runif(1,0,10)
  alpha_proposal <- runif(1,0,10)
  ratio = LogLiklihoodGivenSigmaAndAlpha(data,sigma_proposal,alpha_proposal)/LogLiklihoodGivenSigmaAndAlpha(data,sigma_current,alpha_current)
  if( ratio >= 1) {
     accept = TRUE
     sigma_current = sigma_proposal
     alpha_curent = alpha_proposal 
  } else{
    runif(1,1,0)
    if(runif <= ratio){
      sigma_current = sigma_proposal
      alpha_curent = alpha_proposal 
      accept = TRUE
    }
  }
  
  if (accept =TRUE){
    x_t = Target(sigma_current,alpha_curent)
    append(approximate_sample, x_t)
  }

  accept = FALSE
 }


for (i in burn_in:N)

sigma_current
sigma_prior = 1/10
alpha_prior = 1/10
joint_prior = sigma_prior * alpha_prior
transition_model = 

View(x)


#END main.r
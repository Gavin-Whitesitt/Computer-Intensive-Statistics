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
N = 1000
burn_in = 100
approximate_sample = list()
k = 4
accept = FALSE
x_t_sigma = c()
x_t_alpha = c()
accept_sample_vector = c()
#output parameters


#----------
#code_body
sigma_current <- runif(1,0,10) #Randomly set sigma
alpha_current <- runif(1,0,10) #Randomly set alpha
alpha_vector <- rep(alpha_current/k,k)

current_liklihood = LogLiklihoodGivenSigmaAndAlpha(data,sigma_current,alpha_current)
if(current_liklihood == 0){
  current_liklihood = .Machine$double.xmin
}



for (i in 0:N){
  sigma_proposal <- runif(1,0,10)
  alpha_proposal <- runif(1,0,10)
  
  proposal_liklihood = LogLiklihoodGivenSigmaAndAlpha(data,sigma_proposal,alpha_proposal)
  
  if(proposal_liklihood == 0 |is.nan(proposal_liklihood)){
    proposal_liklihood = .Machine$double.xmin
  }
  
  
  ratio = proposal_liklihood/current_liklihood
  if( ratio >= 1) {
     accept = TRUE
     sigma_current = sigma_proposal
     alpha_current = alpha_proposal 
  } else{
    uniform = runif(1,0,1)
    if(uniform <= ratio){
      sigma_current = sigma_proposal
      alpha_current = alpha_proposal 
      accept = TRUE
    }
  }
  
  accept_sample_vector = append(accept_sample_vector, accept)
  accept = FALSE
  x_t_sigma = append(x_t_sigma, sigma_current)
  x_t_alpha = append(x_t_alpha, alpha_current)
  
  current_liklihood = LogLiklihoodGivenSigmaAndAlpha(data,sigma_current,alpha_current)
  print(i)
}
#a

#median for sigma
median(x_t_sigma)
#median for alpha
median(x_t_alpha)
#confidence interval

#for sigma
quantile(x_t_sigma,.025)
quantile(x_t_sigma,.975)
#for alpha
quantile(x_t_alpha,.025)
quantile(x_t_alpha,.975)

#c
#Traceplots 

#for sigma
plot(0:N,x_t_sigma)


#for alpha
plot(0:N,x_t_alpha)

#In both examples caterpillers are not looking healthy :( , Should probably use better priors

#d
#cumulative sample mean plot
sigma_cumsum <- cumsum(x_t_sigma) / seq_along(x_t_sigma)
alpha_cumsum <- cumsum(x_t_alpha) / seq_along(x_t_alpha)
#for sigma
plot(0:N,sigma_cumsum)

#for alpha
plot(0:N,alpha_cumsum)

#END main.r

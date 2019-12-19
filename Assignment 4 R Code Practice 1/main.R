#@description
#STAT 565, Assignment 3
#@author
#Gavin Whitesitt: gavinw@uidaho.edu, Colby Bland: cbland@uidaho.edu, Dylan Hull-Nye dhullnye@uidaho.edu
#@param NONE
#@return NONE
#@last_change 12\13\2019
#@paths
base_dir <- "C:\\Users\\gwhit\\Desktop\\Gavin\\School\\Graduate\\3rd Semester\\Student\\STAT 565\\Assignment4\\Assignment 4 R Code Practice 1"
script_dir <- paste0(base_dir,"\\scripts")
output_dir <- paste0(base_dir,"\\output")

#----------
#@libraries_and_functions
source(paste0(script_dir, "\\Interference.r")) 
source(paste0(script_dir,"\\FlipValue.r"))
source(paste0(script_dir,"\\Decipher.r"))
source(paste0(script_dir,"\\GenerateDataFromProposedThetas.r"))
source(paste0(script_dir,"\\RDirchletMultinomial.r"))

library(extraDistr)
#@seeds
set.seed(39) #for obvious reasons :)
#----------
#input parameters
load(paste0(base_dir,"//ABCPractice.RData"))
data = KeyObs
message = c("OPIEHAKLESSXFYHBWWEEFLKMRFJAIICNSAIFNDFAJNJVSEUFBEHHRFEQUAQ")
N = 300000 #Number of samples to generate
data_theta_vector_pairs = data.frame(A = numeric(N), B = numeric(N),C = numeric(N),D = numeric(N),E = numeric(N),F = numeric(N),G = numeric(N),H = numeric(N),I = numeric(N),J = numeric(N),K = numeric(N),L = numeric(N),M = numeric(N),N = numeric(N),O = numeric(N),P = numeric(N),Q = numeric(N),R = numeric(N),S = numeric(N),T = numeric(N),U = numeric(N),V = numeric(N),W = numeric(N),X = numeric(N),Y = numeric(N),Z = numeric(N),Message = numeric(N))
#output parameters


#----------
#code_body

# Coded sequence OPIEHAKLESSXFYHBWWEEFLKMRFJAIICNSAIFNDFAJNJVSEUFBEHHRFEQUAQ

row.names(data)=c('A', 'B', 'C', 'D', 'E', 'F', 'G' ,'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O','P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z')

#Creates vector of probabilities associated with the probability of corruption for a particular bit
#at each of 5 time steps

theta1=.01
theta2=.99
theta3=.99
theta4=.97
theta5=.02

interference_vector =c(theta1, theta2, theta3, theta4, theta5)

interfered_data = Interference(data, interference_vector)
rowSums(data)
Decipher(message, data)
rowSums(interfered_data)
Decipher(message,data = interfered_data)


#Implementing ABC given uniform priors of true value for each letter
for(i in 1:N ){
  theta_proposal = matrix(rdunif(26,0,64),nrow=1)
  proposed_data_before_interference = GenerateDataFromProposedThetas(theta_proposal)
  proposed_data_after_interference = Interference(proposed_data_before_interference, interference_vector)
  proposed_data = Decipher(message , proposed_data_after_interference)
  data_theta_vector_pair = append(theta_proposal, proposed_data)
  
  data_theta_vector_pairs[i,] = data_theta_vector_pair
  print(i)
}
#fixes data type
data_theta_vector_pairs[,1:26] <- sapply(data_theta_vector_pairs[,1:26],as.numeric)

#subsets the data_theta_vector_pairs to only set of data that 
#matches the observed data
data_theta_vector_pairs[data_theta_vector_pairs$Message == Decipher(message, data),]
data_theta_vector_pairs[data_theta_vector_pairs$Message == "OPIEHKLESSXFYHWWEEFLKMFJIINSIFNFJNJVSEUFEHHFEQUQ",]

#The message is ABRACADABRA 

#for b assuming theta t are unknown using beta priors
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

estBetaParams(.01,.001)
estBetaParams(.99,.001)
estBetaParams(.99,.001)
estBetaParams(.97,.001)
estBetaParams(.02,.001)

for(i in 1:N ){
  interference_vector =c(rbeta(1,0.089,8.811), rbeta(1,8.811,0.089), rbeta(1,8.811,0.089), rbeta(1,27.257,0.843), rbeta(1,0.372,18.228))
  theta_proposal = matrix(rdunif(26,0,64),nrow=1)
  proposed_data_before_interference = GenerateDataFromProposedThetas(theta_proposal)
  proposed_data_after_interference = Interference(proposed_data_before_interference, interference_vector)
  proposed_data = Decipher(message , proposed_data_after_interference)
  data_theta_vector_pair = append(theta_proposal, proposed_data)
  
  data_theta_vector_pairs[i,] = data_theta_vector_pair
  print(i)
}

data_theta_vector_pairs[data_theta_vector_pairs$Message == "OPIEHKLESSXFYHWWEEFLKMFJIINSIFNFJNJVSEUFEHHFEQUQ",]

#This is how to set up part b. Unfortunately due to computational limitations(?) I was unable to compute any S* that = S obs
#given more computing power(?) we could answer b. I think this is the correct set up though.

#END main.r



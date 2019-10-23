#@description
#STAT 565, Assignment 2
#@author
#Gavin Whitesitt: gavinw@uidaho.edu, Colby Bland: cbland@uidaho.edu, Dylan Hull-Nye dhullnye@uidaho.edu
#@param NONE
#@return NONE
#@last_change 10\21\2019
#@paths
base_dir <- "C:\\Users\\gwhit\\Desktop\\Gavin\\School\\Graduate\\3rd Semester\\Student\\STAT 565\\Assignment2\\Assignment 2 R Code Practice 2"
script_dir <- paste0(base_dir,"\\scripts")
output_dir <- paste0(base_dir,"\\output")

#----------
#@libraries_and_functions
source(paste0(script_dir, "\\GaussianDensity.r"))
source(paste0(script_dir,"\\Expectation.r"))
source(paste0(script_dir,"\\Maximization.r"))
source(paste0(script_dir,"\\LookUpToRV.r"))
source(paste0(script_dir,"\\RDirchletMultinomial.r"))

#these are for plotting results of 2a
library(pracma)

#@seeds
set.seed(42) #for obvious reasons :)
#----------
#input parameters
#output parameters


#----------
#code_body



#Problem 2 a) ------------------------------------------------------------------------
#Defining parameters for each Kth component
n <- 400

#defing mu (centroid) for each bivariate normal distribution
mu1 <- matrix(c(-5,10), nrow =2, ncol=1)
mu2 <- matrix(c(20,20), nrow =2, ncol=1)
mu3 <- matrix(c(-10,-10), nrow =2, ncol=1)
mu4 <- matrix(c(20,-15), nrow =2, ncol=1)

mu_matrix <- cbind(mu1,mu2,mu3,mu4)

plot(t(mu_matrix))

#defining sigma for each bivariate normal distribution
s1 <- matrix(c(1,2), nrow =2, ncol=1)
s2 <- matrix(c(2,1), nrow =2, ncol=1)
s3 <- matrix(c(3,3), nrow =2, ncol=1)
s4 <- matrix(c(1,3), nrow =2, ncol=1)

sigma_matrix <- cbind(s1,s2,s3,s4)
#defining mixture probabilites for each distribution
pi1 <- .1
pi2 <- .2
pi3 <- .3
pi4 <- .4

pi_vector = c(pi1,pi2,pi3,pi4)

#create pi_vector_cmf
temp = c()
pi_vector_cmf = c()
for (i in pi_vector){
  temp = append(temp, i)
  pi_vector_cmf = append(pi_vector_cmf, sum(temp))
}
#Problem 2 b) ------------------------------------------------------------------------
#simulating a dataset with given probabilities
data <- matrix(NA,ncol = 2, nrow = n)
for (i in 1:n){
  u <- runif(1,0,1)
  component_to_sample_from = min(which(pi_vector_cmf >= u))
  data[i,1] <- rnorm(1,mean = mu_matrix[1,component_to_sample_from], sd = sigma_matrix[1,component_to_sample_from])
  data[i,2] <- rnorm(1,mean = mu_matrix[2,component_to_sample_from], sd = sigma_matrix[2,component_to_sample_from])
}

plot(data)
#Problem 2 c) ------------------------------------------------------------------------
#Finding the maximum liklihood estimate of the parameters of the mixture distribution simulated in part b
#This is accomplished using an expectation maximization algorithm

#Initialize 
epsilon <- .001

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

n_epochs = 1000

for (i in 1:n_epochs){
  components = Expectation(data, components)
  components = Maximization(data, components)
  print(i)
}

components$pi
#Problem 2 d) ------------------------------------------------------------------------

#END main.r
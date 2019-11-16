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
source(paste0(script_dir,"\\InitializeComponents.r"))

library(pracma) #practical math functions
library(dplyr) #some practical functions for manipulating matricies
library(MASS) #mvrnorm generates random multivariate data


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
s1 <- matrix(c(1,0,0,2), nrow =2, ncol=2)
s2 <- matrix(c(2,.7,.7,1), nrow =2, ncol=2)
s3 <- matrix(c(3,2,2,3), nrow =2, ncol=2)
s4 <- matrix(c(1,.5,.5,3), nrow =2, ncol=2)

sigma_matrix <- list(s1,s2,s3,s4)
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
  data[i,] <- mvrnorm(n = 1, as.vector(mu_matrix[,component_to_sample_from]),sigma_matrix[[component_to_sample_from]])
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

n_epochs = 100

for (i in 1:n_epochs){
  components = Expectation(data, components)
  components = Maximization(data, components)
  print(i)
}

components$pi
components$mu
components$covariance
#Notice how probabilities of membership converge to true probabilities

#Problem 2 d) ------------------------------------------------------------------------
#building a 100(1-alpha)% confidence interval using boostrap

B = 100
maximized_components = components

maximized_components$pi
temp = c()
pi_vector_cmf = c()
for (i in maximized_components$pi){
  temp = append(temp, i)
  pi_vector_cmf = append(pi_vector_cmf, sum(temp))
}

bootsamples = list()
for (i in 1:B){
  
  bootsample <- matrix(NA,ncol = 2, nrow = n) #Clears data for each bootsample
  components = InitializeComponents(mu_matrix, data)
  
  for (i in 1:n){
    u <- runif(1,0,1)
    component_to_sample_from = min(which(pi_vector_cmf >= u))
    bootsample[i,] <- mvrnorm(n = 1, maximized_components$mu[[component_to_sample_from]],maximized_components$covariance[[component_to_sample_from]])
  }
  
  for (i in 1:n_epochs){
    components = Expectation(bootsample, components)
    components = Maximization(bootsample, components)
    print(i)
  }
  
  bootsamples[B] <- list(components)
  
}



cov_1 <- c()
cov_2 <- c()
cov_3 <- c()
cov_4 <- c()

cov_1_1_vector <- c()
cov_1_2_vector <- c()
cov_1_3_vector <- c()

cov_2_1_vector <- c()
cov_2_2_vector <- c()
cov_2_3_vector <- c()

cov_3_1_vector <- c()
cov_3_2_vector <- c()
cov_3_3_vector <- c()

cov_4_1_vector <- c()
cov_4_2_vector <- c()
cov_4_3_vector <- c()

mu_1 <- c()
mu_2 <- c()
mu_3 <- c()
mu_4 <- c()

mu_1_1_vector <- c()
mu_1_2_vector <-c()
mu_2_1_vector <-c()
mu_2_2_vector <-c()
mu_3_1_vector <-c()
mu_3_2_vector <-c()
mu_4_1_vector <-c()
mu_4_2_vector <-c()

pi_1_vector <-c()
pi_2_vector <-c()
pi_3_vector <-c()
pi_4_vector <- c()

for (i in 1:B){
  current_bootsample <- bootsamples[[B]]
  
  cov_1 <- current_bootsample$covariance[[1]]
  cov_2 <- current_bootsample$covariance[[2]]
  cov_3 <- current_bootsample$covariance[[3]]
  cov_4 <- current_bootsample$covariance[[4]]
  
  cov_1_1_vector <- c(cov_1_1_vector ,cov_1[1,1])
  cov_1_2_vector <- c(cov_1_2_vector ,cov_1[1,2])
  cov_1_3_vector <- c(cov_1_3_vector, cov_1[2,2])
  
  cov_2_1_vector <- c(cov_2_1_vector ,cov_1[1,1])
  cov_2_2_vector <- c(cov_2_2_vector ,cov_1[1,2])
  cov_2_3_vector <- c(cov_2_3_vector ,cov_1[2,2])
  
  cov_3_1_vector <- c(cov_3_1_vector ,cov_1[1,1])
  cov_3_2_vector <- c(cov_3_2_vector ,cov_1[1,2])
  cov_3_3_vector <- c(cov_3_3_vector ,cov_1[2,2])
  
  cov_4_1_vector <- c(cov_4_1_vector ,cov_1[1,1])
  cov_4_2_vector <- c(cov_4_2_vector ,cov_1[1,2])
  cov_4_3_vector <- c(cov_4_3_vector ,cov_1[2,2])
  
  mu_1 <- current_bootsample$mu[[1]]
  mu_2 <- current_bootsample$mu[[2]]
  mu_3 <- current_bootsample$mu[[3]]
  mu_4 <- current_bootsample$mu[[4]]
  
  mu_1_1_vector <- c(mu_1_1_vector, mu_1[1,1])
  mu_1_2_vector <- c(mu_1_1_vector, mu_1[2,1])
  mu_2_1_vector <- c(mu_1_1_vector, mu_2[1,1])
  mu_2_2_vector <- c(mu_1_1_vector, mu_2[2,1])
  mu_3_1_vector <- c(mu_1_1_vector, mu_3[1,1])
  mu_3_2_vector <- c(mu_1_1_vector, mu_3[2,1])
  mu_4_1_vector <- c(mu_1_1_vector, mu_4[1,1])
  mu_4_2_vector <- c(mu_1_1_vector, mu_4[2,1])
    
  pi <- current_bootsample$pi
  
  pi_1_vector <- c(pi_1_vector, pi[[1]])
  pi_2_vector <- c(pi_1_vector, pi[[2]])
  pi_3_vector <- c(pi_1_vector, pi[[3]])
  pi_4_vector <- c(pi_1_vector, pi[[4]])
    
}

alpha = .02
estimate = mu_1_1_vector

#works for any estimate: examples for mu_1_1_vector shown
quantile(estimate, c(alpha, 1-alpha)) 


estimate = mu_1_2_vector
quantile(estimate, c(alpha, 1-alpha))

#END main.r

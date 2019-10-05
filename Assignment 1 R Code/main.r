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
#@seeds
set.seed(42) #for obvious reasons :)
#----------
#input parameters
n = 100
alpha_vector= c(1,5,2)
#output parameters
lambda = c()
p = c()
x = matrix(NA, nrow = N, ncol = n)
z = c()
parameters = list()
data = list()
#----------
#code_body

cmf = DirichletCMF(alpha_vector, n)
max(cmf)
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
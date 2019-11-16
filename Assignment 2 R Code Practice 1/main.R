set.seed(42)
k=4
a1=1.1
a2=2.8
a3=1.3
a4=4
alpha=c(a1, a2, a3, a4)
sigma=2
N=10^5

# Estimate both A_mc and Var(A_mc)

library(MCMCpack)
DIR=rdirichlet(4, alpha)
# Each set of 4 x_i sum to 1
# Total equals 4
e=vector(length=k)
x=vector(length = k)


for (i in 1:k) {
  e[i]=-sigma*sum(DIR[,i]^2)
  x[i]=exp(e[i])*prod(DIR[i]^(alpha[i]-1))
}

data=matrix(ncol = 4, nrow = N)

# Sample 
# Turned into list
taco=list(mode='Vector', length=N)
for (j in 1:N){
  taco[[j]]=rdirichlet(4, alpha)
}
taco=as.data.frame(taco)
# 4 by 400,000

# Constant 
dirconstant=prod(gamma(alpha[1:k]))/gamma(sum(alpha[1:k]))
dirconstant

for (i in 1:length(taco)){
  for (j in 1:k){
    A=sum((taco[j,i]^2))
  }
}

A_mc=1/N*sum(exp(-sigma*A))*dirconstant
A_mc
# 7.817201e-10

# Variance
# use same Values

for (i in 1:length(taco)){
  for (j in 1:k){
    VAR=sum((taco[j,i]-A_mc)^2)/(N-1)
  }
}
ConstantOfIntegration = function(alpha,sigma){
k=4
a1= alpha/4
a2= alpha/4
a3= alpha/4
a4= alpha/4
alpha=c(a1, a2, a3, a4)
sigma= sigma
N=100

# Estimate both A_mc and Var(A_mc)

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


taco=list(mode='Vector', length=N)
for (j in 1:N){
  taco[[j]]=rdirichlet(4, alpha)
}
taco=as.data.frame(taco)
# 4 by 400,000

# Constant 
dirconstant=prod(gamma(alpha[1:k]))/gamma(sum(alpha[1:k]))

for (i in 1:length(taco)){
  for (j in 1:k){
    A=sum((taco[j,i]^2))
  }
}

A_mc=1/N*sum(exp(-sigma*A))*dirconstant
return(A_mc)

}






#ConstantOfIntegration = function(alpha,sigma,k){
#    N = 10000
#   b = c()
#    for (i in 1:N){
#      a = (1/rbeta(1, alpha/k, alpha/k))*rdirichlet(k, alpha/k)
#      b = append(b,a)
#    }
#    c = exp(-sigma*sum(b^2))
#    constant_of_integration = (sum(c)/N)
#    
#    return(constant_of_integration)
#}

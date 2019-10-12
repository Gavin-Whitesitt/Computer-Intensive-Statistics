# 1

set.seed(42)

n=100
beta0=1
beta1=3
beta2=2
x1=runif(n, 0, 10)
x2=runif(n, 0, 10)

error=rnorm(n, 0, .5)

y=beta0+beta1*x1+beta2*x2+error
p1=lm(y~x1+x2)
p1
# intercept=.9505
# x1=2.996
# x2=2.0051

round(svd(x)$u%*%diag(svd(x)$d)%*%t(svd(x)$v), 2)==round(x, 2)
# Use svd to find betas 
x=matrix(c(rep(beta0, 100), x1, x2), 100, 3)

s=svd(x)

u=svd(x)$u
d=s$d
dmatrix=matrix(c(79.760997, 0, 0, 0, 29.606810, 0, 0, 0, 3.583518), nrow = 3)
v=s$v

d[1]

library(corpcor)
pd=pseudoinverse(dmatrix)

pseudoX=u %*% pd %*% t(v)

answer=y %*%pseudoX
# 0.9505316
# 2.995999
# 2.005137


Vector_to_Diagnonal=function(Vector) {
    Matrix1=matrix(0, nrow = length(Vector), ncol =length(Vector))
  for (i in 1:length(Vector)) {
    Matrix1[i,i]=Vector[i]
  }
    return(Matrix1)
}

SVD_function=function(t, y) {
  {
  S=svd(t) 
  U=S$u
  V=S$v
  D=S$d
  Matrix1=Vector_to_Diagnonal(D)
  PI=pseudoinverse(Matrix1)
  pseudoX=U %*% PI %*% t(V)
  answer=y %*%pseudoX
  }
  return(answer)
}





n = 10
p = .3
pmf_vector = c()
cmf = c()
for (k in 0:n){
  pmf = choose(n,k)*(p^k)*((1-p)^(n-k))
  pmf_vector = append(pmf_vector, pmf)
  cmf = append(cmf, sum(pmf_vector))
}

x_vector = 0:n
u = runif(1,0,1)
x = min(which(cmf -1 >= u))

simulated_x = matrix(nrow = 100000,ncol = 1)
for (i in 1:100000){
  u = runif(1,0,1)
  x = min(which(cmf >= u))
  simulated_x[i,1] = x -1
}

hist(simulated_x[,1],freq=TRUE,breaks = 0:n)



Interference = function(data,interference_vector){

  for(i in interference_vector){
    for(j in 1:nrow(data)){
      for(k in 1:ncol(data)){
        a = data[j,k]
        uniform = runif(1,0,1)
        if(uniform <= i){
          a = FlipValue(a)
        }
        data[j,k] = a
      }
     }
    }
  
  return(data)
}
#Assumes input of either a 0 or a 1. If given a 1 outputs a 0, If given a 0 outputs a 1

FlipValue = function(value){
  if(value == 1){
    value = 0
  } else{
    value = 1
  }
  
  return(value)
  }
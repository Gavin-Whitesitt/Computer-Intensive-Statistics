Decipher = function(message,data){
  a = rowSums(data)
  for(i in 1:length(a)){
    if(a[i] <= 32){ #if less than or equal to 32
      a[i] = NA
    }
  }
  
  KEY = names(a[is.na(a)]) #retrives values of letters to not be included given the data
  
  deciphered_message = message
  for(i in 1:length(KEY)){
    deciphered_message = gsub(KEY[i],'',deciphered_message)
  }
  
  return(deciphered_message)
}

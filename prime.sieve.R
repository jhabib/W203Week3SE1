prime.sieve.slow <- function(n){
  
  if(n <= 1){
    return(paste0('No prime numbers exist <= ', n))
  }
  
  else if(n==2){
    return(paste0('Prime number: ', n))
  }
  
  else {
    num.vec <- seq.int(2, n)
    
    for(num in num.vec){
      if(num<=sqrt(num.vec[length(num.vec)])){
        not.prime <- seq.int(2*num, n, num)
        num.vec <- setdiff(num.vec, not.prime)
      }
    }
    return(num.vec)  
  }
  
}

prime.sieve.new <- function(n){
  
  if(n <= 1){
    return(paste0('No prime numbers exist <= ', n))
  }
  
  else if(n==2){
    return(paste0('Prime number: ', n))
  }
  
  else{
    # Generate a vector that includes 2 and odds up to n
    new.vec <- c(2, seq.int(3, n, 2))
    
    i <- 1
    # While p^2 <= n
    while(new.vec[i]**2 <= new.vec[length(new.vec)]){
      # Remove numbers of the form p^2 + 2p from new.vec
      new.vec <- setdiff(new.vec, 
                         seq(new.vec[i]**2,  
                             new.vec[length(new.vec)], 
                             2*new.vec[i]))
      
      i <- i+1 # Go to next p
    }
    return(new.vec)
    
  }
}
library(doSNOW)

prime.sieve.helper <- function(low, high){
  # Generate a vector that includes 2 and odds up to n
  vec <- seq.int(ceiling(low/2), ceiling(high/2))*2-1
  
  sieve <- seq(2, high)
  
  i <- 1
  while(sieve[i]**2 <= sieve[length(sieve)]){
    # not.prime <- seq(sieve[i]**2, high, 2*sieve[i])
    # not.prime <- seq((low%/%sieve[i])*sieve[i], high, sieve[i])
    vec <- setdiff(vec, seq(sieve[i]**2, high, 2*sieve[i]))
    i <- i+1
  }
  return(vec)
}

prime.sieve.parallel <- function(n){
  
  if(n <= 1){
    return(paste0('No prime numbers exist <= ', n))
  }
  
  else if(n==2){
    return(paste0('Prime number: ', n))
  }
  
  else{
    
    cl.nodes <- 4
    
    my.cluster <- makeCluster(cl.nodes, type = "SOCK")
    
    registerDoSNOW(my.cluster)
    
    # Generate a vector that includes 2 and odds up to n
    new.vec <- seq.int(3, n, 2)
    
    # Create a vector chunk for processing by each cluster node
    vec.chunks <- split(new.vec, 
                        ceiling(seq_along(new.vec)/(length(new.vec)/cl.nodes))) 
    
    results <- c(2, 
                 foreach(chunk=vec.chunks, 
                       .combine = append, 
                       .export = 'prime.sieve.helper') %dopar% 
      prime.sieve.helper(chunk[1], chunk[length(chunk)]))
    
    stopCluster(my.cluster)
    
    return(results)
  }
}


# Test --------------------------------------------------------------------

test.numbers <- 10**(3:6)

times1 <- foreach(number=test.numbers, 
        .combine = rbind) %do% 
  system.time(prime.sieve.parallel(number))

times2 <- foreach(number=test.numbers, 
                  .combine = rbind) %do% 
  system.time(prime.sieve.new(number))

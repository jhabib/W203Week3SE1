# A function that determines whether (x, y) is inside a circle of radius 1
# Returns 1 if true, 0 if false
is.inside.circle <- function(vec){
  ifelse(vec[1]**2 + vec[2]**2 <= 1, 1, 0)
}

# Let number of coordinates equal 1000000
n <- 10**6

# Generate n coordinates using runif as an [x, y] matrix
coords <- cbind(runif(n, -1, 1), runif(n, -1, 1))

# Plot histograms of our random coordinates
hist(coords[,1])
hist(coords[,2])

# Pass coords to is.inside.circle and get 1 or 0 for each
# Store this result in a vector
test.vector <- apply(coords, 1, FUN='is.inside.circle')

# Sum of test.vector gives us the number of points inside the circle
# Length gives the total number which is equal to n = 10**6
# We multiply that by the area of a square with side=1 to get estimate of pi
pi.estimate <- sum(test.vector)/length(test.vector)*4

# Calculate the theoretical variance of pi.estimate
# Let r be the result of is.inside.circle(coord)
r.expectation = (pi.estimate/4)*(1) + (pi.estimate/4)*(0)
r_squared.expectation = (pi.estimate/4)*(1)**2 + (pi.estimate/4)*(0)**2

#variance = r_squared.expectation - (r.expectation)**2
variance.theoretical <- (pi.estimate/4)*(1-pi.estimate/4)

# Function for estimating pi n times 
# where sample size for each estimate and number of estimates are given
pi.estimates.n <- function(sample.size, trials){
  pi.estimates.array <- c()
  i <- 1
  while(i <= trials){
    i <- i + 1
    test.vector <- apply(cbind(runif(sample.size, -1, 1), runif(sample.size, -1, 1)), 1, FUN='is.inside.circle')
    pi.estimates.array <- rbind(pi.estimates.array, sum(test.vector)/length(test.vector)*4)
  }
  return(pi.estimates.array)
}

# Numerically calculate the variance of pi.estimates
# where sample size = 100000 and number of trials = 100

pi.estimates <- pi.estimates.n(100000, 10)

variance.numerical <- mean((mean(pi.estimates/4)-(pi.estimates/4))^2)
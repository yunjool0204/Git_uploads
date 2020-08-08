# Modified Basel Traffic Light Tests

# For the backtesting with m-day overlapping PnL, 
# the thresholds for Basel traffic light tests can be determined from MC simulation

# The script runs 'numPath' simulation to identify the number of observation 
# that needs to be breached for h-th percentile

nobs <- 250;  # number of observations 
numPath <- 30000; # number of simulated path to determine thresholds
m = 10; # number of days - PnL timeline

x <- matrix(0, nobs, numPath);
S <- matrix(0, nobs, numPath);
h <- qnorm(0.99, 0, 1)*sqrt(m);
Z <- rnorm((nobs+m-1)*numPath, mean = 0, sd = 1);
Z <- matrix(Z, nrow = nobs+m-1, ncol = numPath, byrow = TRUE);


sumS <- sum(S);
sortS <- sort(sum(S));


for(i in 1:nobs) {
  x[i, 1:numPath] <- colSums(Z[i:(i+m-1),1:numPath], na.rm = FALSE, dims = 1)
  S[i, 1:numPath] <- (x[i, 1:numPath]- h) >0
}

a <- rowSums (S, na.rm = FALSE, dims = 1)
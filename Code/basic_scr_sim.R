## SCR Book section 5.5
library(secr)
library(AHMbook)

set.seed(2013)

# create a 5 x 10 grid of trap locations on a 1 m grid

traplocs <- cbind(sort(rep(1:5, 10)), rep(1:10, 5))
ntraps <- nrow(traplocs)

# compute distance matrix
Dmat <- e2dist(traplocs, traplocs) # you can use the sp package to do this with lat and lon or just knowing that you're are 25 m apart

# Define state-space of point proces (i.e. where animals live)
# Buffer adds fixed buffer to the outer extent of the traps

buffer <- 2 # 2 meter buffer in this case
xlim <- c(min(traplocs[ , 1] - buffer), max(traplocs[ , 1] + buffer))
ylim <- c(min(traplocs[ , 2] - buffer), max(traplocs[ , 2] + buffer))

# population size
N <- 100
K <- 4 # number of occasions

# simulate random activity centers
sx <- runif(N, xlim[1], xlim[2]) 
sy <- runif(N, ylim[1], ylim[2]) 

S <- cbind(sx, sy)

# compute distance matrix of individuals from each trap
D <- e2dist(S, traplocs)

alpha <- 0
sigma <- 0.5
alpha1 <- 1 / (2 * sigma * sigma)

# Compute Probability of Encounter
probcap <- plogis(alpha) * exp(-1 * alpha1 * D * D)

# Generate encounters of every individuals in every trap
Y <- matrix(NA, nrow = N, ncol = ntraps)
for(i in 1:nrow(Y)) {
  Y[i, ] <- rbinom(ntraps, K, probcap[i, ])
}

cbind(1:100, rowSums(Y)) # now we have data. Yay! that was easier than field work




##### NIMBLE #####

library(dplyr)
library(tidyr)
# library(AHMbook)
# library(R2jags)  #rjags could not be loaded?
library(jagsUI)
# library(R2WinBUGS)

EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE)
head(EDF)

#Add a new column for integer session values (session = site)

EDF$site_num <- as.integer(as.factor(EDF$site))
# 
# EDF$session <- NA
# head(EDF)
# EDF$session <- ifelse(EDF$site == "A", 1,
#                   ifelse(EDF$site == "C", 2,
#                     ifelse(EDF$site == "D", 3,
#                       ifelse(EDF$site == "E", 4,
#                          ifelse(EDF$site == "F", 5,
#                            ifelse(EDF$site == "G", 6,
#                              ifelse(EDF$site == "H", 7,
#                                ifelse(EDF$site == "I", 8,
#                                  ifelse(EDF$site == "J", 9,
#                                    ifelse(EDF$site == "K", 10,
#                                      ifelse(EDF$site == "L", 11,
#                                        ifelse(EDF$site == "M", 12,
#                                         ifelse(EDF$site == "N", 13,
#                                           ifelse(EDF$site == "O", 14, NA)
#                                         )))))))))))))
# head(EDF)
summary(EDF)

## Creating model for 1 site first
EDFA <- EDF %>%
  filter(site_num == 1 & species == "CPIC")
EDFA

# Create a Trap Location Matrix (integers = distance apart in m)
traplocsA <- c(0,25,50,75,100,125,150,175) # create trap location file
#this is in a vertical format
traplocsA <- traplocsA/1000
matrixA <- matrix(NA, ncol = length(traplocsA), nrow = length(traplocsA))
matrixA
matrixA[ ,1] <- c(0,25,50,75,100,125,150,175)
matrixA[ ,2] <- c(25,0,25,50,75,100,125,150)
matrixA[ ,3] <- c(50,25,0,25,50,75,100,125)
matrixA[ ,4] <- c(75, 50,25,0,25,50,75,100)
matrixA[ ,5] <- c(100,75, 50,25,0,25,50,75)
matrixA[ ,6] <- c(125,100,75, 50,25,0,25,50)
matrixA[ ,7] <- c(150,125,100,75, 50,25,0,25)
matrixA[ ,8] <- c(175,150,125,100,75, 50,25,0)
matrixA <- matrixA/1000  # will need to use coordinates if use all sites in 1 model! or figure out distance b/w sites

n_traps <- ncol(matrixA) # number of traps
# as.character(EDFA$recap)
N <- nrow(EDFA[which(EDFA$recap == "N"), ])
K <- max(EDFA$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimA <- c(min(matrixA)-buffer, max(matrixA) + buffer)
xlimA
n_ind <- length(unique(EDFA$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFA)

EM <- EDFA %>%
  group_by(ind, trap) %>%
  select(trap, ind) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()

EM <- as.matrix(data.frame(select(EM, -ind)))
# EM <- cbind(1:n_ind, rowSums(EM))
# EM <- rowSums(EM)
# Read in trap hour file
traphoursA <- read.csv(file = "Data/traphoursA.csv", stringsAsFactors = FALSE)
head(traphoursA)
hours <- apply(traphoursA[ , 2:ncol(traphoursA)], 1, sum)


# Data Augmentation
M <- 200
J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))
z <- c(rep(1, n_ind), rep(0, M-n_ind))


#Start values for s (activity centers) of augments (from random uniform constrained by state space size)
X <- traplocsA
sst <- c(runif(M, xlimA[1], xlimA[2])) #parameters, n, max, min
sst # Now populated by starting positions uniformally placed within state space
# For every individual that is not 0, change starting x point to mean of traps associated with encounters for that individual; leaves 0's there from the augmented population and also puts in activity center for augmented individuals that were randomly given an encounter history (caught at least 1 time)
for(i in 1:n_ind) {
  sst[i] <- mean( X[y[i, ] > 0] )
}
sst


# Create model function in BUGS code named X
# Example: SimpleModelA <- X(EM, traps, nb = 2000, ni = 12000, buffer = 1, M = 300)
# buffer = determines size of state space model in scaled units
# Use jags.model function to intitialize model and coda.samples to obtain posterior samples for analysis
# Example: InitialA <- jags.model("X", data = data, inits = inits, n.chains = 3, n.adapt = 1000)
#PostA <- coda.samples(InitialA, parameters, n.iter = 1000, thin = 1)

#### BUS Model Specification ####

# alpha0 = set prob. of baseline encounter (model will alter?)
# logit = constraining prob b/w 0 and 1
# alpha1 = set prob. of encounter at 1 unit distance away from alpha0
# sigma = scale parameter of a half-normal
# why ~ ?
# psi ?
# for each individual for number of augments (M), find z[i] - likelihood that augment is part of sampled population, activity center (acs assumed to be uniformally distributed) , and 
# for each trap
# calculate distance of each individual i from trap j using distance equation
# estimate parameters affecting the observed number of times caught per trap reflecting a      # binomial distribution with probability of encounter and trapping effort (might need to change this to fraction of a day instead of in hours?)
# estimate encounter probability parameters including likelihood is part of population...,     # baseline encounter prob., and the distance coefficient (prob decay rate)
# Calculate population size by summing all 1s associated with individuals within the population that were sampled (prob. of z = 1)
# Calculate density by dividing pop. size by state space size (trapping length + buffers)



library(nimble)
SCRA.nimble <- nimbleCode({
    alpha0 ~ dnorm(0, 0.1)
    logit(p0) <- alpha0
    alpha1 ~ dnorm(0, 0.1)
    sigma <- pow(1/(2*alpha1), 0.5) # circular home range?
    psi ~ dunif(0, 1)
           for(i in 1:M) {
                  z[i] ~ dbern(psi)
                   s[i] ~ dunif(xlimA[1], xlimA[2])
             for(j in 1:n_traps) {
                  d[i,j] <- abs(s[i] - traplocsA[j])
                  y[i,j] ~ dbin(p[i,j], K)
                   p[i,j] <- z[i]*p0*exp(- alpha1 * d[i,j] * d[i,j])
      }
  }
    N <- sum(z[])
    D <- N/(xlimA[2] - xlimA[1])
})

data <- list(M=M, X=X, xlim=xlimA, y=y)
constants <- list(M=M, J=J, K=K)
inits <- list(alpha0=rnorm(1,-2,.4), alpha1=runif(1,1,2), S=sst, z=z, S = S, sigma = sigma, psi = psi, w = rbinom(200,1,0.5))

params <- c("alpha0", "alpha1", "N", "D", "g0","sigma")


Rmodel <- nimbleModel(code=code, constants=constants, data=data, inits=inits)
Cmodel <- compileNimble(Rmodel)




####
jags_data <- list(y = y, traplocsA = traplocsA, K=K, M=M, xlimA=xlimA, n_traps = n_traps)
inits <- function() {
  list(alpha0=rnorm(1,-2,.4), alpha1=runif(1,1,2), s=sst, z=z)
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "D")

jinitA <- jagsUI(model.file = "Code/JAGS/SCRA.txt", parameters.to.save = parameters, data=jags_data, inits=inits, n.iter = 1000, n.chains = 3, n.adapt =500)
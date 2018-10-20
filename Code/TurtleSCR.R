
##### SCR Analysis Data Compilation and Model Creation Using BUGS and JAGS ####

library(AHMbook)
library(R2jags)  #rjags could not be loaded?
library(jagsUI)
library(R2WinBUGS)

EDF <- read.csv(file = "~/Desktop/Thesis/cocanal-turtles/Data/EDF.csv")
head(EDF)
#Add a new column for integer session values (session = site)
EDF$session <- NA
head(EDF)
EDF$session <- ifelse(EDF$site == "A", 1,
                  ifelse(EDF$site == "C", 2,
                    ifelse(EDF$site == "D", 3,
                      ifelse(EDF$site == "E", 4,
                         ifelse(EDF$site == "F", 5,
                           ifelse(EDF$site == "G", 6,
                             ifelse(EDF$site == "H", 7,
                               ifelse(EDF$site == "I", 8,
                                 ifelse(EDF$site == "J", 9,
                                   ifelse(EDF$site == "K", 10,
                                     ifelse(EDF$site == "L", 11,
                                       ifelse(EDF$site == "M", 12,
                                        ifelse(EDF$site == "N", 13,
                                          ifelse(EDF$site == "O", 14, NA)
                                        )))))))))))))
head(EDF)
summary(EDF)

## Creating model for 1 site first
EDFA <- subset(EDF, session == "1")
EDFA

# Create a Trap Location Matrix (integers = distance apart in m)
traplocsA <- cbind(0,25,50,75,100,125,150,175) # create trap location file
#this is in a vertical format
traplocsA
matrixA <- matrix(NA, ncol = ncol(traplocsA), nrow = ncol(traplocsA))
matrixA
matrixA[ ,1] <- c(0,25,50,75,100,125,150,175)
matrixA[ ,2] <- c(25,0,25,50,75,100,125,150)
matrixA[ ,3] <- c(50,25,0,25,50,75,100,125)
matrixA[ ,4] <- c(75, 50,25,0,25,50,75,100)
matrixA[ ,5] <- c(100,75, 50,25,0,25,50,75)
matrixA[ ,6] <- c(125,100,75, 50,25,0,25,50)
matrixA[ ,7] <- c(150,125,100,75, 50,25,0,25)
matrixA[ ,8] <- c(175,150,125,100,75, 50,25,0)
matrixA  # will need to use coordinates if use all sites in 1 model! or figure out distance b/w sites

ntrapsA <- 8 # number of traps
as.character(EDFA$recap)
N <- 44
K <- 4 # trap nights per session
buffer <- 50 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimA <- c(-50, 225)
xlimA
EM <- read.csv(file = "~/Desktop/Thesis/cocanal-turtles/Data/EncounterMatrixA.csv")
head(EM)
EM[is.na(EM)] <- 0
EM <- as.matrix(EM)
EM <- cbind(1:42, rowSums(EM))
# Read in trap hour file
traphoursA <- read.csv(file = "~/Desktop/Thesis/cocanal-turtles/Data/traphoursA.csv")
head(traphoursA)
K <- apply(traphoursA[ , 2:ncol(traphoursA)], 1, sum)
K


# Data Augmentation
M <- 200
nind <- nrow(EM)
J <- 8
y <- rbind(EM, matrix(0, nrow = M-nind, ncol=ncol(EM)))
z <- c(rep(1, nind), rep(0, M-nind))


#Start values for s (activity centers) of augments (from random uniform constrained by state space size)
X <- traplocsA
sst <- cbind(runif(M, xlimA[1], xlimA[2])) #parameters, n, max, min
sst # Now populated by starting positions uniformally placed within state space
# For every individual that is not 0, change starting x point to mean of traps associated with encounters for that individual; leaves 0's there from the augmented population and also puts in activity center for augmented individuals that were randomly given an encounter history (caught at least 1 time)
for(i in 1:nind) {
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

cat ("
model {
    alpha0 ~ dnorm(0, 0.1)
    logit(p0) <- alpha0
    alpha1 ~ dnorm(0, 0.1)
    sigma <- sqrt(1/(2*alpha1))
    psi ~ dunif(0, 1)
    for(i in 1:M) {
        z[i] ~ dbern(psi)
        s[i] ~ dunif(xlimA[1], xlimA[2])
        for(j in 1:8) {
            d[i,j] <- sqrt(pow(s[i] - traplocsA[j], 2))
            y[i,j] ~ dbin(p[i,j], K)
            p[i,j] <- z[i]*p0*exp(- alpha1 * d[i,j] * d[i,j])
      }
    }

# error line 13, index out of range taking subset of y (y[i,j])

N <- sum(z[])
D <- N/300
}
", file = "SCRA.txt")

data <- list(y = EM, X=traplocsA, K=K, M=M, xlim=xlimA)
inits <- function() {
  list(alpha0=rnorm(1,-4,.4), alpha1=runif(1,1,2), s=sst, z=z)
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "D")

jinitA <- jags.model("SCRA.txt", data=data, inits=inits, n.chains = 3, n.adapt =1000)
# Get error parsing model file: syntax error online 11 near n

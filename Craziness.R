456345673453456
##### SCR Analysis Data Compilation and Model Creation Using BUGS and JAGS ####

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
traplocsA
traplocsA_stand <- traplocsA/25
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
matrixA  # will need to use coordinates if use all sites in 1 model! or figure out distance b/w sites
matrixA_standard <- matrixA/25
matrixA_standard

n_traps <- ncol(matrixA) # number of traps
# as.character(EDFA$recap)
N <- nrow(EDFA[which(EDFA$recap == "N"), ])
K <- max(EDFA$day) # trap nights per session
buffer <- 100/25 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimA <- c(min(matrixA_standard)-buffer, max(matrixA_standard) + buffer)
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
head(EM)
# EM <- cbind(1:n_ind, rowSums(EM))
# EM <- rowSums(EM)
# Read in trap hour file
traphoursA <- read.csv(file = "Data/traphoursA.csv", stringsAsFactors = FALSE)
traphoursA
hours <- apply(traphoursA[ , 2:ncol(traphoursA)], 1, sum) #Sum hours per trap over 4 days
#hours

# Data Augmentation
M <- 200
J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps)) # combine EM data and augments
z <- c(rep(1, n_ind), rep(0, M-n_ind)) # assign z (prob. that individual is part of sampled population), augments are z = 0 at the moment
#z

# Add y inits
yin <- array(0, c(M,J,K))
for(j in 1:J) {
  for(k in 1:K) {
    yin[1:M, j, k] <- rmultinom(1, n[j,k], rep(1/M, M))
  }}
# Error in n[j, k] : object of type 'closure' is not subsettable

?rmultinom

#Start values for s (activity centers) of augments (from random uniform constrained by state space size)
X <- traplocsA_stand
sst <- c(runif(M, xlimA[1], xlimA[2])) # parameters, n, max, min
sst # Now populated by starting positions uniformally placed within state space
# For every individual that is not 0, change starting x point to mean of traps associated with encounters for that individual; leaves 0's there from the augmented population and also puts in activity center for augmented individuals that were randomly given an encounter history (caught at least 1 time)
for(i in 1:n_ind) {
  sst[i] <- mean( X[y[i, ] > 0] )
}
sst

# set z
#for (i in 1:M){ #m     
#for(k in 1:(first[i]-1)) { #t
#S[i,t] <- 0  # Individual not in river, needed to follow node in JAGS
# z[i,t] <- 0  # Individual not in river, needed to follow node in JAGS
#} #t

z[i,first[i]] ~ dbern(1)  # Individual known to be alive at entry into study area
S[i,first[i]] ~ dunif(xl,xu)  # No prior information on individual’s location



z[] <- 0
z[first[i]] ~ dbern(1)


#change from mean to first function!!

# Raabe et al.
#for (i in 1:M){ # m     
#  for(k in 1:(first[i]-1)) { #t
#  S[i,k] <- 0  # Individual not in canal, needed to follow node in JAGS
#  z[i,k] <- 0  # Individual not in canal, needed to follow node in JAGS
# } #t


# Example of s and sm inits
#sm <- cbind(runif(m, xlims[1], xlims[2]), runif(m, ylims[1]))
#s <- cbind(runif(M, xlims[1], xlims[2]), runif(M, ylims[1], ylims[2])) 
#inits <- function() {list(baseline.p=runif(1),alpha1=runif(1,0,1),
# alpha2=rnorm(1), sm=sm, s=s, zm=rep(1,max), z=rep(1,M),yu=yin)}



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

if(!dir.exists("Code/JAGS")) dir.create("Code/JAGS", recursive = TRUE)

cat ("
     model {
     alpha0 ~ dnorm(0, 0.1)        # coefficeint for baseline encounter prob.
     logit(p0) <- alpha0           # making baseline encounter prob. positive
     alpha1 ~ dnorm(0, 0.1)        # prob. of encounter 1 unit away
     sigma <- pow(1/(2*alpha1), 0.5)   # circular home range?
     psi ~ dunif(0, 1)
     yin <- array(0, c(M,J,K))
     for(j in 1:J) {
     for(k in 1:K) {
     yin[1:M, j, k] <- rmultinom(1, n[j,k], rep(1/M, M))
     }}
     for(i in 1:M) {
     for(j in 1:n_traps) {
     z[i, first[i]] ~ dbern(1) # ind. is alive at entry into study
     s[i, first[i]] ~ dunif(xlimA[1], xlimA[2])
     d[i,j, first[i]] <- abs(s[i] - traplocsA[j])
     y[i,j, first[i]] ~ dpois(p[i,j], K)  # changed from dbin to dpois
     p[i,j, first[i]] <- z[i, first[i]]*p0*exp(- alpha1 * d[i,j, first[i]] * d[i,j, first[i]])
     for (j in (first[i]+1):last[i]) {
     z[i,j] ~ dbern(0,1)
     d[i,j] <- abs(s[i] - traplocsA[j])
     y[i,j] ~ dbin(p[i,j], K)
     p[i,j] <- z[i]*p0*exp(- alpha1 * d[i,j] * d[i,j])
     }
     }
     }
     
     
     N <- sum(z[])
     D <- N/(xlimA[2] - xlimA[1])
     }
     ", file = "Code/JAGS/SCRA.txt")

jags_data <- list(y = y, traplocsA = traplocsA, K=K, M=M, xlimA=xlimA, n_traps = n_traps)
inits <- function() {
  list(alpha0=rnorm(1,-2,.4), alpha1=runif(1,1,2), s=sst, z=z)
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "D")

jinitA <- jagsUI(model.file = "Code/JAGS/SCRA.txt", parameters.to.save = parameters, data=jags_data, inits=inits, n.iter = 1000, n.chains = 3, n.adapt =500)
# Get error node inconsistent with parents

library(R2jags)
jinitA <- R2jags::jags(model.file = "Code/JAGS/SCRA.txt", parameters.to.save = parameters, data=jags_data, inits=inits, n.chains = 3, n.burnin = 500, n.iter = 1000)
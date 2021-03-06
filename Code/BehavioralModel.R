##### Behavioral SCR Model Creation JAGS ####

#### Adding in Trap Shyness/Happiness ####

library(dplyr)
library(tidyr)
library(rjags)
library(parallel)
# library(AHMbook)
# library(jagsUI)

EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE)
head(EDF)

#Add a new column for integer session values (session = site)

EDF$site_num <- as.integer(as.factor(EDF$site))
summary(EDF)

## Creating model for 1 site first
EDFA <- EDF %>%
  filter(site_num == 1 & species == "CPIC")
EDFA

# Create a Trap Location Matrix (integers = distance apart in m)
traplocsA <- c(0,25,50,75,100,125,150,175) # create trap location file
#this is in a vertical format
traplocsA
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

traplocsA <- traplocsA / 100
matrixA <- matrixA / 100 # scale for computational purposes

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
EDFA

EM <- EDFA %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EM)
EM

full_df <- tidyr::expand(EM, ind, day)

EM <- left_join(full_df, EM)
EM <- as.data.frame(EM, stringsAsFactors = FALSE)
EM[is.na(EM)] <- 0

# Data Augmentation
M <- 200 # max population size
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))
z <- c(rep(1, n_ind), rep(0, M-n_ind))
df_aug <- as.data.frame(matrix(0, nrow = (M - n_ind), ncol = n_traps), stringsAsFactors = FALSE)

# convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_array <- array(NA, dim = c(M, n_traps, K))
for(i in 1:K){
  foo <- EM[(which(EM[]$day == i)), ]
  foo_less <- select(foo, -ind, -day)
  colnames(foo_less) <- colnames(df_aug)
  foo_augment <- bind_rows(foo_less, df_aug)
  EM_array[1:(M), 1:n_traps, i] <- as.matrix(foo_augment)
}

# EM <- cbind(1:n_ind, rowSums(EM))
# EM <- rowSums(EM)
# Read in trap hour file
# traphoursA <- read.csv(file = "Data/traphoursA.csv", stringsAsFactors = FALSE)
# traphoursA
# hours <- apply(traphoursA[ , 2:ncol(traphoursA)], 1, sum)
# hours

#Start values for s (activity centers) of augments (from random uniform constrained by state space size)
#X <- traplocsA
# Now populated by starting positions uniformally placed within state space
# For every individual that is not 0, change starting x point to mean of traps associated with encounters for that individual; leaves 0's there from the augmented population and also puts in activity center for augmented individuals that were randomly given an encounter history (caught at least 1 time)

sum_caps <- apply(EM_array, c(1,2), sum)
sst <- (sum_caps %*% traplocsA) / (ifelse(rowSums(sum_caps) > 0, rowSums(sum_caps), 1))

for(i in (n_ind+1):M) {
  sst[i] <- c(runif(1, xlimA[1], xlimA[2])) #parameters, n, max, min
}
sst

##### SEX VECTOR with sex (as a random variable) indicated for caught individuals and NA for augmented individuals #####

sex_list <- EDFA$sex
sex_vector <- ifelse(sex_list == "F", 1, 2)
Sex <- c(sex_vector-1, rep(NA, M-n_ind))

#### Behavior Matrix ######

BM <- EDFA %>%
  group_by(ind, day, recap) %>%
  select(ind, day, recap) %>%
  ungroup()

str(BM)
BM

BM <- as.data.frame(BM, stringsAsFactors = FALSE)
BM$behav <- ifelse(BM$recap == "R", 1, 0)
BM_less <- select(BM, -recap)

# make Cij with 1 if a recap and 0 otherwise
C_obs <- BM_less %>%
  tidyr::expand(ind, day) %>%
  left_join(BM_less) %>%
  group_by(ind) %>%
  mutate(behav = ifelse(is.na(behav), 0, behav),
         cumu = cumsum(behav)) %>%
  mutate(cij = ifelse(cumu > 0, 1, 0)) %>% # if you stop it here you can see what it's doing 
  select(ind, day, cij) %>%
  spread(key = day, value = cij, sep = "_") %>% # stop here if you want to see individual ID before dropping it
  ungroup %>%
  select(-ind)

# augment unobserved individuals
C_unobs <- as.data.frame(matrix(0, nrow = M-nrow(C_obs), ncol = 4))
colnames(C_unobs) <- colnames(C_obs)
C <- bind_rows(C_obs, C_unobs)


#nrow(BM_less[(which(BM_less$day == i)), ])

# BM_array <- array(NA, dim = c(M, K))
# for(i in 1:K){
#   df_bm_aug <- as.data.frame(matrix(0, nrow = (M - nrow(BM_less[(which(BM_less$day == i)), ])), ncol = 1), stringsAsFactors = FALSE)
#   foobm <- BM_less[(which(BM_less$day == i)), ]
#   foobm_less <- select(foobm, - ind, - day)
#   colnames(foobm_less) <- colnames(df_bm_aug)
#   foobm_augment <- bind_rows(foobm_less, df_bm_aug)
#   BM_array[1:(M), i] <- as.matrix(foobm_augment)
#   
# }  ## NEED TO ADD IN INDIVIDUALS NOT CAUGHT ON SPECIFIC TRAP DAYS, BUT CAUGHT ON OTHER TRAP OCCASIONS
# 
# C <- BM_array


#########

cat ("
     model {
     # alpha1 ~ dgamma(0.1, 0.1) # consider appropriate prior
     # alpha1 ~ dt(0, 1 / (5^2), 1)I(0, )  ## implies half-cauchy with scale of 5
     psi ~ dunif(0, 1) # giving numbers between 0 and 1, need to change?
     psi.sex ~ dunif(0, 1)
     
     for(t in 1:2){
     alpha1[t] ~ dnorm(0, 1 / (25^2))I(0, ) 	## half normal
     sigma[t] <- pow(1 / (2*alpha1[t]), 0.5) # sd of half normal
     } # t
     
     
     sigma_ind ~ dt(0, 1 / (25^2), 1)I(0, ) 	## implies half-cauchy with scale of 25
     for(i in 1:M) {
     for(k in 1:K) { ## Would it work to add an extra eta loop here for sex?
     eta[i,k] ~ dnorm(0, 1 / (sigma_ind * sigma_ind))
     } # i
     } # k
     
     for(t in 1:2) {
     alpha0[t] ~ dnorm(0, 0.1)
     } # t
    

     for(m in 1:M) {
     alpha2[m] ~ dnorm(mu_a2, 1 / sd_a2 / sd_a2)
     }
    mu_a2 ~ dnorm(0, 0.01)
    # sd_a2 ~ dt(0, pow(5, -2), 1)T(0, ) # half cauchy prior with scale = 5 (25?)
     sd_a2 ~ dunif(0, 5)

     for(i in 1:M) {
     z[i] ~ dbern(psi)
     s[i] ~ dunif(xlimA[1], xlimA[2])
     
     for(j in 1:n_traps) {
     d[i,j] <- abs(s[i] - traplocsA[j])
     
     for(k in 1:K) {
     for(t in 1:2) {
     logit(p0[i, j, k, t]) <- alpha0[t] + (alpha2[i] * C[i, k]) + eta[i, k] # alpha2*C to rep. global behav. response
     } # i
     } # j
     } # k
     } # t
     
     for(i in 1:M) {
     Sex[i] ~ dbern(psi.sex)
     Sex2[i] <- Sex[i] + 1
     for (j in 1:n_traps) {
     for (k in 1:K) {
     y[i, j, k] ~ dbern(p[i,j,k])
     p[i, j, k] <- z[i]*p0[i, j, k, Sex2[i]]* exp(- alpha1[Sex2[i]] * d[i,j] * d[i,j])
     } # i
     } # j
     } # k
     
     # Derived parameters
     N <- sum(z[ ])
     density <- N / (xlimA[2] - xlimA[1]) # divided distances by 100 so calculates turtles per 100 m of canal
     }
     ", file = "Code/JAGS/SCR_Beh.txt")


cat ("
     model {
     # alpha1 ~ dgamma(0.1, 0.1) # consider appropriate prior
     # alpha1 ~ dt(0, 1 / (5^2), 1)I(0, ) 	## implies half-cauchy with scale of 5
     psi ~ dunif(0, 1) # giving numbers between 0 and 1, need to change?
     psi.sex ~ dunif(0, 1)
     
     for(t in 1:2){
     alpha1[t] ~ dnorm(0, 1 / (25^2))I(0, ) 	## half normal
     sigma[t] <- pow(1 / (2*alpha1[t]), 0.5) # sd of half normal
     } # t
     
     
     sigma_ind ~ dt(0, 1 / (25^2), 1)I(0, ) 	## implies half-cauchy with scale of 25
     for(i in 1:M) {
     for(k in 1:K) { ## Would it work to add an extra eta loop here for sex?
     eta[i,k] ~ dnorm(0, 1 / (sigma_ind * sigma_ind))
     } # i
     } # k
     
     for(t in 1:2) {
     alpha0[t] ~ dnorm(0, 0.1)
     } # t
     
     alpha2 ~ dnorm(0, 0.01)
     
     for(i in 1:M) {
     z[i] ~ dbern(psi)
     s[i] ~ dunif(xlimA[1], xlimA[2])
     
     for(j in 1:n_traps) {
     d[i,j] <- abs(s[i] - traplocsA[j])
     
     for(k in 1:K) {
     for(t in 1:2) {
     logit(p0[i, j, k, t]) <- alpha0[t] + (alpha2 * C[i, k]) + eta[i, k] # alpha2*C to rep. global behav. response
     } # i
     } # j
     } # k
     } # t
     
     for(i in 1:M) {
     Sex[i] ~ dbern(psi.sex)
     Sex2[i] <- Sex[i] + 1
     for (j in 1:n_traps) {
     for (k in 1:K) {
     y[i, j, k] ~ dbern(p[i,j,k])
     p[i, j, k] <- z[i]*p0[i, j, k, Sex2[i]]* exp(- alpha1[Sex2[i]] * d[i,j] * d[i,j])
     } # i
     } # j
     } # k
     
     # Derived parameters
     N <- sum(z[ ])
     density <- N / (xlimA[2] - xlimA[1]) # divided distances by 100 so calculates turtles per 100 m of canal
     }
     ", file = "Code/JAGS/SCR_Beh2.txt")


############## End ITS Model ################

############ Running ITS Model ##############

jags_data_m4 <- list(y = EM_array, 
                     Sex = Sex, 
                     traplocsA = traplocsA, 
                     K=K, 
                     M=M, 
                     xlimA=xlimA, 
                     n_traps = n_traps, 
                     C = C) #, n_ind = n_ind)
# "initial values for the observed data have to be specified as NA"
inits <- function() {
  list(alpha0=rnorm(2,-2,0.5), 
       alpha1=runif(2,1,2), 
       alpha2=runif(M, 1, 2),
       s=as.numeric(sst), 
       z=z, psi = runif(1), 
       psi.sex = runif(1)) #, Sex = c(rep(NA, n_ind))) ## Error = "Invalid parameters for chain 1: non-numeric intial values supplied for variable(s) Sex"   #### ALPHA2????
}

inits2 <- function() {
  list(alpha0=rnorm(2,-2,0.5), 
       alpha1=runif(2,1,2), 
       alpha2=runif(1, 1, 2),
       s=as.numeric(sst), 
       z=z, psi = runif(1), 
       psi.sex = runif(1)) #, Sex = c(rep(NA, n_ind))) ## Error = "Invalid parameters for chain 1: non-numeric intial values supplied for variable(s) Sex"   #### ALPHA2????
}

parameters <- c("sigma", "N", "density", "s", "sigma_ind", "psi", "psi.sex", "C", "alpha2", "alpha", "sigma") 

# cpic_1_mcmc <- jagsUI(model.file = "Code/JAGS/SCRA.txt", parameters.to.save = parameters, data=jags_data, inits=inits, n.iter = 1000, n.chains = 3, n.adapt =500) # jagsUI is nice but the plotting is interactive which is obnoxious 

# plot(cpic_1_mcmc)
# traceplot(cpic_1_mcmc)
# cpic_1_mcmc


testing <- TRUE
if(testing) {
  na = 500
  ni = 500
  nt = 1
  nc = 3
} else {
  na = 100000
  ni = 600000
  nt = 60
  nc = 4
}

# run in parallel explicitly

cl <- makeCluster(nc)                        # Request # cores
clusterExport(cl, c("jags_data_m4", "inits", "inits2", "parameters", "n_ind", "z", "sst", "Sex", "ni", "na", "nt", "K", "C", "M")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Beh2.txt", jags_data_m4, inits = inits2, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out))
  })
}) #



stopCluster(cl)

out2 <- mcmc.list(out)
plot(out2[ , c("N", "density", "sigma_ind", "alpha2", "alpha0", "sigma")])

######################################

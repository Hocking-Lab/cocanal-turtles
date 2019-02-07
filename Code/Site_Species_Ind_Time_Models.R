

########## Site A CPIC Objects for Model ########
n_trapsA <- ncol(matrixA) # number of traps
# as.character(EDFA$recap)
N_A <- nrow(EDFA[which(EDFA$recap == "N"), ])
K <- max(EDFA$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimA <- c(min(matrixA)-buffer, max(matrixA) + buffer)
n_indA <- length(unique(EDFA$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFA)
EDFA

EMA <- EDFA %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EM)
EMA

full_dfA <- tidyr::expand(EMA, ind, day)

EMA <- left_join(full_dfA, EMA)
EMA <- as.data.frame(EMA, stringsAsFactors = FALSE)
EMA[is.na(EMA)] <- 0

##### DATA AUGMENTATION #####
M_A <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_A <- c(rep(1, n_indA), rep(0, M_A-n_indA))
df_augA <- as.data.frame(matrix(0, nrow = (M_A - n_indA), ncol = n_trapsA), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayA <- array(NA, dim = c(M_A, n_trapsA, K))
for(i in 1:K){
  fooA <- EMA[(which(EMA[]$day == i)), ]
  foo_lessA <- select(fooA, -ind, -day)
  colnames(foo_lessA) <- colnames(df_augA)
  foo_augmentA <- bind_rows(foo_lessA, df_augA)
  EM_arrayA[1:(M_A), 1:n_trapsA, i] <- as.matrix(foo_augmentA)
}

sum_capsA <- apply(EM_arrayA, c(1,2), sum)
sstA <- (sum_capsA %*% traplocsA) / (ifelse(rowSums(sum_capsA) > 0, rowSums(sum_capsA), 1))

for(m in (n_indA+1):M_A) {
  sstA[m] <- c(runif(1, xlimA[1], xlimA[2])) #parameters, n, max, min
}

##### Putting Objects into Model ######


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

library(rjags)
library(parallel)

jags_data <- list(y = EM_arrayA, traplocsA = traplocsA, K=K, M=M_A, xlimA=xlimA, n_traps = n_trapsA)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstA[]), z=z_A, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_A", "sstA", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_1_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_1_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_1_cpic))
  })
}) #

stopCluster(cl)

cpic_A_mcmc <- mcmc.list(out_1_cpic)

save(cpic_A_mcmc, file = "Results/JAGS/cpic_A_mcmc.RData")

####################################################


########## Site A CSER Objects for Model ########

n_trapsA <- ncol(matrixA) # number of traps
# as.character(EDFA$recap)
N_A <- nrow(EDFA_CSER[which(EDFA_CSER$recap == "N"), ])
K <- max(EDFA_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimA <- c(min(matrixA)-buffer, max(matrixA) + buffer)
n_indA <- length(unique(EDFA_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFA_CSER

EMA_CSER <- EDFA_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()

EMA_CSER

full_dfA_CSER <- tidyr::expand(EMA_CSER, ind, day)

EMA_CSER <- left_join(full_dfA_CSER, EMA_CSER)
EMA_CSER <- as.data.frame(EMA_CSER, stringsAsFactors = FALSE)
EMA_CSER[is.na(EMA_CSER)] <- 0

##### DATA AUGMENTATION #####
M_A <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_A <- c(rep(1, n_indA), rep(0, M_A-n_indA))
df_augA <- as.data.frame(matrix(0, nrow = (M_A - n_indA), ncol = n_trapsA), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayA_CSER <- array(NA, dim = c(M_A, n_trapsA, K))
for(i in 1:K){
  fooA <- EMA_CSER[(which(EMA_CSER[]$day == i)), ] ##### Nothing on day 2 so loop does not work
  foo_lessA <- select(fooA, -ind, -day)
  if(nrow(foo_lessA) > 0){
    foo_lessA$'2' <- 0
  }
  if(nrow(foo_lessA) > 0){
    foo_lessA$'3' <- 0
  }
  if(nrow(foo_lessA) > 0){
    foo_lessA$'4' <- 0
  }
  if(nrow(foo_lessA) > 0){
    foo_lessA$'8' <- 0
  }
  if(nrow(foo_lessA) == 0){
    foo_lessA <- array(0, dim = c(5, 8))
  }
  colnames(foo_lessA) <- c("1","2","3","4","5","6","7","8")
  colnames(foo_lessA) <- colnames(df_augA)
  foo_lessA <- as.data.frame(foo_lessA)
  foo_augmentA <- bind_rows(foo_lessA, df_augA)
  EM_arrayA_CSER[1:(M_A), 1:n_trapsA, i] <- as.matrix(foo_augmentA)
}

str(EM_arrayA_CSER)

sum_capsA <- apply(EM_arrayA_CSER, c(1,2), sum)
sstA <- (sum_capsA %*% traplocsA) / (ifelse(rowSums(sum_capsA) > 0, rowSums(sum_capsA), 1))

for(m in (n_indA+1):M_A) {
  sstA[m] <- c(runif(1, xlimA[1], xlimA[2])) #parameters, n, max, min
}

##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayA_CSER, traplocsA = traplocsA, K=K, M=M_A, xlimA=xlimA, n_traps = n_trapsA)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstA[]), z=z_A, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_A", "sstA", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_1_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_1_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_1_cser))
  })
}) #

stopCluster(cl)

cser_A_mcmc <- mcmc.list(out_1_cser)

save(cser_A_mcmc, file = "Results/JAGS/cser_A_mcmc.RData")


########## Site C CPIC Objects for Model ########
n_trapsC <- ncol(matrixC) # number of traps
# as.character(EDFA$recap)
N_C <- nrow(EDFC[which(EDFC$recap == "N"), ])
K <- max(EDFC$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimC <- c(min(matrixC)-buffer, max(matrixC) + buffer)
n_indC <- length(unique(EDFC$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFC)
EDFC

EMC <- EDFC %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMC)
EMC

full_dfC <- tidyr::expand(EMC, ind, day)

EMC <- left_join(full_dfC, EMC)
EMC <- as.data.frame(EMC, stringsAsFactors = FALSE)
EMC[is.na(EMC)] <- 0

##### DATA AUGMENTATION #####
M_C <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_C <- c(rep(1, n_indC), rep(0, M_A-n_indC))
df_augC <- as.data.frame(matrix(0, nrow = (M_C - n_indC), ncol = n_trapsC), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayC <- array(NA, dim = c(M_C, n_trapsC, K))
for(i in 1:K){
  fooC <- EMC[(which(EMC[]$day == i)), ]
  foo_lessC <- select(fooC, -ind, -day)
  foo_lessC$'5' <- 0
  foo_lessC$'6' <- 0
  foo_lessC <- foo_lessC[ ,c("1","2","3","4","5","6","7","8","9","10")]
  colnames(foo_lessC) <- colnames(df_augC)
  foo_augmentC <- bind_rows(foo_lessC, df_augC)
  EM_arrayC[1:(M_C), 1:n_trapsC, i] <- as.matrix(foo_augmentC)
}

sum_capsC <- apply(EM_arrayC, c(1,2), sum)
sstC <- (sum_capsC %*% traplocsC) / (ifelse(rowSums(sum_capsC) > 0, rowSums(sum_capsC), 1))

for(m in (n_indC+1):M_C) {
  sstC[m] <- c(runif(1, xlimC[1], xlimC[2])) #parameters, n, max, min
}

##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayC, traplocsA = traplocsC, K=K, M=M_C, xlimA=xlimC, n_traps = n_trapsC)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstC[]), z=z_C, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_C", "sstC", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_C_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_C_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_C_cpic))
  })
}) #

stopCluster(cl)

cpic_C_mcmc <- mcmc.list(out_C_cpic)

save(cpic_C_mcmc, file = "Results/JAGS/cpic_C_mcmc.RData")


########################################################

########## Site C CSER Objects for Model ################

n_trapsC <- ncol(matrixC) # number of traps
# as.character(EDFA$recap)
N_C <- nrow(EDFC_CSER[which(EDFC_CSER$recap == "N"), ])
K <- max(EDFC_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimC <- c(min(matrixC)-buffer, max(matrixC) + buffer)
n_indC <- length(unique(EDFC_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFC_CSER

EMC_CSER <- EDFC_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMC_CSER

full_dfC_CSER <- tidyr::expand(EMC_CSER, ind, day)

EMC_CSER <- left_join(full_dfC_CSER, EMC_CSER)
EMC_CSER <- as.data.frame(EMC_CSER, stringsAsFactors = FALSE)
EMC_CSER[is.na(EMC_CSER)] <- 0

##### DATA AUGMENTATION #####
M_C <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_C <- c(rep(1, n_indC), rep(0, M_A-n_indC))
df_augC <- as.data.frame(matrix(0, nrow = (M_C - n_indC), ncol = n_trapsC), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayC_CSER <- array(NA, dim = c(M_C, n_trapsC, K))
for(i in 1:K){
  fooC <- EMC_CSER[(which(EMC_CSER[]$day == i)), ]
  foo_lessC <- select(fooC, -ind, -day)
  foo_lessC$'1' <- 0
  foo_lessC$'5' <- 0
  foo_lessC$'6' <- 0
  foo_lessC$'10' <- 0
  foo_lessC <- foo_lessC[ ,c("1","2","3","4","5","6","7","8","9","10")]
  colnames(foo_lessC) <- colnames(df_augC)
  foo_augmentC <- bind_rows(foo_lessC, df_augC)
  EM_arrayC_CSER[1:(M_C), 1:n_trapsC, i] <- as.matrix(foo_augmentC)
}

sum_capsC <- apply(EM_arrayC_CSER, c(1,2), sum)
sstC <- (sum_capsC %*% traplocsC) / (ifelse(rowSums(sum_capsC) > 0, rowSums(sum_capsC), 1))

for(m in (n_indC+1):M_C) {
  sstC[m] <- c(runif(1, xlimC[1], xlimC[2])) #parameters, n, max, min
}

##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayC_CSER, traplocsA = traplocsC, K=K, M=M_C, xlimA=xlimC, n_traps = n_trapsC)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstC[]), z=z_C, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_C", "sstC", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_C_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_C_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_C_cser))
  })
}) #

stopCluster(cl)

cser_C_mcmc <- mcmc.list(out_C_cser)

save(cser_C_mcmc, file = "Results/JAGS/cser_C_mcmc.RData")


########## Site D CPIC Objects for Model ###############

n_trapsD <- ncol(matrixD) # number of traps
# as.character(EDFA$recap)
N_D<- nrow(EDFD[which(EDFD$recap == "N"), ])
K <- max(EDFD$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimD <- c(min(matrixD)-buffer, max(matrixD) + buffer)
n_indD <- length(unique(EDFD$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFD)
EDFD

EMD <- EDFD %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMD)
EMD

full_dfD <- tidyr::expand(EMD, ind, day)

EMD <- left_join(full_dfD, EMD)
EMD <- as.data.frame(EMD, stringsAsFactors = FALSE)
EMD[is.na(EMD)] <- 0

##### DATA AUGMENTATION #####
M_D <- 200 # max population size, change with site
# J <- n_traps

z_D <- c(rep(1, n_indD), rep(0, M_D-n_indD))
df_augD <- as.data.frame(matrix(0, nrow = (M_D - n_indD), ncol = n_trapsD), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayD <- array(NA, dim = c(M_D, n_trapsD, K))
for(i in 1:K){
  fooD <- EMD[(which(EMD[]$day == i)), ]
  foo_lessD <- select(fooD, -ind, -day)
  foo_lessD$'1' <- 0
  foo_lessD$'6' <- 0
  foo_lessD$'8' <- 0
  foo_lessD <- foo_lessD[ ,c("1","2","3","4","5","6","7","8")]
  colnames(foo_lessD) <- colnames(df_augD)
  foo_augmentD <- bind_rows(foo_lessD, df_augD)
  EM_arrayD[1:(M_D), 1:n_trapsD, i] <- as.matrix(foo_augmentD)
}

sum_capsD <- apply(EM_arrayD, c(1,2), sum)
sstD <- (sum_capsD %*% traplocsD) / (ifelse(rowSums(sum_capsD) > 0, rowSums(sum_capsD), 1))

for(m in (n_indD+1):M_D) {
  sstD[m] <- c(runif(1, xlimD[1], xlimD[2])) #parameters, n, max, min
}

##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayD, traplocsA = traplocsD, K=K, M=M_D, xlimA=xlimD, n_traps = n_trapsD)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstD[]), z=z_D, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_D", "sstD", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_D_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_D_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_D_cpic))
  })
}) #

stopCluster(cl)

cpic_D_mcmc <- mcmc.list(out_D_cpic)

save(cpic_D_mcmc, file = "Results/JAGS/cpic_D_mcmc.RData")


#########################################################


########## Site E CPIC Objects for Model ################

n_trapsE <- ncol(matrixE) # number of traps
# as.character(EDFA$recap)
N_E <- nrow(EDFE[which(EDFE$recap == "N"), ])
K <- max(EDFE$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimE <- c(min(matrixE)-buffer, max(matrixE) + buffer)
n_indE <- length(unique(EDFE$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFE)
EDFE

EME <- EDFE %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EME)
EME

full_dfE <- tidyr::expand(EME, ind, day)

EME <- left_join(full_dfE, EME)
EME <- as.data.frame(EME, stringsAsFactors = FALSE)
EME[is.na(EME)] <- 0

##### DATA AUGMENTATION #####
M_E <- 300 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_E <- c(rep(1, n_indE), rep(0, M_E-n_indE))
df_augE <- as.data.frame(matrix(0, nrow = (M_E - n_indE), ncol = n_trapsE), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayE <- array(NA, dim = c(M_E, n_trapsE, K))
for(i in 1:K){
  fooE <- EME[(which(EME[]$day == i)), ]
  foo_lessE <- select(fooE, -ind, -day)
  foo_lessE$'1' <- 0
  foo_lessE$'7' <- 0
  foo_lessE <- foo_lessE[ ,c("1","2","3","4","5","6","7","8","9","10","11","12","13","14")]
  colnames(foo_lessE) <- colnames(df_augE)
  foo_augmentE <- bind_rows(foo_lessE, df_augE)
  EM_arrayE[1:(M_E), 1:n_trapsE, i] <- as.matrix(foo_augmentE)
}

sum_capsE <- apply(EM_arrayE, c(1,2), sum)
sstE <- (sum_capsE %*% traplocsE) / (ifelse(rowSums(sum_capsE) > 0, rowSums(sum_capsE), 1))

for(m in (n_indE+1):M_E) {
  sstE[m] <- c(runif(1, xlimE[1], xlimE[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayE, traplocsA = traplocsE, K=K, M=M_E, xlimA=xlimE, n_traps = n_trapsE)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstE[]), z=z_E, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_E", "sstE", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_E_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_E_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_E_cpic))
  })
}) #

stopCluster(cl)

cpic_E_mcmc <- mcmc.list(out_E_cpic)

save(cpic_E_mcmc, file = "Results/JAGS/cpic_E_mcmc.RData")


###################################################

########## Site E CSER Objects for Model ################

n_trapsE <- ncol(matrixE) # number of traps
# as.character(EDFA$recap)
N_E <- nrow(EDFE_CSER[which(EDFE_CSER$recap == "N"), ])
K <- max(EDFE_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimE <- c(min(matrixE)-buffer, max(matrixE) + buffer)
n_indE <- length(unique(EDFE_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFE_CSER

EME_CSER <- EDFE_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EME_CSER

full_dfE <- tidyr::expand(EME_CSER, ind, day)

EME_CSER <- left_join(full_dfE, EME_CSER)
EME_CSER <- as.data.frame(EME_CSER, stringsAsFactors = FALSE)
EME_CSER[is.na(EME_CSER)] <- 0

##### DATA AUGMENTATION #####
M_E <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_E <- c(rep(1, n_indE), rep(0, M_E-n_indE))
df_augE <- as.data.frame(matrix(0, nrow = (M_E - n_indE), ncol = n_trapsE), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayE_CSER <- array(NA, dim = c(M_E, n_trapsE, K))
for(i in 1:K){
  fooA <- EME_CSER[(which(EME_CSER[]$day == i)), ] ##### Nothing on day X, so have to use if,then
  foo_lessE <- select(fooE, -ind, -day)
  if(nrow(foo_lessE) > 0){
    foo_lessE$'1' <- 0
  }
  if(nrow(foo_lessE) > 0){
    foo_lessE$'2' <- 0
  }
  if(nrow(foo_lessE) > 0){
    foo_lessE$'5' <- 0
  }
  if(nrow(foo_lessE) > 0){
    foo_lessE$'7' <- 0
  }
  if(nrow(foo_lessE) > 0){
    foo_lessE$'12' <- 0
  }
  if(nrow(foo_lessE) > 0){
    foo_lessE$'13' <- 0
  }
  if(nrow(foo_lessE) == 0){
    foo_lessE <- array(0, dim = c(11, 14))
  }
  colnames(foo_lessE) <- c("1","2","3","4","5","6","7","8","9","10","11","12","13","14")
  colnames(foo_lessE) <- colnames(df_augE)
  foo_lessE <- as.data.frame(foo_lessE)
  foo_augmentE <- bind_rows(foo_lessE, df_augE)
  EM_arrayE_CSER[1:(M_E), 1:n_trapsE, i] <- as.matrix(foo_augmentE)
}

str(EM_arrayA_CSER)

sum_capsE <- apply(EM_arrayE_CSER, c(1,2), sum)
sstE <- (sum_capsE %*% traplocsE) / (ifelse(rowSums(sum_capsE) > 0, rowSums(sum_capsE), 1))

for(m in (n_indE+1):M_E) {
  sstE[m] <- c(runif(1, xlimE[1], xlimE[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayE_CSER, traplocsA = traplocsE, K=K, M=M_E, xlimA=xlimE, n_traps = n_trapsE)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstE[]), z=z_E, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_E", "sstE", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_E_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_E_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_E_cser))
  })
}) #

stopCluster(cl)

cser_E_mcmc <- mcmc.list(out_E_cser)

save(cser_E_mcmc, file = "Results/JAGS/cser_E_mcmc.RData")


########## Site F CPIC Objects for Model ################

n_trapsF <- ncol(matrixF) # number of traps
# as.character(EDFA$recap)
N_F <- nrow(EDFF[which(EDFF$recap == "N"), ])
K <- max(EDFF$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimF <- c(min(matrixF)-buffer, max(matrixF) + buffer)
n_indF <- length(unique(EDFF$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFF)
EDFF

EMF <- EDFF %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMF)
EMF

full_dfF <- tidyr::expand(EMF, ind, day)

EMF <- left_join(full_dfF, EMF)
EMF <- as.data.frame(EMF, stringsAsFactors = FALSE)
EMF[is.na(EMF)] <- 0

##### DATA AUGMENTATION #####
M_F <- 1000 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_F <- c(rep(1, n_indF), rep(0, M_F-n_indF))
df_augF <- as.data.frame(matrix(0, nrow = (M_F - n_indF), ncol = n_trapsF), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayF <- array(NA, dim = c(M_F, n_trapsF, K))
for(i in 1:K){
  fooF <- EMF[(which(EMF[]$day == i)), ]
  foo_lessF <- select(fooF, -ind, -day)
  colnames(foo_lessF) <- colnames(df_augF)
  foo_augmentF <- bind_rows(foo_lessF, df_augF)
  EM_arrayF[1:(M_F), 1:n_trapsF, i] <- as.matrix(foo_augmentF)
}

sum_capsF <- apply(EM_arrayF, c(1,2), sum)
sstF <- (sum_capsF %*% traplocsF) / (ifelse(rowSums(sum_capsF) > 0, rowSums(sum_capsF), 1))

for(m in (n_indF+1):M_F) {
  sstF[m] <- c(runif(1, xlimF[1], xlimF[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayF, traplocsA = traplocsF, K=K, M=M_F, xlimA=xlimF, n_traps = n_trapsF)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstF[]), z=z_F, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_F", "sstF", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_F_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_F_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_F_cpic))
  })
}) #

stopCluster(cl)

cpic_F_mcmc <- mcmc.list(out_F_cpic)

save(cpic_F_mcmc, file = "Results/JAGS/cpic_F_mcmc.RData")


#######################################################

########## Site F CSER Objects for Model ################

n_trapsF <- ncol(matrixF) # number of traps
# as.character(EDFA$recap)
N_F <- nrow(EDFF_CSER[which(EDFF_CSER$recap == "N"), ])
K <- max(EDFF_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimF <- c(min(matrixF)-buffer, max(matrixF) + buffer)
n_indF <- length(unique(EDFF_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFF_CSER

EMF_CSER <- EDFF_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMF_CSER

full_dfF <- tidyr::expand(EMF_CSER, ind, day)

EMF_CSER <- left_join(full_dfF, EMF_CSER)
EMF_CSER <- as.data.frame(EMF_CSER, stringsAsFactors = FALSE)
EMF_CSER[is.na(EMF_CSER)] <- 0

##### DATA AUGMENTATION #####
M_F <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_F <- c(rep(1, n_indF), rep(0, M_F - n_indF))
df_augF <- as.data.frame(matrix(0, nrow = (M_F - n_indF), ncol = n_trapsF), stringsAsFactors = FALSE)
rm(df_augF)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayF_CSER <- array(NA, dim = c(M_F, n_trapsF, K))
for(i in 1:K){
  fooF <- EMF_CSER[(which(EMF_CSER[]$day == i)), ]
  foo_lessF <- select(fooF, -ind, -day)
  if(nrow(foo_lessF) > 0){
    foo_lessF$'1' <- 0
  }
  if(nrow(foo_lessF) > 0){
    foo_lessF$'6' <- 0
  }
  if(nrow(foo_lessF) == 0){
    foo_lessF <- array(0, dim = c(8, 7))
  }
  colnames(foo_lessF) <- c("1","2","3","4","5","6","7")
  colnames(foo_lessF) <- colnames(df_augF)
  foo_lessF <- as.data.frame(foo_lessF)
  foo_augmentF <- bind_rows(foo_lessF, df_augF)
  EM_arrayF_CSER[1:(M_F), 1:n_trapsF, i] <- as.matrix(foo_augmentF)
}

sum_capsF <- apply(EM_arrayF_CSER, c(1,2), sum)
sstF <- (sum_capsF %*% traplocsF) / (ifelse(rowSums(sum_capsF) > 0, rowSums(sum_capsF), 1))

for(m in (n_indF+1):M_F) {
  sstF[m] <- c(runif(1, xlimF[1], xlimF[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayF_CSER, traplocsA = traplocsF, K=K, M=M_F, xlimA=xlimF, n_traps = n_trapsF)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstF[]), z=z_F, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_F", "sstF", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_F_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_F_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_F_cser))
  })
}) #

stopCluster(cl)

cser_F_mcmc <- mcmc.list(out_F_cser)

save(cser_F_mcmc, file = "Results/JAGS/cser_F_mcmc.RData")


########## Site G CPIC Objects for Model ##############

n_trapsG <- ncol(matrixG) # number of traps
# as.character(EDFA$recap)
N_G <- nrow(EDFG[which(EDFG$recap == "N"), ])
K <- max(EDFG$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimG <- c(min(matrixG)-buffer, max(matrixG) + buffer)
n_indG <- length(unique(EDFG$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFG)
EDFG

EMG <- EDFG %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMG)
EMG

full_dfG <- tidyr::expand(EMG, ind, day)

EMG <- left_join(full_dfG, EMG)
EMG <- as.data.frame(EMG, stringsAsFactors = FALSE)
EMG[is.na(EMG)] <- 0

##### DATA AUGMENTATION #####
M_G <- 400 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_G <- c(rep(1, n_indG), rep(0, M_G-n_indG))
df_augG <- as.data.frame(matrix(0, nrow = (M_G - n_indG), ncol = n_trapsG), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayG <- array(NA, dim = c(M_G, n_trapsG, K))
for(i in 1:K){
  fooG <- EMG[(which(EMG[]$day == i)), ]
  foo_lessG <- select(fooG, -ind, -day)
  colnames(foo_lessG) <- colnames(df_augG)
  foo_augmentG <- bind_rows(foo_lessG, df_augG)
  EM_arrayG[1:(M_G), 1:n_trapsG, i] <- as.matrix(foo_augmentG)
}

sum_capsG <- apply(EM_arrayG, c(1,2), sum)
sstG <- (sum_capsG %*% traplocsG) / (ifelse(rowSums(sum_capsG) > 0, rowSums(sum_capsG), 1))

for(m in (n_indG+1):M_G) {
  sstG[m] <- c(runif(1, xlimG[1], xlimG[2])) #parameters, n, max, min
}

##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayG, traplocsA = traplocsG, K=K, M=M_G, xlimA=xlimG, n_traps = n_trapsG)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstG[]), z=z_G, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_G", "sstG", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_G_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_G_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_G_cpic))
  })
}) #

stopCluster(cl)

cpic_G_mcmc <- mcmc.list(out_G_cpic)

save(cpic_G_mcmc, file = "Results/JAGS/cpic_G_mcmc.RData")

###################################################


########## Site G CSER Objects for Model ################

n_trapsG <- ncol(matrixG) # number of traps
# as.character(EDFA$recap)
N_G <- nrow(EDFG_CSER[which(EDFG_CSER$recap == "N"), ])
K <- max(EDFG_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimG <- c(min(matrixG)-buffer, max(matrixG) + buffer)
n_indG <- length(unique(EDFG_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFG_CSER

EMG_CSER <- EDFG_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMG_CSER

full_dfG <- tidyr::expand(EMG_CSER, ind, day)

EMG_CSER <- left_join(full_dfG, EMG_CSER)
EMG_CSER <- as.data.frame(EMG_CSER, stringsAsFactors = FALSE)
EMG_CSER[is.na(EMG_CSER)] <- 0

##### DATA AUGMENTATION #####
M_G <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_G <- c(rep(1, n_indG), rep(0, M_G - n_indG))
df_augG <- as.data.frame(matrix(0, nrow = (M_G - n_indG), ncol = n_trapsG), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayG_CSER <- array(NA, dim = c(M_G, n_trapsG, K))
for(i in 1:K){
  fooG <- EMG_CSER[(which(EMG_CSER[]$day == i)), ]
  foo_lessG <- select(fooG, -ind, -day)
  if(nrow(foo_lessG) > 0){
    foo_lessG$'3' <- 0
  }
  if(nrow(foo_lessG) > 0){
    foo_lessG$'4' <- 0
  }
  if(nrow(foo_lessG) > 0){
    foo_lessG$'6' <- 0
  }
  if(nrow(foo_lessG) == 0){
    foo_lessG <- array(0, dim = c(5, 7))
  }
  colnames(foo_lessG) <- c("1","2","3","4","5","6","7")
  colnames(foo_lessG) <- colnames(df_augG)
  foo_lessG <- as.data.frame(foo_lessG)
  foo_augmentG <- bind_rows(foo_lessG, df_augG)
  EM_arrayG_CSER[1:(M_G), 1:n_trapsG, i] <- as.matrix(foo_augmentG)
}

sum_capsG <- apply(EM_arrayG_CSER, c(1,2), sum)
sstG <- (sum_capsG %*% traplocsG) / (ifelse(rowSums(sum_capsG) > 0, rowSums(sum_capsG), 1))

for(m in (n_indG+1):M_G) {
  sstG[m] <- c(runif(1, xlimG[1], xlimG[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayG_CSER, traplocsA = traplocsG, K=K, M=M_G, xlimA=xlimG, n_traps = n_trapsG)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstG[]), z=z_G, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_G", "sstG", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_G_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_G_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_G_cser))
  })
}) #

stopCluster(cl)

cser_G_mcmc <- mcmc.list(out_G_cser)

save(cser_G_mcmc, file = "Results/JAGS/cser_G_mcmc.RData")


########## Site J CPIC Objects for Model ###########

n_trapsJ <- ncol(matrixJ) # number of traps
# as.character(EDFA$recap)
N_J <- nrow(EDFJ[which(EDFJ$recap == "N"), ])
K <- max(EDFJ$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimJ <- c(min(matrixJ)-buffer, max(matrixJ) + buffer)
n_indJ <- length(unique(EDFJ$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFJ)
EDFJ

EMJ <- EDFJ %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMJ)
EMJ

full_dfJ <- tidyr::expand(EMJ, ind, day)

EMJ <- left_join(full_dfJ, EMJ)
EMJ <- as.data.frame(EMJ, stringsAsFactors = FALSE)
EMJ[is.na(EMJ)] <- 0

##### DATA AUGMENTATION #####
M_J <- 500 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_J <- c(rep(1, n_indJ), rep(0, M_J-n_indJ))
df_augJ <- as.data.frame(matrix(0, nrow = (M_J - n_indJ), ncol = n_trapsJ), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayJ <- array(NA, dim = c(M_J, n_trapsJ, K))
for(i in 1:K){
  fooJ <- EMJ[(which(EMJ[]$day == i)), ]
  foo_lessJ <- select(fooJ, -ind, -day)
  colnames(foo_lessJ) <- colnames(df_augJ)
  foo_augmentJ <- bind_rows(foo_lessJ, df_augJ)
  EM_arrayJ[1:(M_J), 1:n_trapsJ, i] <- as.matrix(foo_augmentJ)
}

sum_capsJ <- apply(EM_arrayJ, c(1,2), sum)
sstJ <- (sum_capsJ %*% traplocsJ) / (ifelse(rowSums(sum_capsJ) > 0, rowSums(sum_capsJ), 1))

for(m in (n_indJ+1):M_J) {
  sstJ[m] <- c(runif(1, xlimJ[1], xlimJ[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayJ, traplocsA = traplocsJ, K=K, M=M_J, xlimA=xlimJ, n_traps = n_trapsJ)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstJ[]), z=z_J, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_J", "sstJ", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_J_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_J_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_J_cpic))
  })
}) #

stopCluster(cl)

cpic_J_mcmc <- mcmc.list(out_J_cpic)

save(cpic_J_mcmc, file = "Results/JAGS/cpic_J_mcmc.RData")


####################################################

########## Site J CSER Objects for Model ###########

n_trapsJ <- ncol(matrixJ) # number of traps
# as.character(EDFA$recap)
N_J <- nrow(EDFJ_CSER[which(EDFJ_CSER$recap == "N"), ])
K <- max(EDFJ_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimJ <- c(min(matrixJ)-buffer, max(matrixJ) + buffer)
n_indJ <- length(unique(EDFJ_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFJ_CSER

EMJ_CSER <- EDFJ_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMJ_CSER

full_dfJ <- tidyr::expand(EMJ_CSER, ind, day)

EMJ_CSER <- left_join(full_dfJ, EMJ_CSER)
EMJ_CSER <- as.data.frame(EMJ_CSER, stringsAsFactors = FALSE)
EMJ_CSER[is.na(EMJ_CSER)] <- 0

##### DATA AUGMENTATION #####
M_J <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_J <- c(rep(1, n_indJ), rep(0, M_J-n_indJ))
df_augJ <- as.data.frame(matrix(0, nrow = (M_J - n_indJ), ncol = n_trapsJ), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayJ_CSER <- array(NA, dim = c(M_J, n_trapsJ, K))
for(i in 1:K){
  fooJ <- EMJ_CSER[(which(EMJ_CSER[]$day == i)), ]
  foo_lessJ <- select(fooJ, -ind, -day)
  if(nrow(foo_lessJ) > 0){
    foo_lessJ$'1' <- 0
  }
  if(nrow(foo_lessJ) > 0){
    foo_lessJ$'2' <- 0
  }
  if(nrow(foo_lessJ) > 0){
    foo_lessJ$'3' <- 0
  }
  if(nrow(foo_lessJ) > 0){
    foo_lessJ$'6' <- 0
  }
  if(nrow(foo_lessJ) > 0){
    foo_lessJ$'7' <- 0
  }
  if(nrow(foo_lessJ) > 0){
    foo_lessJ$'9' <- 0
  }
  if(nrow(foo_lessJ) == 0){
    foo_lessJ <- array(0, dim = c(5, 10))
  }
  colnames(foo_lessJ) <- c("1","2","3","4","5","6","7","8","9","10")
  colnames(foo_lessJ) <- colnames(df_augJ)
  foo_lessJ <- as.data.frame(foo_lessJ)
  foo_augmentJ <- bind_rows(foo_lessJ, df_augJ)
  EM_arrayJ_CSER[1:(M_J), 1:n_trapsJ, i] <- as.matrix(foo_augmentJ)
}

sum_capsJ <- apply(EM_arrayJ_CSER, c(1,2), sum)
sstJ <- (sum_capsJ %*% traplocsJ) / (ifelse(rowSums(sum_capsJ) > 0, rowSums(sum_capsJ), 1))

for(m in (n_indJ+1):M_J) {
  sstJ[m] <- c(runif(1, xlimJ[1], xlimJ[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayJ_CSER, traplocsA = traplocsJ, K=K, M=M_J, xlimA=xlimJ, n_traps = n_trapsJ)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstJ[]), z=z_J, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_J", "sstJ", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_J_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_J_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_J_cser))
  })
}) #

stopCluster(cl)

cser_J_mcmc <- mcmc.list(out_J_cser)

save(cser_J_mcmc, file = "Results/JAGS/cser_J_mcmc.RData")


########## Site K CPIC Objects for Model ###########

n_trapsK <- ncol(matrixK) # number of traps
# as.character(EDFA$recap)
N_K <- nrow(EDFK[which(EDFK$recap == "N"), ])
K <- max(EDFK$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimK <- c(min(matrixK)-buffer, max(matrixK) + buffer)
n_indK <- length(unique(EDFK$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFK)
EDFK

EMK <- EDFK %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMK)
EMK

full_dfK <- tidyr::expand(EMK, ind, day)

EMK <- left_join(full_dfK, EMK)
EMK <- as.data.frame(EMK, stringsAsFactors = FALSE)
EMK[is.na(EMK)] <- 0

##### DATA AUGMENTATION #####
M_K <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_K <- c(rep(1, n_indK), rep(0, M_K-n_indK))
df_augK <- as.data.frame(matrix(0, nrow = (M_K - n_indK), ncol = n_trapsK), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayK <- array(NA, dim = c(M_K, n_trapsK, K))
for(i in 1:K){
  fooK <- EMK[(which(EMK[]$day == i)), ]
  foo_lessK <- select(fooK, -ind, -day)
  colnames(foo_lessK) <- colnames(df_augK)
  foo_augmentK <- bind_rows(foo_lessK, df_augK)
  EM_arrayK[1:(M_K), 1:n_trapsK, i] <- as.matrix(foo_augmentK)
}

sum_capsK <- apply(EM_arrayK, c(1,2), sum)
sstK <- (sum_capsK %*% traplocsK) / (ifelse(rowSums(sum_capsK) > 0, rowSums(sum_capsK), 1))

for(m in (n_indK+1):M_K) {
  sstK[m] <- c(runif(1, xlimK[1], xlimK[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayK, traplocsA = traplocsK, K=K, M=M_K, xlimA=xlimK, n_traps = n_trapsK)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstK[]), z=z_K, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_K", "sstK", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_K_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_K_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_K_cpic))
  })
}) #

stopCluster(cl)

cpic_K_mcmc <- mcmc.list(out_K_cpic)

save(cpic_K_mcmc, file = "Results/JAGS/cpic_K_mcmc.RData")

####################################################


########## Site L CPIC Objects for Model ###########

n_trapsL <- ncol(matrixL) # number of traps
# as.character(EDFA$recap)
N_L <- nrow(EDFL[which(EDFL$recap == "N"), ])
K <- max(EDFL$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimL <- c(min(matrixL)-buffer, max(matrixL) + buffer)
n_indL <- length(unique(EDFL$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFL)
EDFL

EML <- EDFL %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EML)
EML

full_dfL <- tidyr::expand(EML, ind, day)

EML <- left_join(full_dfL, EML)
EML <- as.data.frame(EML, stringsAsFactors = FALSE)
EML[is.na(EML)] <- 0

##### DATA AUGMENTATION #####
M_L <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_L <- c(rep(1, n_indL), rep(0, M_L-n_indL))
df_augL <- as.data.frame(matrix(0, nrow = (M_L - n_indL), ncol = n_trapsL), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayL <- array(NA, dim = c(M_L, n_trapsL, K))
for(i in 1:K){
  fooL <- EML[(which(EML[]$day == i)), ]
  foo_lessL <- select(fooL, -ind, -day)
  foo_lessL$'3' <- 0
  foo_lessL <- foo_lessL[ ,c("1","2","3","4","5","6","7","8")]
  colnames(foo_lessL) <- colnames(df_augL)
  foo_augmentL <- bind_rows(foo_lessL, df_augL)
  EM_arrayL[1:(M_L), 1:n_trapsL, i] <- as.matrix(foo_augmentL)
}

sum_capsL <- apply(EM_arrayL, c(1,2), sum)
sstL <- (sum_capsL %*% traplocsL) / (ifelse(rowSums(sum_capsL) > 0, rowSums(sum_capsL), 1))

for(m in (n_indL+1):M_L) {
  sstL[m] <- c(runif(1, xlimL[1], xlimL[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayL, traplocsA = traplocsL, K=K, M=M_L, xlimA=xlimL, n_traps = n_trapsL)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstL[]), z=z_L, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_L", "sstL", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_L_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_L_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_L_cpic))
  })
}) #

stopCluster(cl)

cpic_L_mcmc <- mcmc.list(out_L_cpic)

save(cpic_L_mcmc, file = "Results/JAGS/cpic_L_mcmc.RData")

####################################################

########## Site M CPIC Objects for Model ###########

n_trapsM <- ncol(matrixM) # number of traps
# as.character(EDFA$recap)
N_M <- nrow(EDFM[which(EDFM$recap == "N"), ])
K <- max(EDFM$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimM <- c(min(matrixM)-buffer, max(matrixM) + buffer)
n_indM <- length(unique(EDFM$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFM)
EDFM

EMM <- EDFM %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMM)
EMM

full_dfM <- tidyr::expand(EMM, ind, day)

EMM <- left_join(full_dfM, EMM)
EMM <- as.data.frame(EMM, stringsAsFactors = FALSE)
EMM[is.na(EMM)] <- 0

##### DATA AUGMENTATION #####
M_M <- 800 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_M <- c(rep(1, n_indM), rep(0, M_M-n_indM))
df_augM <- as.data.frame(matrix(0, nrow = (M_M - n_indM), ncol = n_trapsM), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayM <- array(NA, dim = c(M_M, n_trapsM, K))
for(i in 1:K){
  fooM <- EMM[(which(EMM[]$day == i)), ]
  foo_lessM <- select(fooM, -ind, -day)
  colnames(foo_lessM) <- colnames(df_augM)
  foo_augmentM <- bind_rows(foo_lessM, df_augM)
  EM_arrayM[1:(M_M), 1:n_trapsM, i] <- as.matrix(foo_augmentM)
}

sum_capsM <- apply(EM_arrayM, c(1,2), sum)
sstM <- (sum_capsM %*% traplocsM) / (ifelse(rowSums(sum_capsM) > 0, rowSums(sum_capsM), 1))

for(m in (n_indM+1):M_M) {
  sstM[m] <- c(runif(1, xlimM[1], xlimM[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayM, traplocsA = traplocsM, K=K, M=M_M, xlimA=xlimM, n_traps = n_trapsM)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstM[]), z=z_M, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_M", "sstM", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_M_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_M_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_M_cpic))
  })
}) #

stopCluster(cl)

cpic_M_mcmc <- mcmc.list(out_M_cpic)

save(cpic_M_mcmc, file = "Results/JAGS/cpic_M_mcmc.RData")


####################################################


########## Site M CSER Objects for Model ###########

n_trapsM <- ncol(matrixM) # number of traps
# as.character(EDFA$recap)
N_M <- nrow(EDFM_CSER[which(EDFM_CSER$recap == "N"), ])
K <- max(EDFM_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimM <- c(min(matrixM)-buffer, max(matrixM) + buffer)
n_indM <- length(unique(EDFM_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFM_CSER

EMM_CSER <- EDFM_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMM_CSER

full_dfM <- tidyr::expand(EMM_CSER, ind, day)

EMM_CSER <- left_join(full_dfM, EMM_CSER)
EMM_CSER <- as.data.frame(EMM_CSER, stringsAsFactors = FALSE)
EMM_CSER[is.na(EMM_CSER)] <- 0

##### DATA AUGMENTATION #####
M_M <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_M <- c(rep(1, n_indM), rep(0, M_M-n_indM))
df_augM <- as.data.frame(matrix(0, nrow = (M_M - n_indM), ncol = n_trapsM), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayM_CSER <- array(NA, dim = c(M_M, n_trapsM, K))
for(i in 1:K){
  fooM <- EMM_CSER[(which(EMM_CSER[]$day == i)), ]
  foo_lessM <- select(fooM, -ind, -day)
  if(nrow(foo_lessM) > 0){
    foo_lessM$'2' <- 0
  }
  if(nrow(foo_lessM) > 0){
    foo_lessM$'3' <- 0
  }
  if(nrow(foo_lessM) > 0){
    foo_lessM$'5' <- 0
  }
  if(nrow(foo_lessM) > 0){
    foo_lessM$'8' <- 0
  }
  if(nrow(foo_lessM) > 0){
    foo_lessM$'9' <- 0
  }
  if(nrow(foo_lessM) > 0){
    foo_lessM$'11' <- 0
  }
  if(nrow(foo_lessM) == 0){
    foo_lessM <- array(0, dim = c(8, 12))
  }
  colnames(foo_lessM) <- c("1","2","3","4","5","6","7","8","9","10","11","12")
  colnames(foo_lessM) <- colnames(df_augM)
  foo_lessM <- as.data.frame(foo_lessM)
  foo_augmentM <- bind_rows(foo_lessM, df_augM)
  EM_arrayM_CSER[1:(M_M), 1:n_trapsM, i] <- as.matrix(foo_augmentM)
}

sum_capsM <- apply(EM_arrayM_CSER, c(1,2), sum)
sstM <- (sum_capsM %*% traplocsM) / (ifelse(rowSums(sum_capsM) > 0, rowSums(sum_capsM), 1))

for(m in (n_indM+1):M_M) {
  sstM[m] <- c(runif(1, xlimM[1], xlimM[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayM_CSER, traplocsA = traplocsM, K=K, M=M_M, xlimA=xlimM, n_traps = n_trapsM)
inits <- function() {
  list(alpha0=rnorm(2,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstM[]), z=z_M, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_M", "sstM", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_M_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_M_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_M_cser))
  })
}) #

stopCluster(cl)

cser_M_mcmc <- mcmc.list(out_M_cser)

save(cser_M_mcmc, file = "Results/JAGS/cser_M_mcmc.RData")


####################################################


########## Site N CPIC Objects for Model ###########

n_trapsN <- ncol(matrixN) # number of traps
# as.character(EDFA$recap)
N_N <- nrow(EDFN[which(EDFN$recap == "N"), ])
K <- max(EDFN$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimN <- c(min(matrixN)-buffer, max(matrixN) + buffer)
n_indN <- length(unique(EDFN$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFN)
EDFN

EMN <- EDFN %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMN)
EMN

full_dfN <- tidyr::expand(EMN, ind, day)

EMN <- left_join(full_dfN, EMN)
EMN <- as.data.frame(EMN, stringsAsFactors = FALSE)
EMN[is.na(EMN)] <- 0

##### DATA AUGMENTATION #####
M_N <- 800 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_N <- c(rep(1, n_indN), rep(0, M_N-n_indN))
df_augN <- as.data.frame(matrix(0, nrow = (M_N - n_indN), ncol = n_trapsN), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayN <- array(NA, dim = c(M_N, n_trapsN, K))
for(i in 1:K){
  fooN <- EMN[(which(EMN[]$day == i)), ]
  foo_lessN <- select(fooN, -ind, -day)
  colnames(foo_lessN) <- colnames(df_augN)
  foo_augmentN <- bind_rows(foo_lessN, df_augN)
  EM_arrayN[1:(M_N), 1:n_trapsN, i] <- as.matrix(foo_augmentN)
}

sum_capsN <- apply(EM_arrayN, c(1,2), sum)
sstN <- (sum_capsN %*% traplocsN) / (ifelse(rowSums(sum_capsN) > 0, rowSums(sum_capsN), 1))

for(m in (n_indN+1):M_N) {
  sstN[m] <- c(runif(1, xlimN[1], xlimN[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayN, traplocsA = traplocsN, K=K, M=M_N, xlimA=xlimN, n_traps = n_trapsN)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstN[]), z=z_N, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_N", "sstN", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_N_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_N_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_N_cpic))
  })
}) #

stopCluster(cl)

cpic_N_mcmc <- mcmc.list(out_N_cpic)

save(cpic_N_mcmc, file = "Results/JAGS/cpic_N_mcmc.RData")


####################################################

########## Site N CSER Objects for Model ###########

n_trapsN <- ncol(matrixN) # number of traps
# as.character(EDFA$recap)
N_N <- nrow(EDFN_CSER[which(EDFN_CSER$recap == "N"), ])
K <- max(EDFN_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimN <- c(min(matrixN)-buffer, max(matrixN) + buffer)
n_indN <- length(unique(EDFN_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFN_CSER

EMN_CSER <- EDFN_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMN_CSER

full_dfN <- tidyr::expand(EMN_CSER, ind, day)

EMN_CSER <- left_join(full_dfN, EMN_CSER)
EMN_CSER <- as.data.frame(EMN_CSER, stringsAsFactors = FALSE)
EMN_CSER[is.na(EMN_CSER)] <- 0

##### DATA AUGMENTATION #####
M_N <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_N <- c(rep(1, n_indN), rep(0, M_N-n_indN))
df_augN <- as.data.frame(matrix(0, nrow = (M_N - n_indN), ncol = n_trapsN), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayN_CSER <- array(NA, dim = c(M_N, n_trapsN, K))
for(i in 1:K){
  fooN <- EMN_CSER[(which(EMN_CSER[]$day == i)), ]
  foo_lessN <- select(fooN, -ind, -day)
  if(nrow(foo_lessN) > 0){
    foo_lessN$'2' <- 0
  }
  if(nrow(foo_lessN) > 0){
    foo_lessN$'3' <- 0
  }
  if(nrow(foo_lessN) > 0){
    foo_lessN$'6' <- 0
  }
  if(nrow(foo_lessN) > 0){
    foo_lessN$'7' <- 0
  }
  if(nrow(foo_lessN) > 0){
    foo_lessN$'8' <- 0
  }
  if(nrow(foo_lessN) > 0){
    foo_lessN$'9' <- 0
  }
  if(nrow(foo_lessN) > 0){
    foo_lessN$'10' <- 0
  }
  if(nrow(foo_lessN) == 0){
    foo_lessN <- array(0, dim = c(5, 10))
  }
  colnames(foo_lessN) <- c("1","2","3","4","5","6","7","8","9","10")
  colnames(foo_lessN) <- colnames(df_augN)
  foo_lessN <- as.data.frame(foo_lessN)
  foo_augmentN <- bind_rows(foo_lessN, df_augN)
  EM_arrayN_CSER[1:(M_N), 1:n_trapsN, i] <- as.matrix(foo_augmentN)
}

sum_capsN <- apply(EM_arrayN_CSER, c(1,2), sum)
sstN <- (sum_capsN %*% traplocsN) / (ifelse(rowSums(sum_capsN) > 0, rowSums(sum_capsN), 1))

for(m in (n_indN+1):M_N) {
  sstN[m] <- c(runif(1, xlimN[1], xlimN[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayN_CSER, traplocsA = traplocsN, K=K, M=M_N, xlimA=xlimN, n_traps = n_trapsN)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstN[]), z=z_N, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_N", "sstN", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_N_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_N_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_N_cser))
  })
}) #

stopCluster(cl)

cser_N_mcmc <- mcmc.list(out_N_cser)

save(cser_N_mcmc, file = "Results/JAGS/cser_N_mcmc.RData")


####################################################

########## Site O CPIC Objects for Model ###########

n_trapsO <- ncol(matrixO) # number of traps
# as.character(EDFA$recap)
N_O <- nrow(EDFO[which(EDFO$recap == "N"), ])
K <- max(EDFO$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimO <- c(min(matrixO)-buffer, max(matrixO) + buffer)
n_indO <- length(unique(EDFO$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFO)
EDFO

EMO <- EDFO %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMO)
EMO

full_dfO <- tidyr::expand(EMO, ind, day)

EMO <- left_join(full_dfO, EMO)
EMO <- as.data.frame(EMO, stringsAsFactors = FALSE)
EMO[is.na(EMO)] <- 0

##### DATA AUGMENTATION #####
M_O <- 800 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_O <- c(rep(1, n_indO), rep(0, M_O-n_indO))
df_augO <- as.data.frame(matrix(0, nrow = (M_O - n_indO), ncol = n_trapsO), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayO <- array(NA, dim = c(M_O, n_trapsO, K))
for(i in 1:K){
  fooO <- EMO[(which(EMO[]$day == i)), ]
  foo_lessO <- select(fooO, -ind, -day)
  colnames(foo_lessO) <- colnames(df_augO)
  foo_augmentO <- bind_rows(foo_lessO, df_augO)
  EM_arrayO[1:(M_O), 1:n_trapsO, i] <- as.matrix(foo_augmentO)
}

sum_capsO <- apply(EM_arrayO, c(1,2), sum)
sstO <- (sum_capsO %*% traplocsO) / (ifelse(rowSums(sum_capsO) > 0, rowSums(sum_capsO), 1))

for(m in (n_indO+1):M_O) {
  sstO[m] <- c(runif(1, xlimO[1], xlimO[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayO, traplocsA = traplocsO, K=K, M=M_O, xlimA=xlimO, n_traps = n_trapsO)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstO[]), z=z_O, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_O", "sstO", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_O_cpic <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_O_cpic <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_O_cpic))
  })
}) #

stopCluster(cl)

cpic_O_mcmc <- mcmc.list(out_O_cpic)

save(cpic_O_mcmc, file = "Results/JAGS/cpic_O_mcmc.RData")


####################################################

########## Site O CSER Objects for Model ###########

n_trapsO <- ncol(matrixO) # number of traps
# as.character(EDFA$recap)
N_O <- nrow(EDFO_CSER[which(EDFO_CSER$recap == "N"), ])
K <- max(EDFO_CSER$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimO <- c(min(matrixO)-buffer, max(matrixO) + buffer)
n_indO <- length(unique(EDFO_CSER$ind))

# Make encounter histories with number of times each individual is captured in each trap
EDFO_CSER

EMO_CSER <- EDFO_CSER %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
EMO_CSER

full_dfO <- tidyr::expand(EMO_CSER, ind, day)

EMO_CSER <- left_join(full_dfO, EMO_CSER)
EMO_CSER <- as.data.frame(EMO_CSER, stringsAsFactors = FALSE)
EMO_CSER[is.na(EMO_CSER)] <- 0

##### DATA AUGMENTATION #####
M_O <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

z_O <- c(rep(1, n_indO), rep(0, M_O-n_indO))
df_augO <- as.data.frame(matrix(0, nrow = (M_O - n_indO), ncol = n_trapsO), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_arrayO_CSER <- array(NA, dim = c(M_O, n_trapsO, K))
for(i in 1:K){
  fooO <- EMO_CSER[(which(EMO_CSER[]$day == i)), ]
  foo_lessO <- select(fooO, -ind, -day)
  if(nrow(foo_lessO) > 0){
    foo_lessO$'2' <- 0
  }
  if(nrow(foo_lessO) > 0){
    foo_lessO$'3' <- 0
  }
  if(nrow(foo_lessO) > 0){
    foo_lessO$'4' <- 0
  }
  if(nrow(foo_lessO) > 0){
    foo_lessO$'6' <- 0
  }
  if(nrow(foo_lessO) > 0){
    foo_lessO$'7' <- 0
  }
  if(nrow(foo_lessO) > 0){
    foo_lessO$'8' <- 0
  }
  if(nrow(foo_lessO) == 0){
    foo_lessO <- array(0, dim = c(5, 10))
  }
  colnames(foo_lessO) <- c("1","2","3","4","5","6","7","8","9","10")
  colnames(foo_lessO) <- colnames(df_augO)
  foo_lessO <- as.data.frame(foo_lessO)
  foo_augmentO <- bind_rows(foo_lessO, df_augO)
  EM_arrayO_CSER[1:(M_O), 1:n_trapsO, i] <- as.matrix(foo_augmentO)
}

sum_capsO <- apply(EM_arrayO_CSER, c(1,2), sum)
sstO <- (sum_capsO %*% traplocsO) / (ifelse(rowSums(sum_capsO) > 0, rowSums(sum_capsO), 1))

for(m in (n_indO+1):M_O) {
  sstO[m] <- c(runif(1, xlimO[1], xlimO[2])) #parameters, n, max, min
}


##### Putting Objects into Model ######

jags_data <- list(y = EM_arrayO_CSER, traplocsA = traplocsO, K=K, M=M_O, xlimA=xlimO, n_traps = n_trapsO)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sstO[]), z=z_O, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")

# run in parallel explicitly
cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z_O", "sstO", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out_O_cser <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out_O_cser <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out_O_cser))
  })
}) #

stopCluster(cl)

cser_O_mcmc <- mcmc.list(out_O_cser)

save(cser_O_mcmc, file = "Results/JAGS/cser_O_mcmc.RData")
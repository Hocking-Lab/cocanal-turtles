
#### Spatial Ecology Project ####

#### Libraries ####
library(dplyr)
library(spatstat)
library(spThin)

######## Activity Centers Per Individual and Densities Per Species Per Site #########

####### Site A, CPIC ########
# Load MCMC Data
load("Results/JAGS/cpic_A_mcmc.RData")

# combine chains
df_mcmc_cpic_A <- as.data.frame(cpic_A_mcmc[[1]])
for(i in 2:length(cpic_A_mcmc)) {
  df_mcmc_cpic_A <- bind_rows(df_mcmc_cpic_A, as.data.frame(cpic_A_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_A <- df_mcmc_cpic_A %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cpic_A <- centers_cpic_A[1:36, ]

hist(centers_cpic_A[]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_A <- df_mcmc_cpic_A$density[501:1000] #densities per iteration after burnin
mean_density_cpic_A <- mean(densities_cpic_A) # Mean # turtles per 100 meters

estimated_N_cpic_A <- df_mcmc_cpic_A$N[501:1000]
mean_est_N_cpic_A <- mean(estimated_N_cpic_A) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_A/mean_density_cpic_A

###################################################

############ Site A, CSER ##############
load("Results/JAGS/cser_A_mcmc.RData")

# combine chains
df_mcmc_cser_A <- as.data.frame(cser_A_mcmc[[1]])
for(i in 2:length(cser_A_mcmc)) {
  df_mcmc_cser_A <- bind_rows(df_mcmc_cser_A, as.data.frame(cser_A_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_A <- df_mcmc_cser_A %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cser_A <- centers_cser_A[1:n_indA, ]

hist(centers[1:n_indA, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_A <- df_mcmc_cser_A$density[501:1000] #densities per iteration after burnin
mean_density_cser_A <- mean(densities_cser_A) # Mean # turtles per 100 meters

estimated_N_cser_A <- df_mcmc_cser_A$N[501:1000]
mean_est_N_cser_A <- mean(estimated_N_cser_A) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_A/mean_density_cser_A


####### Site C, CPIC ########

# Load MCMC Data
load("Results/JAGS/cpic_C_mcmc.RData")

# combine chains
df_mcmc_cpic_C <- as.data.frame(cpic_C_mcmc[[1]])
for(i in 2:length(cpic_C_mcmc)) {
  df_mcmc_cpic_C <- bind_rows(df_mcmc_cpic_C, as.data.frame(cpic_C_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_C <- df_mcmc_cpic_C %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_C <- centers_cpic_C[1:n_indC, ]

hist(centers_cpic_C) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_C <- df_mcmc_cpic_C$density[501:1000] #densities per iteration after burnin
mean_density_cpic_C <- mean(densities_cpic_C) # Mean # turtles per 100 meters

estimated_N_cpic_C <- df_mcmc_cpic_C$N[501:1000]
mean_est_N_cpic_C <- mean(estimated_N_cpic_C) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_C/mean_density_cpic_C

####### Site C, CSER ########

# Load MCMC Data
load("Results/JAGS/cser_C_mcmc.RData")

# combine chains
df_mcmc_cser_C <- as.data.frame(cser_C_mcmc[[1]])
for(i in 2:length(cser_C_mcmc)) {
  df_mcmc_cser_C <- bind_rows(df_mcmc_cser_C, as.data.frame(cser_C_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_C <- df_mcmc_cser_C %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_C <- centers_cser_C[1:n_indC, ]

hist(centers[1:n_indC, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_C <- df_mcmc_cser_C$density[501:1000] #densities per iteration after burnin
mean_density_cser_C <- mean(densities_cser_C) # Mean # turtles per 100 meters

estimated_N_cser_C <- df_mcmc_cser_C$N[501:1000]
mean_est_N_cser_C <- mean(estimated_N_cser_C) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_C/mean_density_cser_C


####### Site D, CPIC ########

load("Results/JAGS/cpic_D_mcmc.RData")

# combine chains
df_mcmc_cpic_D <- as.data.frame(cpic_D_mcmc[[1]])
for(i in 2:length(cpic_D_mcmc)) {
  df_mcmc_cpic_D <- bind_rows(df_mcmc_cpic_D, as.data.frame(cpic_D_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_D <- df_mcmc_cpic_D %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_D <- centers_cpic_D[1:n_indD, ]

hist(centers_cpic_D) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_D <- df_mcmc_cpic_D$density[501:1000] #densities per iteration after burnin
mean_density_cpic_D <- mean(densities_cpic_D) # Mean # turtles per 100 meters

estimated_N_cpic_D <- df_mcmc_cpic_D$N[501:1000]
mean_est_N_cpic_D <- mean(estimated_N_cpic_D) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_D/mean_density_cpic_D

####### Site E, CPIC ########

load("Results/JAGS/cpic_E_mcmc.RData")

# combine chains
df_mcmc_cpic_E <- as.data.frame(cpic_E_mcmc[[1]])
for(i in 2:length(cpic_E_mcmc)) {
  df_mcmc_cpic_E <- bind_rows(df_mcmc_cpic_E, as.data.frame(cpic_E_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_E <- df_mcmc_cpic_E %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_E <- centers_cpic_E[1:n_indE, ]

hist(centers_cpic_E) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_E <- df_mcmc_cpic_E$density[501:1000] #densities per iteration after burnin
mean_density_cpic_E <- mean(densities_cpic_E) # Mean # turtles per 100 meters

estimated_N_cpic_E <- df_mcmc_cpic_E$N[501:1000]
mean_est_N_cpic_E <- mean(estimated_N_cpic_E) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_E/mean_density_cpic_E


####### Site E, CSER ########

load("Results/JAGS/cser_E_mcmc.RData")

# combine chains
df_mcmc_cser_E <- as.data.frame(cser_E_mcmc[[1]])
for(i in 2:length(cser_E_mcmc)) {
  df_mcmc_cser_E <- bind_rows(df_mcmc_cser_E, as.data.frame(cser_E_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_E <- df_mcmc_cser_E %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_E <- centers_cser_E[1:n_indE, ]

hist(centers[1:n_indE, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_E <- df_mcmc_cser_E$density[501:1000] #densities per iteration after burnin
mean_density_cser_E <- mean(densities_cser_E) # Mean # turtles per 100 meters

estimated_N_cser_E <- df_mcmc_cser_E$N[501:1000]
mean_est_N_cser_E <- mean(estimated_N_cser_E) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_E/mean_density_cser_E

####### Site F, CSER ########

load("Results/JAGS/cser_F_mcmc.RData")

# combine chains
df_mcmc_cser_F <- as.data.frame(cser_F_mcmc[[1]])
for(i in 2:length(cser_F_mcmc)) {
  df_mcmc_cser_F <- bind_rows(df_mcmc_cser_F, as.data.frame(cser_F_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_F <- df_mcmc_cser_F %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_F <- centers_cser_F[1:n_indF, ]

hist(centers[1:n_indF, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_F <- df_mcmc_cser_F$density[501:1000] #densities per iteration after burnin
mean_density_cser_F <- mean(densities_cser_F) # Mean # turtles per 100 meters

estimated_N_cser_F <- df_mcmc_cser_F$N[501:1000]
mean_est_N_cser_F <- mean(estimated_N_cser_F) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_F/mean_density_cser_F

####### Site F, CPIC ########

load("Results/JAGS/cpic_F_mcmc.RData")

# combine chains
df_mcmc_cpic_F <- as.data.frame(cpic_F_mcmc[[1]])
for(i in 2:length(cpic_F_mcmc)) {
  df_mcmc_cpic_F <- bind_rows(df_mcmc_cpic_F, as.data.frame(cpic_F_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_F <- df_mcmc_cpic_F %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_F <- centers_cpic_F[1:n_indF, ]

hist(centers_cpic_F) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_F <- df_mcmc_cpic_F$density[501:1000] #densities per iteration after burnin
mean_density_cpic_F <- mean(densities_cpic_F) # Mean # turtles per 100 meters

estimated_N_cpic_F <- df_mcmc_cpic_F$N[501:1000]
mean_est_N_cpic_F <- mean(estimated_N_cpic_F) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_F/mean_density_cpic_F


####### Site G, CPIC ########

load("Results/JAGS/cpic_G_mcmc.RData")

# combine chains
df_mcmc_cpic_G <- as.data.frame(cpic_G_mcmc[[1]])
for(i in 2:length(cpic_G_mcmc)) {
  df_mcmc_cpic_G <- bind_rows(df_mcmc_cpic_G, as.data.frame(cpic_G_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_G <- df_mcmc_cpic_G %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_G <- centers_cpic_G[1:n_indG, ]

hist(centers_cpic_G) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_G <- df_mcmc_cpic_G$density[501:1000] #densities per iteration after burnin
mean_density_cpic_G <- mean(densities_cpic_G) # Mean # turtles per 100 meters

estimated_N_cpic_G <- df_mcmc_cpic_G$N[501:1000]
mean_est_N_cpic_G <- mean(estimated_N_cpic_G) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_G/mean_density_cpic_G

####### Site G, CSER ########

load("Results/JAGS/cser_G_mcmc.RData")

# combine chains
df_mcmc_cser_G <- as.data.frame(cser_G_mcmc[[1]])
for(i in 2:length(cser_G_mcmc)) {
  df_mcmc_cser_G <- bind_rows(df_mcmc_cser_G, as.data.frame(cser_G_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_G <- df_mcmc_cser_G %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_G <- centers_cser_G[1:n_indG, ]

hist(centers[1:n_indG, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_G <- df_mcmc_cser_G$density[501:1000] #densities per iteration after burnin
mean_density_cser_G <- mean(densities_cser_G) # Mean # turtles per 100 meters

estimated_N_cser_G <- df_mcmc_cser_G$N[501:1000]
mean_est_N_cser_G <- mean(estimated_N_cser_G) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_G/mean_density_cser_G


####### Site J, CPIC ########

load("Results/JAGS/cpic_J_mcmc.RData")

# combine chains
df_mcmc_cpic_J <- as.data.frame(cpic_J_mcmc[[1]])
for(i in 2:length(cpic_J_mcmc)) {
  df_mcmc_cpic_J <- bind_rows(df_mcmc_cpic_J, as.data.frame(cpic_J_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_J <- df_mcmc_cpic_J %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_J <- centers_cpic_J[1:n_indJ, ]

hist(centers_cpic_J) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_J <- df_mcmc_cpic_J$density[501:1000] #densities per iteration after burnin
mean_density_cpic_J <- mean(densities_cpic_J) # Mean # turtles per 100 meters

estimated_N_cpic_J <- df_mcmc_cpic_J$N[501:1000]
mean_est_N_cpic_J <- mean(estimated_N_cpic_J) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_J/mean_density_cpic_J

####### Site J, CSER ########

load("Results/JAGS/cser_J_mcmc.RData")

# combine chains
df_mcmc_cser_J <- as.data.frame(cser_J_mcmc[[1]])
for(i in 2:length(cser_J_mcmc)) {
  df_mcmc_cser_J <- bind_rows(df_mcmc_cser_J, as.data.frame(cser_J_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_J <- df_mcmc_cser_J %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_J <- centers_cser_J[1:n_indJ, ]

hist(centers[1:n_indJ, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_J <- df_mcmc_cser_J$density[501:1000] #densities per iteration after burnin
mean_density_cser_J <- mean(densities_cser_J) # Mean # turtles per 100 meters

estimated_N_cser_J <- df_mcmc_cser_J$N[501:1000]
mean_est_N_cser_J <- mean(estimated_N_cser_J) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_J/mean_density_cser_J

####### Site K, CPIC ########

load("Results/JAGS/cpic_K_mcmc.RData")

# combine chains
df_mcmc_cpic_K <- as.data.frame(cpic_K_mcmc[[1]])
for(i in 2:length(cpic_K_mcmc)) {
  df_mcmc_cpic_K <- bind_rows(df_mcmc_cpic_K, as.data.frame(cpic_K_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_K <- df_mcmc_cpic_K %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_K <- centers_cpic_K[1:n_indK, ]

hist(centers_cpic_K) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_K <- df_mcmc_cpic_K$density[501:1000] #densities per iteration after burnin
mean_density_cpic_K <- mean(densities_cpic_K) # Mean # turtles per 100 meters

estimated_N_cpic_K <- df_mcmc_cpic_K$N[501:1000]
mean_est_N_cpic_K <- mean(estimated_N_cpic_K) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_K/mean_density_cpic_K

####### Site L, CPIC ########

load("Results/JAGS/cpic_L_mcmc.RData")

# combine chains
df_mcmc_cpic_L <- as.data.frame(cpic_L_mcmc[[1]])
for(i in 2:length(cpic_L_mcmc)) {
  df_mcmc_cpic_L <- bind_rows(df_mcmc_cpic_L, as.data.frame(cpic_L_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_L <- df_mcmc_cpic_L %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_L <- centers_cpic_L[1:n_indL, ]

hist(centers_cpic_L) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_L <- df_mcmc_cpic_L$density[501:1000] #densities per iteration after burnin
mean_density_cpic_L <- mean(densities_cpic_L) # Mean # turtles per 100 meters

estimated_N_cpic_L <- df_mcmc_cpic_L$N[501:1000]
mean_est_N_cpic_L <- mean(estimated_N_cpic_L) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_L/mean_density_cpic_L

####### Site M, CPIC ########

load("Results/JAGS/cpic_M_mcmc.RData")

# combine chains
df_mcmc_cpic_M <- as.data.frame(cpic_M_mcmc[[1]])
for(i in 2:length(cpic_M_mcmc)) {
  df_mcmc_cpic_M <- bind_rows(df_mcmc_cpic_M, as.data.frame(cpic_M_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_M <- df_mcmc_cpic_M %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_M <- centers_cpic_M[1:n_indM, ]

hist(centers_cpic_M) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_M <- df_mcmc_cpic_M$density[501:1000] #densities per iteration after burnin
mean_density_cpic_M <- mean(densities_cpic_M) # Mean # turtles per 100 meters

estimated_N_cpic_M <- df_mcmc_cpic_M$N[501:1000]
mean_est_N_cpic_M <- mean(estimated_N_cpic_M) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_M/mean_density_cpic_M

####### Site M, CSER ########

load("Results/JAGS/cser_M_mcmc.RData")

# combine chains
df_mcmc_cser_M <- as.data.frame(cser_M_mcmc[[1]])
for(i in 2:length(cser_M_mcmc)) {
  df_mcmc_cser_M <- bind_rows(df_mcmc_cser_M, as.data.frame(cser_M_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_M <- df_mcmc_cser_M %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_M <- centers_cser_M[1:n_indM, ]

hist(centers[1:n_indM, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_M <- df_mcmc_cser_M$density[501:1000] #densities per iteration after burnin
mean_density_cser_M <- mean(densities_cser_M) # Mean # turtles per 100 meters

estimated_N_cser_M <- df_mcmc_cser_M$N[501:1000]
mean_est_N_cser_M <- mean(estimated_N_cser_M) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_M/mean_density_cser_M


####### Site N, CPIC ########

load("Results/JAGS/cpic_N_mcmc.RData")

# combine chains
df_mcmc_cpic_N <- as.data.frame(cpic_N_mcmc[[1]])
for(i in 2:length(cpic_N_mcmc)) {
  df_mcmc_cpic_N <- bind_rows(df_mcmc_cpic_N, as.data.frame(cpic_N_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_N <- df_mcmc_cpic_N %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_N <- centers_cpic_N[1:n_indN, ]

hist(centers_cpic_N) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_N <- df_mcmc_cpic_N$density[501:1000] #densities per iteration after burnin
mean_density_cpic_N <- mean(densities_cpic_N) # Mean # turtles per 100 meters

estimated_N_cpic_N <- df_mcmc_cpic_N$N[501:1000]
mean_est_N_cpic_N <- mean(estimated_N_cpic_N) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_N/mean_density_cpic_N

####### Site N, CSER ########

load("Results/JAGS/cser_N_mcmc.RData")

# combine chains
df_mcmc_cser_N <- as.data.frame(cser_N_mcmc[[1]])
for(i in 2:length(cser_N_mcmc)) {
  df_mcmc_cser_N <- bind_rows(df_mcmc_cser_N, as.data.frame(cser_N_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_N <- df_mcmc_cser_N %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_N <- centers_cser_N[1:n_indN, ]

hist(centers[1:n_indN, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_N <- df_mcmc_cser_N$density[501:1000] #densities per iteration after burnin
mean_density_cser_N <- mean(densities_cser_N) # Mean # turtles per 100 meters

estimated_N_cser_N <- df_mcmc_cser_N$N[501:1000]
mean_est_N_cser_N <- mean(estimated_N_cser_N) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_N/mean_density_cser_N

####### Site O, CPIC ########

load("Results/JAGS/cpic_O_mcmc.RData")

# combine chains
df_mcmc_cpic_O <- as.data.frame(cpic_O_mcmc[[1]])
for(i in 2:length(cpic_O_mcmc)) {
  df_mcmc_cpic_O <- bind_rows(df_mcmc_cpic_O, as.data.frame(cpic_O_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cpic_O <- df_mcmc_cpic_O %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()

centers_cpic_O <- centers_cpic_O[1:n_indO, ]

hist(centers_cpic_O) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cpic_O <- df_mcmc_cpic_O$density[501:1000] #densities per iteration after burnin
mean_density_cpic_O <- mean(densities_cpic_O) # Mean # turtles per 100 meters

estimated_N_cpic_O <- df_mcmc_cpic_O$N[501:1000]
mean_est_N_cpic_O <- mean(estimated_N_cpic_O) # Mean # turtles over trap area (+ boundary)
mean_est_N_cpic_O/mean_density_cpic_O

####### Site O, CSER ########

load("Results/JAGS/cser_O_mcmc.RData")

# combine chains
df_mcmc_cser_O <- as.data.frame(cser_O_mcmc[[1]])
for(i in 2:length(cser_O_mcmc)) {
  df_mcmc_cser_O <- bind_rows(df_mcmc_cser_O, as.data.frame(cser_O_mcmc[[i]]))
}

# Obtain Activity Centers
centers_cser_O <- df_mcmc_cser_O %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

centers_cser_O <- centers_cser_O[1:n_indO, ]

hist(centers[1:n_indO, ]) # activity centers for individuals caught at least once
# hist(centers)

#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

densities_cser_O <- df_mcmc_cser_O$density[501:1000] #densities per iteration after burnin
mean_density_cser_O <- mean(densities_cser_O) # Mean # turtles per 100 meters

estimated_N_cser_O <- df_mcmc_cser_O$N[501:1000]
mean_est_N_cser_O <- mean(estimated_N_cser_O) # Mean # turtles over trap area (+ boundary)
mean_est_N_cser_O/mean_density_cser_O

########################### Histograms #########################



##################### Plotting Activity Centers ################

par(mfrow=c(3,2))
plot(xy.coords(x = centers_cpic_A[], y = rep(0, 36)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site A")
plot(centers_cpic_A)
plot(xy.coords(x = centers_cpic_C[], y = rep(0, 30)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site C")
plot(xy.coords(x = centers_cpic_D[], y = rep(0, 22)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site D")
#plot(xy.coords(x = centers_cpic_E[], y = rep(0, 11)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site C")
plot(xy.coords(x = centers_cpic_F[], y = rep(0, 128)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site F")
plot(xy.coords(x = centers_cpic_G[], y = rep(0, 54)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site G")
plot(xy.coords(x = centers_cpic_J[], y = rep(0, 68)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site J")
plot(xy.coords(x = centers_cpic_K[], y = rep(0, 31)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site K")
plot(xy.coords(x = centers_cpic_L[], y = rep(0, 25)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site L")
plot(xy.coords(x = centers_cpic_M[], y = rep(0, 82)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site M")
plot(xy.coords(x = centers_cpic_N[], y = rep(0, 84)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site N")
plot(xy.coords(x = centers_cpic_O[], y = rep(0, 80)), pch = ".", cex = 4, xlab = "Standardized Location in Site", ylab = "", col = "red", main = "CPIC Activity Centers: Site O")

par(mfrow = c(1,1))


########################### FGK TESTS ###################################

#####See methods.lpp and methods.ppx for other methods applicable to lpp objects.

# Calculations on an lpp object: intensity.lpp, distfun.lpp, nndist.lpp, nnwhich.lpp, nncross.lpp, nnfun.lpp.
# 
# Summary functions: linearK, linearKinhom, linearpcf, linearKdot, linearKcross, linearmarkconnect, etc.
# 
# Random point patterns on a linear network can be generated by rpoislpp or runiflpp.
# 
# See linnet for linear networks. 


# linear network

canal_line <- psp(0, 0, 3, 0, window = owin(xrange = c(0,3), yrange = c(-1,1)))
plot(canal_line)
canal_line <- as.linnet(canal_line)
#### Check validity
plot(canal_line)
text(vertices(canal_line), labels = vertexdegree(canal_line))


##### Creating lpp objects - point process along a linear network #####

ACPIC_X <- list(x=c(centers_cpic_A), y = c(rep(0,36)))
ACPIC_X <- as.lpp(ACPIC_X, L = canal_line)

CCPIC_X <- list(x=c(centers_cpic_C), y = c(rep(0,30)))
CCPIC_X <- as.lpp(CCPIC_X, L = canal_line)

DCPIC_X <- list(x=c(centers_cpic_D), y = c(rep(0,22)))
DCPIC_X <- as.lpp(DCPIC_X, L = canal_line)

ECPIC_X <- list(x=c(centers_cpic_E), y = c(rep(0,42)))
ECPIC_X <- as.lpp(ECPIC_X, L = canal_line)

FCPIC_X <- list(x=c(centers_cpic_F), y = c(rep(0,128)))
FCPIC_X <- as.lpp(FCPIC_X, L = canal_line)

GCPIC_X <- list(x=c(centers_cpic_G), y = c(rep(0,54)))
GCPIC_X <- as.lpp(GCPIC_X, L = canal_line)

JCPIC_X <- list(x=c(centers_cpic_J), y = c(rep(0,68)))
JCPIC_X <- as.lpp(JCPIC_X, L = canal_line)

KCPIC_X <- list(x=c(centers_cpic_K), y = c(rep(0,31)))
KCPIC_X <- as.lpp(KCPIC_X, L = canal_line)

LCPIC_X <- list(x=c(centers_cpic_L), y = c(rep(0,25)))
LCPIC_X <- as.lpp(LCPIC_X, L = canal_line)

MCPIC_X <- list(x=c(centers_cpic_M), y = c(rep(0,82)))
MCPIC_X <- as.lpp(MCPIC_X, L = canal_line)

NCPIC_X <- list(x=c(centers_cpic_N), y = c(rep(0,84)))
NCPIC_X <- as.lpp(NCPIC_X, L = canal_line)

OCPIC_X <- list(x=c(centers_cpic_O), y = c(rep(0,80)))
OCPIC_X <- as.lpp(OCPIC_X, L = canal_line)

############ Linear K Monte Carlo Analysis Per Species Per Site ######################


## CPIC, A
linearK_A_CPIC <- linearK(X = ACPIC_X)
summary(linearK_A_CPIC)

linK_A_CPIC <- envelope(ACPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_A_CPIC, main = "Linear K Test on CPIC Site A")

## CPIC, C
linearK_C_CPIC <- linearK(X = CCPIC_X)
summary(linearK_C_CPIC)

linK_C_CPIC <- envelope(CCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_C_CPIC, main = "Linear K Test on CPIC Site C")

## CPIC, D
linearK_D_CPIC <- linearK(X = DCPIC_X)
summary(linearK_D_CPIC)

linK_D_CPIC <- envelope(DCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_D_CPIC, main = "Linear K Test on CPIC Site D")

## CPIC, E
linearK_E_CPIC <- linearK(X = ECPIC_X)
summary(linearK_E_CPIC)

linK_E_CPIC <- envelope(ECPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_E_CPIC, main = "Linear K Test on CPIC Site E")

## CPIC, F
linearK_F_CPIC <- linearK(X = FCPIC_X)
summary(linearK_F_CPIC)

linK_F_CPIC <- envelope(FCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_F_CPIC, main = "Linear K Test on CPIC Site F")

## CPIC, G
linearK_G_CPIC <- linearK(X = GCPIC_X)
summary(linearK_G_CPIC)

linK_G_CPIC <- envelope(GCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_G_CPIC, main = "Linear K Test on CPIC Site G")

## CPIC, J
linearK_J_CPIC <- linearK(X = JCPIC_X)
summary(linearK_J_CPIC)

linK_J_CPIC <- envelope(JCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_J_CPIC, main = "Linear K Test on CPIC Site J")

## CPIC, K
linearK_K_CPIC <- linearK(X = KCPIC_X)
summary(linearK_K_CPIC)

linK_K_CPIC <- envelope(KCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_K_CPIC, main = "Linear K Test on CPIC Site K")

## CPIC, L
linearK_L_CPIC <- linearK(X = LCPIC_X)
summary(linearK_L_CPIC)

linK_L_CPIC <- envelope(LCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_L_CPIC, main = "Linear K Test on CPIC Site L")

## CPIC, M
linearK_M_CPIC <- linearK(X = MCPIC_X)
summary(linearK_M_CPIC)

linK_M_CPIC <- envelope(MCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_M_CPIC, main = "Linear K Test on CPIC Site M")

## CPIC, N
linearK_N_CPIC <- linearK(X = NCPIC_X)
summary(linearK_N_CPIC)

linK_N_CPIC <- envelope(NCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_N_CPIC, main = "Linear K Test on CPIC Site N")

## CPIC, O
linearK_O_CPIC <- linearK(X = OCPIC_X)
summary(linearK_O_CPIC)

linK_O_CPIC <- envelope(OCPIC_X, linearK, nsim = 99, nrank = 1)
plot(linK_O_CPIC, main = "Linear K Test on CPIC Site O")


############  Nearest Neighbor Monte Carlo Analysis #####################

## CPIC, A
# LPCF_A_CPIC <- linearpcf(X = ACPIC_X)
# summary(LPCF_A_CPIC)
# 
# LPCF_A_CPIC <- envelope(ACPIC_X, linearpcf, nsim = 99, nrank = 1)
# plot(LPCF_A_CPIC, main = "Linear Pair Correlation Function on CPIC Site A")














#############   https://rdrr.io/cran/spatstat/man/lpp.html ############

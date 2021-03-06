
#### Spatial Ecology Project ####

#### Libraries ####
library(dplyr)

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

hist(centers[1:36, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cser_A <- centers_cser_A[1:36, ]

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
str(centers) # activity centers

centers_cpic_C <- centers_cpic_C[1:n_indC, ]

hist(centers[1:n_indC, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_D <- centers_cpic_D[1:n_indD, ]

hist(centers[1:n_indD, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_E <- centers_cpic_E[1:n_indE, ]

hist(centers[1:n_indE, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_F <- centers_cpic_F[1:n_indF, ]

hist(centers[1:n_indF, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_G <- centers_cpic_G[1:n_indG, ]

hist(centers[1:n_indG, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_J <- centers_cpic_J[1:n_indJ, ]

hist(centers[1:n_indJ, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_K <- centers_cpic_K[1:n_indK, ]

hist(centers[1:n_indK, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_L <- centers_cpic_L[1:n_indL, ]

hist(centers[1:n_indL, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_M <- centers_cpic_M[1:n_indM, ]

hist(centers[1:n_indM, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_N <- centers_cpic_N[1:n_indN, ]

hist(centers[1:n_indN, ]) # activity centers for individuals caught at least once
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
str(centers) # activity centers

centers_cpic_O <- centers_cpic_O[1:n_indO, ]

hist(centers[1:n_indO, ]) # activity centers for individuals caught at least once
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


######## Checking z and psi
# animals in pop
# z <- df_mcmc %>%
#   select(starts_with("z[")) %>%
#   summarise_all(mean) %>%
#   t()
# str(z)
# 
# psi <- df_mcmc %>%
#   select(starts_with("psi")) %>%
#   summarise_all(mean)





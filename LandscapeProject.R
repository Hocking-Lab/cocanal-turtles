library(dplyr)


#### Spatial Ecology Project ####

#### Libraries ####

#### Creating Ojects for Model ####



#### Creating the Model ####








### make object of p0 type, then figure out how to index to get components you need
#### hard code k and i


#### Obtaining Activity Centers Per individual from MCMC Output ####
load("Results/JAGS/cpic_1_mcmc.RData")

# combine chains
df_mcmc <- as.data.frame(cpic_1_mcmc[[1]])
for(i in 2:length(cpic_1_mcmc)) {
  df_mcmc <- bind_rows(df_mcmc, as.data.frame(cpic_1_mcmc[[i]]))
}

centers <- df_mcmc %>%
  select(starts_with("s[")) %>%
  summarise_all(mean) %>%
  t()
str(centers) # activity centers

hist(centers[1:36, ]) # activity centers for individuals caught at least once
# hist(centers)

# animals in pop
z <- df_mcmc %>%
  select(starts_with("z[")) %>%
  summarise_all(mean) %>%
  t()
str(z)

psi <- df_mcmc %>%
  select(starts_with("psi")) %>%
  summarise_all(mean)















ttt <- data.frame(cpic_1_mcmc[[1]])
ttt <- ttt[ , grep("s", names(ttt))]
names(ttt)
ttt <- ttt[ , -c(1,202,203)]
head(ttt)
ac <- apply(ttt, 2, mean)  # averaged activity centers (estimated per chain), make sure 2 is the correct value (1 vs. 2 avg rows vs. columns)

ac_vec <- as.vector(ac)
str(ac_vec)
caught_ac_vec <- ac_vec[1:n_ind]
str(caught_ac_vec)

trap_list <- EDFA$trap[which(EDFA$recap == "N")]

sex_list2 <- EDFA$sex[which(EDFA$recap == "N")]
cpic1_ac_df <- data.frame(ID = c(1:n_ind), s = caught_ac_vec, sex = sex_list2, trap = trap_list)


#### Obtaining Density For Site 1 and CPICS from MCMC Output ####

ddd1 <- data.frame(out[[1]])
ddd1 <- ddd1[ , "density"]

ddd2 <- data.frame(out[[2]])
ddd2 <- ddd2[ , "density"]

ddd3 <- data.frame(out[[3]])
ddd3 <- ddd3[ , "density"]

cpic1_mean_density <- mean(ddd1) + mean(ddd2) + mean(ddd3) ##? Why add? Need to do for activity centers?


#### 





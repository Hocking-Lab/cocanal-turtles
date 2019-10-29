######### Load Libraries #########
##

library(dplyr)
library(rjags)
library(parallel)

######### Load Data from Previous script #########

load(file = "Data/Derived/all_site.RData")

######### Set data and MCMC Conditions ########

############ TEMP #################
# Fake covariate data for testing

forest_std <- rnorm(12, 0, 2)
depth_std <- rnorm(12, 0, 2)


#############################
# Real covariate data
forest <- read.csv(file = "Data/LandUse/Forest_Cover_SingleColumn.csv", header = FALSE)
depth <- read.csv(file = "Data/LandUse/Avg_Depth_m.csv", header = TRUE)

forest_std <- as.numeric(scale(forest))
depth_std <- as.numeric(scale(depth))

# make M variable by site to speed code
EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE)

#Take out sites H and I
EDF_CPIC <- EDF %>%
  filter(site != "H" & site != "I" & species == "CPIC")
str(EDF_CPIC)

# get number of unique individuals caught per site
inds <- EDF_CPIC %>%
  group_by(site) %>%
  select(site, ind) %>%
  distinct() %>%
  summarise(individuals = n())

# assume capture rate of min_cap_rate (~0.03) to get max individuals to augment (M[g])
min_cap_rate <- 0.03
df_M <- inds %>%
  mutate(M = individuals / min_cap_rate)
df_M

# restrict M to being the maximum size of the data array so the jags loops doesn't go out of bounds
M <- if_else(df_M$M > M, M, trunc(df_M$M))
M <- if_else(df_M$M < max_ind_sp, max_ind_sp, M) # real captures can be up to row 750 at any site, all after are definitely augmentation

# in previous code add max(EM_array[ , , , ], na.rm = TRUE) for each species and save as max_ind_sp (683 for CPIC)

if(testing) {
  M <- rep(200, G)
}



jags_data_site <- list(y = EM_array, 
                       Sex = Sex, 
                       trap_locs = trap_locs, 
                       K=K, 
                       M=M, 
                       xlim=xlim, 
                       max_trap = max_trap, 
                       forest = forest_std,
                       depth = depth_std,
                       C = C, 
                       n_sites = G) #, n_ind = n_ind)
# "initial values for the observed data have to be specified as NA"
inits <- function() {
  list(# alpha0 = rnorm(n_sites, -2, 0.5), 
    # alpha1 = matrix(abs(rnorm(n_sites * 2, 1, 2)), n_sites, 2),
    # alpha2 = matrix(rnorm(n_sites * M, 1, 2), n_sites, M),
    alpha2 = rnorm(1, -1, 1),
    s = t(sst), 
    z = z, 
    # psi = runif(n_sites), 
    psi.sex = runif(n_sites)) #, Sex = c(rep(NA, n_ind))) ## Error = "Invalid parameters for chain 1: non-numeric intial values supplied for variable(s) Sex"   #### ALPHA2????
}

parameters <- c("N", "density", "alpha2", "mu_a0", "sd_a0", "mu_a1", "sd_a1", "alpha0", "alpha1", "alpha3", "beta1", "beta2", "psi.sex", "p_cap_day", "p_cap_sex", "mu_psi", "sd_psi", "sigma_mean", "sigma_site", "sigma_sex") # "sigma", # "C", maybe C or a summary stat, might blow up if saving each activity center "s". 

testing <- TRUE
if(testing) {
  na = 500
  ni = 100
  nt = 1
  nc = 2
} else {
  na = 50000
  ni = 60000
  nt = 6*4
  nc = 4
}


######### Run model ##########

# testing single chain not in parallel
if(FALSE) {
  jm <- jags.model("Code/JAGS/scr_test.txt", jags_data_site, inits = inits, n.adapt = 10, n.chains = 1)
  out <- coda.samples(jm, parameters, n.iter = 100, thin = 1) 
}

if(FALSE) {
  library(jagsUI)
  samples <- jags(data = jags_data_site, inits = inits, parameters.to.save = parameters, model.file = "Code/JAGS/scr_var_M.txt", n.chains = 1, n.adapt = 20, n.burnin = 1, n.iter = 10)
}


cl <- makeCluster(nc)                        # Request # cores
clusterExport(cl, c("jags_data_site", "inits", "parameters", "z", "sst", "Sex", "ni", "na", "nt", "K", "C", "M", "G", "n_sites")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/scr_test.txt", jags_data_site, inits = inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out))
  })
}) #

stopCluster(cl)

samples <- mcmc.list(out)

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS", recursive = TRUE)
saveRDS(samples, "Results/JAGS/all_site_reg_test.rds")

########## Quick checks ###########
plot(samples[ , c("density[1]", "alpha2[1,1]", "alpha0[1]", "beta1", "alpha1[2,1]", "alpha1[2,2]")])
plot(samples[ , c("density[1]", "density[2]", "density[3]", "density[4]", "density[5]", "density[6]", "density[7]", "density[8]", "density[9]", "density[10]", "density[11]", "density[12]")])

plot(samples[ , c("N[2]", "density[2]", "sigma_ind[2]", "alpha2[2,1]", "alpha0[2,1]")])

plot(samples[ , c("sigma[1,1]")])




#---------- crazy slow in JAGS - try NIMBLE to see if get faster mixing ------------

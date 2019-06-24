######### Load Libraries #########
##

library(dplyr)
library(rjags)

######### Load Data from Previous script #########

load(file = "Data/Derived/all_site.RData")

######### Set data and MCMC Conditions ########

############ TEMP #################
# Fake covariate data for testing
forest_std <- rnorm(12, 0, 2)
depth_std <- rnorm(12, 0, 2)
#############################


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
  list(alpha0 = rnorm(n_sites, -2, 0.5), 
       # alpha1 = matrix(abs(rnorm(n_sites * 2, 1, 2)), n_sites, 2),
       alpha2 = matrix(rnorm(n_sites * M, 1, 2), n_sites, M),
       s = t(sst), 
       z = z, 
       psi = runif(n_sites), 
       psi.sex = runif(n_sites)) #, Sex = c(rep(NA, n_ind))) ## Error = "Invalid parameters for chain 1: non-numeric intial values supplied for variable(s) Sex"   #### ALPHA2????
}

parameters <- c("sigma", "N", "density", "sigma_ind", "alpha2", "alpha0", "alpha1", "beta1", "beta2", "alpha1_mean") # "C", maybe C or a summary stat, might blow up if saving each activity center "s". 

testing <- TRUE
if(testing) {
  na = 500
  ni = 100
  nt = 1
  nc = 2
} else {
  na = 100000
  ni = 600000
  nt = 60
  nc = 4
}


######### Run model ##########

# testing single chain not in parallel
if(FALSE) {
  jm <- jags.model("Code/JAGS/scr_all_sites_simple_regression.txt", jags_data_site, inits = inits, n.adapt = 100, n.chains = 1)
  out <- coda.samples(jm, parameters, n.iter = 100, thin = 1) 
}
#


cl <- makeCluster(nc)                        # Request # cores
clusterExport(cl, c("jags_data_site", "inits", "parameters", "n_ind", "z", "sst", "Sex", "ni", "na", "nt", "K", "C", "M", "n_sites")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/scr_all_sites_simple_regression.txt", jags_data_site, inits = inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out))
  })
}) #

stopCluster(cl)

out2 <- mcmc.list(out)

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS", recursive = TRUE)
saveRDS(out2, "Results/JAGS/all_site_reg.rds")

########## Quick checks ###########
plot(out2[ , c("density[1]", "alpha2[1,1]", "alpha0[1]", "beta1", "alpha1[2,1]", "alpha1[2,2]")])
plot(out2[ , c("density[1]", "density[2]", "density[3]", "density[4]", "density[5]", "density[6]", "density[7]", "density[8]", "density[9]", "density[10]", "density[11]", "density[12]")])

plot(out2[ , c("N[2]", "density[2]", "sigma_ind[2]", "alpha2[2,1]", "alpha0[2,1]")])




#---------- crazy slow in JAGS - try NIMBLE to see if get faster mixing ------------

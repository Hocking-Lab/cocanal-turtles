########## Load Libraries ##########
library(coda)
library(rjags)
# library(devtools)
# install_github("https://github.com/stan-dev/bayesplot" # if want latest development version of bayesplot)
library(ggplot2)
library(bayesplot)

######### Load MCMC Object #########

samples <- readRDS("Results/JAGS/all_site_reg.rds")

######### Summary Figures ########

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 80% intervals")
mcmc_areas(samples, 
           regex_pars = c("sigma"), 
           prob = 0.8) + plot_title
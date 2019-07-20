########## Load Libraries ##########
library(coda)
library(rjags)
# library(devtools)
# install_github("https://github.com/stan-dev/bayesplot" # if want latest development version of bayesplot)
library(bayesplot)

######### Load MCMC Object #########

samples <- readRDS("Results/JAGS/all_site_reg.rds")

######### Check MCMC ########

color_scheme_set("mix-blue-pink")
p <- mcmc_trace(samples,  pars = c("N[2]"), regexpr("mu"), # n_warmup = 300,
                facet_args = list(nrow = 2, labeller = label_parsed))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regexpr("sd"), # n_warmup = 300,
                facet_args = list(nrow = 2, labeller = label_parsed))
p + facet_text(size = 15)

p <- mcmc_trace(samples, regexpr("density"))
p + facet_text(size = 15)




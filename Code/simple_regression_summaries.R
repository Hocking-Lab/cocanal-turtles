
model {
  
  mu_a0 ~ dnorm(0, 0.01)
  # sd_a0 ~ dt(0, pow(5, -2), 1)T(0, )
  # sd_a0 ~ T(dnorm(0, 1 / 25), 0, 10000)
  sd_a0 ~ dunif(0, 5)
  
  alpha2 ~ dnorm(0, 0.01)
  
  # mu_a1 ~ dnorm(0, 1 / (5^2))I(0, ) ## half normal
  # sd_a1 ~ dunif(0, 5)
  # mu_a2 ~ dnorm(0, 0.01)
  # sd_a2 ~ dt(0, pow(5, -2), 1)T(0, ) # half cauchy prior with scale = 5 (25?)
  # sd_a2 ~ dunif(0, 5)
  
  beta1 ~ dnorm(0, 0.1)
  beta2 ~ dnorm(0, 0.1)
  
  for(t in 1:2){
    mu_a1[t] ~ dnorm(0, 1 / 9) # fixed intercept differing by sex
  }
  
  for(g in 1:n_sites) {
    
    for(i in 1:M) {
      Sex[g, i] ~ dbern(psi.sex[g])
      Sex2[g, i] <- Sex[g, i] + 1
    }
    
    for(t in 1:2){
      # mu_a1[g, t] ~ dnorm(0, 1 / (5^2))T(0.00001, ) ## half normal independent across sites and sexes
      sigma[g, t] <- pow(1 / (2*alpha1[g, t]), 0.5) # sd of half normal - derived parameter
      
      # log_alpha1 <- beta1
      # alpha1[g, t] <- mu_a1[g, t] + exp(log_alpha1[g, t])
      log(alpha1[g, t]) <- mu_a1[t] + beta1 * depth[g] + beta2 * forest[g] # linear regression on home range hence density? - maybe should put regression on psi instead?
      # mu_a1[g, t] ~ dnorm(mu_a1, 1 / sd_a1 / sd_a1) # random intercept
      
      
    } # t
    
    
    # for(i in 1:M) {
    #    alpha2[g, i] ~ dnorm(mu_a2, 1 / sd_a2 / sd_a2) # Trap behavior universal distribution across sites
    # } # m
    
    psi[g] ~ dunif(0, 1) # prob of individual being in the population (for augmentation since N unknown)
    # logit(psi[g]) ~ dnorm(mu_psi, 1 / sd_psi / sd_psi) # consider drawing from a normal distribution across sites
    # mu_psi ~ dnorm(0, 0.01)
    # sd_psi ~ dunif(0, 10)
    psi.sex[g] ~ dunif(0, 1)
    
    
    alpha0[g] ~ dnorm(mu_a0, sd_a0) 
    
    for(i in 1:M) {
      z[g, i] ~ dbern(psi[g])
      s[g, i] ~ dunif(xlim[g, 1], xlim[g, 2]) ##??
      
      for(j in 1:max_trap[g]) { 
        d[g,i,j] <- abs(s[g, i] - trap_locs[g, j])
        
        for(k in 1:K) {
          for(t in 1:2) {
            logit(p0[g, i, j, k, t]) <- alpha0[g] + (alpha2 * C[i, k, g])  # alpha2*C to rep. global behav. response, (alpha2[g, i] * C[i, k, g]) = individual trap response to recapture
          } # t
        } # k
      } # j
    } # i    
    
    for(i in 1:M) {
      for (j in 1:max_trap[g]) {
        for (k in 1:K) {
          y[i, j, k, g] ~ dbern(p[g, i, j, k])
          p[g, i, j, k] <- z[g, i] * p0[g, i, j, k, Sex2[g, i]] * exp(- alpha1[g, Sex2[g, i]] * d[g, i,j] * d[g, i,j])
        } # i
      } # j
    } # k
    
    # Derived parameters
    # N[g] <- sum(z[g , ])
    # N[g] <- inprod(z[1:M_allsites], sitedummy[ , t]) ## see panel 9.2
    density[g] <- sum(z[g , ]) / (xlim[g, 2] - xlim[g, 1]) # divided distances by 100 so calculates turtles per 100 m of canal
    
  } # g
}


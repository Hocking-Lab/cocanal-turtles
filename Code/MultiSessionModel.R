##### Multi-Session SCR Model Creation JAGS ####

## Also dubbed stratified population model

#### Adding in Variation in N per site ####
library(dplyr)
library(tidyr)
library(rjags)
library(parallel)
library(zoo)
library(rgdal)
library(sp)
library(utils)


coords <- read.csv(file = "Data/coords.csv")
str(coords)
summary(coords)

trap_locs_degrees <- coords
trap_locs_degrees$trap <- 1:nrow(trap_locs_degrees)
trap_num <- trap_locs_degrees$trap


## trap locations per site, manually created (estimated 25m between each trap)
# traplocsA <- c(0,25,50,75,100,125,150,175) # create trap location file
# traplocsC <- c(0,25,50,75,100,125,150,175,200,225)
# traplocsD <- c(0,25,50,75,100,125,150,175)
# traplocsE <- c(0,25,50,75,100,125,150,175,200,225,250,275,300,325)
# traplocsF <- c(0,25,50,75,100,125,150)
# traplocsG <- c(0,25,50,75,100,125,150)
# traplocsJ <- c(0,25,50,75,100,125,150,175,200,225)
# traplocsK <- c(0,25,50,75,100,125,150,175,200,225)
# traplocsL <- c(0,25,50,75,100,125,150,175)
# traplocsM <- c(0,25,50,75,100,125,150,175,200,225,250,275)
# traplocsN <- c(0,25,50,75,100,125,150,175,200,225)
# traplocsO <- c(0,25,50,75,100,125,150,175,200,225)
#thess are in a vertical format

## List of all trap location vectors ##
# traplocs_list <- list(traplocsA, traplocsC, traplocsD, traplocsE, 
#                    traplocsF, traplocsG, traplocsJ, traplocsK, 
#                    traplocsL, traplocsM, traplocsN, traplocsO)


## List of distance matrices (each site = 1 matrix) ##


# convert to utm to have distance in meters
coords_dd = SpatialPoints(coords[ , c("lon", "lat")], proj4string=CRS("+proj=longlat"))
coords_utm <- spTransform(coords_dd, CRS("+init=epsg:26917"))

trap_locs <- coords_utm
trap_locs <- as.data.frame(trap_locs)
trap_locs <- cbind(trap_num, trap_locs)
colnames(trap_locs) = c("trap_id", "easting", "northing")
# trap_locs as single vector with distance between


## Full distance matrix with all sites combined ##
dist_mat <- dist(as.data.frame(coords_utm))
str(dist_mat)
summary(dist_mat)

summary(log(dist_mat))
head(dist_mat)

dist_mat <- as.matrix(dist_mat)
str(dist_mat)

log_dist_mat <- log(dist_mat)

## trap number and row number match... thus row numbers in matrix represent trap numbers --

# Separating distance matrices per site ##
dist_mat_A <- dist(as.data.frame(coords_utm[1:8, ]))
#str(dist_mat_A)
dist_mat_C <- dist(as.data.frame(coords_utm[9:18, ]))
dist_mat_D <- dist(as.data.frame(coords_utm[19:26, ]))
dist_mat_E <- dist(as.data.frame(coords_utm[27:40, ]))
dist_mat_F <- dist(as.data.frame(coords_utm[41:47, ]))
dist_mat_G <- dist(as.data.frame(coords_utm[48:54, ]))
dist_mat_J <- dist(as.data.frame(coords_utm[61:70, ]))
dist_mat_K <- dist(as.data.frame(coords_utm[71:80, ]))
dist_mat_L <- dist(as.data.frame(coords_utm[81:90, ]))
dist_mat_M <- dist(as.data.frame(coords_utm[91:102, ]))
dist_mat_N <- dist(as.data.frame(coords_utm[103:112, ]))
dist_mat_O <- dist(as.data.frame(coords_utm[113:122, ]))

# List of separated distance matrices ##
dist_mat_list <- list(dist_mat_A, dist_mat_C, dist_mat_D,
                      dist_mat_E, dist_mat_F, dist_mat_G,
                      dist_mat_J, dist_mat_K, dist_mat_L,
                      dist_mat_M, dist_mat_N, dist_mat_O)



## Creating trap location vector per site using coordinates (sp package required)
trap_dist_A <- spDistsN1(coords_utm[1:8, ], coords_utm[1, ])
trap_dist_C <- spDistsN1(coords_utm[9:18, ], coords_utm[9, ])
trap_dist_D <- spDistsN1(coords_utm[19:26, ], coords_utm[19, ])
trap_dist_E <- spDistsN1(coords_utm[27:40, ], coords_utm[27, ])
trap_dist_F <- spDistsN1(coords_utm[41:47, ], coords_utm[41, ])
trap_dist_G <- spDistsN1(coords_utm[48:54, ], coords_utm[48, ])
trap_dist_J <- spDistsN1(coords_utm[61:70, ], coords_utm[61, ])
trap_dist_K <- spDistsN1(coords_utm[71:80, ], coords_utm[71, ])
trap_dist_L <- spDistsN1(coords_utm[81:90, ], coords_utm[81, ])
trap_dist_M <- spDistsN1(coords_utm[91:102, ], coords_utm[91, ])
trap_dist_N <- spDistsN1(coords_utm[103:112, ], coords_utm[103, ])
trap_dist_O <- spDistsN1(coords_utm[113:122, ], coords_utm[113, ])

trap_dist_list <- list(trap_dist_A, trap_dist_C, trap_dist_D, trap_dist_E,
                       trap_dist_F, trap_dist_G, trap_dist_J, trap_dist_K,
                       trap_dist_L, trap_dist_M, trap_dist_N, trap_dist_O)



####### EDF FILE ########

EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE)
head(EDF)
summary(EDF)

#Take out sites H and I

EDF_CPIC <- EDF %>%
  filter(site != "H" & site != "I" & species == "CPIC")
EDF_CPIC

EDF_CPIC$trap_id_edited <- ifelse(EDF_CPIC$trap_id >= 61, EDF_CPIC$trap_id - 6, EDF_CPIC$trap_id - 0)

## subtract 6 from trap ids > = 61 (Sites H and I)

#Add a new column for integer session values (session = site)

EDF_CPIC$site_num <- as.integer(as.factor(EDF_CPIC$site))
summary(EDF_CPIC)


## How to label trap numbers in overall EDF file? esp. as some are "skipped" -- caught 0 turtles

##################### 3_7_19 ############################

# # Create a Trap Location Matrix (integers = distance apart in m)
# traplocsA <- c(0,25,50,75,100,125,150,175) # create trap location file
# #this is in a vertical format
# traplocsA
# matrixA <- matrix(NA, ncol = length(traplocsA), nrow = length(traplocsA))
# matrixA
# matrixA[ ,1] <- c(0,25,50,75,100,125,150,175)
# matrixA[ ,2] <- c(25,0,25,50,75,100,125,150)
# matrixA[ ,3] <- c(50,25,0,25,50,75,100,125)
# matrixA[ ,4] <- c(75, 50,25,0,25,50,75,100)
# matrixA[ ,5] <- c(100,75, 50,25,0,25,50,75)
# matrixA[ ,6] <- c(125,100,75, 50,25,0,25,50)
# matrixA[ ,7] <- c(150,125,100,75, 50,25,0,25)
# matrixA[ ,8] <- c(175,150,125,100,75, 50,25,0)
# matrixA  # will need to use coordinates if use all sites in 1 model! or figure out distance b/w sites
# 

####### WORK ON THIS SECTION NEXT 3_11_19 ########

# traplocsA <- traplocsA / 100
# matrixA <- matrixA / 100 # scale for computational purposes
# 
n_traps_site <- read.csv(file = "Data/Max_Traps_Site.csv") # number of traps per site
n_traps <- n_traps_site$max_traps

########


# # as.character(EDFA$recap)
N <- nrow(EDF_CPIC[which(EDF_CPIC$recap == "N"), ])
N <- nrow(EDF_CPIC[which(EDF_CPIC$recap == "N" & EDF_CPIC$trap_id_edited == g), ])

traps_per_site <- read.csv(file = "Data/trapids_sites.csv")


for(g in 1:12){
  N_persite[g] <- list(nrow(EDF_CPIC[which(EDF_CPIC$recap == "N" & EDF_CPIC$site_num == g), ]))
}

K <- max(EDF_CPIC$day) # trap nights per session
# buffer <- 1 # check literature to make sure doesn't need to be larger
# #xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
# xlimA <- c(min(matrixA)-buffer, max(matrixA) + buffer)
# xlimA
##n_ind <- length(unique(EDF_CPIC$ind)) ## Needs to match up with N? 3 off...
## should't use unique as does not count those that were b/w sites; why counting? Codes are different...



# Make encounter histories with number of times each individual is captured in each trap
##### Want EM ARRAY with ijk with index for site ########

str(EDF)
EDF_CPIC

EM_CPIC <- EDF_CPIC %>%
  group_by(ind, site_num, trap_id_edited, day) %>%
  select(ind, site_num, trap_id_edited, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap_id_edited, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EM_CPIC)
EM_CPIC

###### Takes out traps in which no turtles were trapped, not included in the array
###### Need to have them included as rows w/ all 0's
###### HOW?

full_df_CPIC <- tidyr::expand(EM_CPIC, ind, day, site_num)

EM_CPIC <- left_join(full_df_CPIC, EM_CPIC)
EM_CPIC <- as.data.frame(EM_CPIC, stringsAsFactors = FALSE)
EM_CPIC[is.na(EM_CPIC)] <- 0
head(EM_CPIC)


# Data Augmentation
M_allsites <- 8000 # max population size
J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))
z <- c(rep(1, n_ind), rep(0, M_allsites-N))
df_aug <- as.data.frame(matrix(0, nrow = (M_allsites - N), ncol = n_traps), stringsAsFactors = FALSE)
num_sites <- max(EDF_CPIC$site_num)
G <- num_sites

M_persite <- list(200,200,200,300,1000,400,500,200,200,800,800,800)
sum(200,200,200,300,1000,400,500,200,200,800,800,800)

## USE for loop to stack EMs per site? OR have all together
# for (g in 1:num_sites) {
#   EM_array_CPIC[g] <- array(NA, dim = c(M_persite[g], n_traps[g], K))
#   EM_stack <- stack(EM_array_CPIC[g], EM_array_CPIC[g-1])
# }

EM_stack_CPIC <- stack(array(NA, dim = c(M_allsites, n_traps, K)))
#### ERROR -- Vector is too large!!!! WAS IST DAS??????
### Can use code from "TurtleSCR_PerSite.R" and change traps per site to edited trap ids

for(i in 1:K){
  foo <- EM_CPIC[(which(EM_CPIC[]$day == i)), ]
  foo_less <- select(foo, -ind, -day)
  colnames(foo_less) <- colnames(df_aug)
  foo_augment <- bind_rows(foo_less, df_aug)
  EM_array[1:(M), 1:n_traps, i] <- as.matrix(foo_augment)
}



# convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days

## Create list of EM array per site

EM_array_list_CPIC <- array(NA, dim = c(M, n_traps, K))

#EM_array_CPIC <- array(NA, dim = c(M_allsites, n_traps, K, G))
####????
# for(g in 1:G) {
# for(i in 1:K) {
#   foo <- EM_CPIC[which(EM_CPIC[]$day == i & EM_CPIC$site_num == g), ]
#   foo_less <- select(foo, -ind, -day)
#   colnames(foo_less) <- colnames(df_aug)
#   foo_augment <- bind_rows(foo_less, df_aug)
#   EM_array_CPIC[1:(M_allsites), 1:n_traps, i] <- as.matrix(foo_augment)
# } i
# } g

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
  expand(ind, day) %>%
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

Sites <- read.csv(file = "Data/trapids_sites.csv", header = TRUE)

#########

cat ("
     model {

    for(g in 1:length(Sites)) {

     alpha1[g, t] ~ dnorm(mu_a1, 1 / sd_a1 / sd_a1 )
     mu_a1 ~ dnorm(0, 1 / (25^2))I(0, ) ## half normal
     sd_a1 ~ dunif(0, 5)

     for(t in 1:2){
     sigma[g, t] <- pow(1 / (2*alpha1[t]), 0.5) # sd of half normal
     } # t

     psi[g] ~ dunif(0, 1) # giving numbers between 0 and 1, need to change?
     psi.sex[g] ~ dunif(0, 1)
     
     
     sigma_ind[g] ~ dt(0, 1 / (25^2), 1)I(0, ) 	## implies half-cauchy with scale of 25
     
    for(i in 1:M) {
     for(k in 1:K) {
     eta[i,k] ~ dnorm(0, 1 / (sigma_ind * sigma_ind))
     } # i
     } # k
     
     for(t in 1:2) {
     alpha0[g, t] ~ dnorm(0, 0.1)
     } # t
     
     
     for(m in 1:M) {
     alpha2[g, m] ~ dnorm(mu_a2, 1 / sd_a2 / sd_a2) # take out g here? Trap behavior universal b/w sites?
     }
     mu_a2 ~ dnorm(0, 0.01)
     # sd_a2 ~ dt(0, pow(5, -2), 1)T(0, ) # half cauchy prior with scale = 5 (25?)
     sd_a2 ~ dunif(0, 5)
     
     for(i in 1:M) {
     z[g, i] ~ dbern(psi)
     s[g, i] ~ dunif(xlim[g, 1], xlim[g, 2]) ##??
     
     for(j in 1:n_traps) {  # change n_traps to 122? or 116?
     d[i,j] <- abs(s[g, i] - traplocsA[g, j])
     
     for(k in 1:K) {
     for(t in 1:2) {
     logit(p0[g, i, j, k, t]) <- alpha0[t] + (alpha2[i] * C[i, k]) + eta[i, k] # alpha2*C to rep. global behav. response
     } # i
     } # j
     } # k
     } # t
     
     for(i in 1:M) {
     Sex[i] ~ dbern(psi.sex)
     Sex2[i] <- Sex[i] + 1
     for (j in 1:n_traps) {
     for (k in 1:K) {
     y[g, i, j, k] ~ dbern(p[g, i,j,k])
     p[g, i, j, k] <- z[i]*p0[g, i, j, k, Sex2[i]]* exp(- alpha1[Sex2[i]] * d[i,j] * d[i,j])
     } # i
     } # j
     } # k
     
     # Derived parameters
     N[g] <- inprod(z[1:M_allsites], sitedummy[ , t]) ## see panel 9.2
     density[g] <- N[g] / (xlimA[g, 2] - xlimA[g, 1]) # divided distances by 100 so calculates turtles per 100 m of canal
    }
}
     ", file = "Code/JAGS/SCR_Allsites.txt")

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

parameters <- c("sigma", "N", "density", "s", "sigma_ind", "psi", "psi.sex", "C", "alpha2", "alpha", "sigma")

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


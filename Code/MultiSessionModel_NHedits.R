##### Multi-Session SCR Model Creation JAGS ####
##

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
# library(reshape)
# library(plyr)

testing <- FALSE

n_traps <- 14

# number of possible individuals per site
M <- 1000 
if(testing) {
  M <- 700
}

Sites <- read.csv(file = "Data/trapids_sites.csv", header = TRUE)

max_trap_csv <- read.csv(file = "Data/Max_Traps_Site.csv")
max_trap <- max_trap_csv$max_traps

coords <- read.csv(file = "Data/coords.csv")
str(coords)
summary(coords)

trap_locs_degrees <- coords
trap_locs_degrees$trap <- 1:nrow(trap_locs_degrees)
trap_num <- trap_locs_degrees$trap ## Change?? 122 right now


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


# ## Full distance matrix with all sites combined ##
# dist_mat <- dist(as.data.frame(coords_utm))
# str(dist_mat)
# summary(dist_mat)
# 
# summary(log(dist_mat))
# head(dist_mat)
# 
# dist_mat <- as.matrix(dist_mat)
# str(dist_mat)
# 
# log_dist_mat <- log(dist_mat)
# 
# ## trap number and row number match... thus row numbers in matrix represent trap numbers --
# 
# # Separating distance matrices per site ##
# dist_mat_A <- dist(as.data.frame(coords_utm[1:8, ]))
# #str(dist_mat_A)
# dist_mat_C <- dist(as.data.frame(coords_utm[9:18, ]))
# dist_mat_D <- dist(as.data.frame(coords_utm[19:26, ]))
# dist_mat_E <- dist(as.data.frame(coords_utm[27:40, ]))
# dist_mat_F <- dist(as.data.frame(coords_utm[41:47, ]))
# dist_mat_G <- dist(as.data.frame(coords_utm[48:54, ]))
# dist_mat_J <- dist(as.data.frame(coords_utm[61:70, ]))
# dist_mat_K <- dist(as.data.frame(coords_utm[71:80, ]))
# dist_mat_L <- dist(as.data.frame(coords_utm[81:90, ]))
# dist_mat_M <- dist(as.data.frame(coords_utm[91:102, ]))
# dist_mat_N <- dist(as.data.frame(coords_utm[103:112, ]))
# dist_mat_O <- dist(as.data.frame(coords_utm[113:122, ]))
# 
# # List of separated distance matrices ##
# dist_mat_list <- list(dist_mat_A, dist_mat_C, dist_mat_D,
#                       dist_mat_E, dist_mat_F, dist_mat_G,
#                       dist_mat_J, dist_mat_K, dist_mat_L,
#                       dist_mat_M, dist_mat_N, dist_mat_O)
# 
# dist_mat <- dist_mat_list
# 
# for (i in 1:length(dist_mat_list)) {
#   dist_mat[[i]] <- log(dist_mat_list[[i]])
# }



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
# trap_locs <- trap_dist_list

trap_locs <- matrix(NA, 12, max(max_trap))
for (i in 1:12) {
trap_locs[i, 1:max_trap[i]] <- trap_dist_list[[i]] / 100
}



# xlim_pre <- list()
# xlim <- list()

#alter the buffer to better represent home range?

# for(i in 1:12){
#   xlim_pre[[i]] <- c(min(trap_dist_list[[i]] + 100), max(trap_dist_list[[i]] + 100))
#   xlim_pre[[i]] <- log(xlim_pre[[i]])
#   xlim_pre[[i]][1] <- xlim_pre[[i]][1]*(-1)
#   xlim[[i]] <- xlim_pre[[i]]
# }

xlim <- matrix(NA, 12, 2)
for(i in 1:12){
  xlim[i, 1:2] <- c(min(trap_dist_list[[i]]) - 300, max(trap_dist_list[[i]]) + 300) / 100 # need to have buffer on each side without being negative. Just added 50 to the end for testing but will have to think through
}


####### EDF FILE ########

EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
head(EDF)
summary(EDF)

#Take out sites H and I

EDF_CPIC <- EDF %>%
  filter(site != "H" & site != "I" & species == "CPIC")
EDF_CPIC

## subtract 6 from trap ids > = 61 (Sites H and I)

#Add a new column for integer session values (session = site)
#EDF_CPIC$site_num <- as.integer(as.factor(EDF_CPIC$site))

EDF_CPIC$trap_id_edited <- ifelse(EDF_CPIC$trap_id >= 61, EDF_CPIC$trap_id - 6, EDF_CPIC$trap_id - 0)
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

#N <- nrow(EDF_CPIC[which(EDF_CPIC$recap == "N"), ])

N_persite <- list()
N <- list()
for(i in 1:12) {
  N_persite[[i]] <- nrow(EDF_CPIC[which(EDF_CPIC$recap == "N" & EDF_CPIC$site_num == i), ])
  N[[i]] <- N_persite[[i]]
}
n_ind <- N
n_ind_total <- nrow(EDF_CPIC[which(EDF_CPIC$recap == "N"), ])
do.call(sum, N)
K <- max(EDF_CPIC$day) # trap nights per session
# buffer <- 1 # check literature to make sure doesn't need to be larger
# #xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
# xlimA <- c(min(matrixA)-buffer, max(matrixA) + buffer)
# xlimA
##n_ind <- length(unique(EDF_CPIC$ind)) ## Needs to match up with N? 3 off...
## should't use unique as does not count those that were b/w sites; why counting? Codes are different...

traps_per_site <- read.csv(file = "Data/trapids_sites.csv")


# Make encounter histories with number of times each individual is captured in each trap
##### Want EM ARRAY with ijk with index for site ########


########
########
########
########

str(EDF)
EDF_CPIC

EM_CPIC <- EDF_CPIC %>%
  group_by(site_num, ind, trap_id_edited, day) %>%
  select(site_num, ind, trap_id_edited, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  #spread(trap_id_edited, count, fill = 0) %>%
  ungroup() %>%
  mutate(id = as.integer(as.factor(ind)))

EM_CPIC$site_trap <- ave(EM_CPIC$trap_id_edited, EM_CPIC$site_num, FUN = function(x) as.numeric(factor(x)))


# site_id_combos <- expand.grid(site_num = 1:12, id = 1:500) %>%
#   arrange(site_num, id)
site_trap_combos <- expand.grid(site_num = 1:12, site_trap = 1:14) %>%
  arrange(site_num, site_trap) # ... need to add in extra traps per site, need to go through and label trap ids with 14 trap "gap" per site

# foo <- site_id_combos %>%
#    left_join(EM_CPIC)
  
######

#foo$trap_id_edited <- as.integer(foo$trap_id_edited)


foo <- site_trap_combos  %>%
   left_join(EM_CPIC)

foo <- foo %>%
left_join(select(max_trap_csv, -site)) %>%
  # mutate(count = ifelse(site_trap > max_trap, NA_integer_, count)) %>%
  mutate(count = ifelse(site_trap <= .$max_traps & is.na(count), 0, count))

#add in augments

foo_spread <- foo %>%
spread(site_trap, count, fill = 0)

full_df <- tidyr::expand(foo_spread, id, day, site_num)
full_df <- na.omit(full_df)

EM <- left_join(full_df, foo_spread)


EM <- as.data.frame(EM, stringsAsFactors = FALSE)
EM <- na.omit(EM)
EM <- as.data.frame(EM, stringsAsFactors = FALSE)

#### INCLUDES 790 entries... should be 676 (-3: changed dispersers to new individuals) Ns and 114 (+3) Rs (total = 790)

###########

# EM_2 <- select(EM, -trap_id_edited, -day, -site_num, -ind, -max_traps)
# EM_2 <- aggregate(.~id, data = EM_2, sum, na.rm = FALSE)
# 
# extra_info <- as.data.frame(cbind(id = EM$id, site_num = EM$site_num))
# EM_2 <- merge(EM_2, transform(extra_info, EM_2 = extra_info), all.x = TRUE)
# EM_2 <- EM_2 %>% 
#   distinct

############

#n_traps <- 14
J <- n_traps
num_sites <- max(EDF_CPIC$site_num)
G <- num_sites

###########

# EM <- EM %>%
#   select(EM, -c(id, ind, trap_id_edited, max_traps))

#EM_array <- array(NA, dim = c(M, n_traps, K)
# for(i in 1:K){
#   for(g in 1:G){
#     foog <- EM[(which(EM[]$day == i) & (EM[]$site_num == g)), ]
#     foog_less <- select(foog, -ind, -day)
#     colnames(foog_less) <- colnames(df_aug)
#     foo_augment <- bind_rows(foo_less, df_aug)
#     EM_array[1:(M), 1:n_traps, i] <- as.matrix(foo_augment)
#   }
# }

#EM_CPIC_split <- split(EM_CPIC, EM_CPIC$site_num)
#EM_split_array <- as.array(EM_split)
#EM_CPIC_split <- data.frame(select(EM_CPIC_split, -ind, -site_num)) ##????? site_num not found?


## Somehow add 14 - X traps for each site num...
## Add in traps that turtles were not trapped in
###### Takes out traps in which no turtles were trapped, not included in the array
###### Need to have them included as rows w/ all 0's
###### HOW?


#full_df_CPIC <- tidyr::expand(EM_CPIC, ind, day)

# full_df_CPIC <- list()
# 
# for(i in 1:12){
#   full_df_CPIC[[i]] <- tidyr::expand(EM_CPIC_split[[i]], ind, day)
# }
# 
# EM_CPIC <- list()
# for(i in 1:12){
#   EM_CPIC[[i]] <- left_join(full_df_CPIC[[i]], EM_CPIC_split[[i]])
#   EM_CPIC[[i]] <- as.data.frame(EM_CPIC[[i]], stringsAsFactors = FALSE)
#   EM_CPIC[[i]][is.na(EM_CPIC[[i]])] <- 0
# }


# EM_CPIC <- left_join(full_df_CPIC, EM_CPIC_split)
# EM_CPIC <- as.data.frame(EM_CPIC, stringsAsFactors = FALSE)
# EM_CPIC[is.na(EM_CPIC)] <- 0


# Data Augmentation
#M_allsites <- 8000 # max population size
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

#df_aug <- as.data.frame(matrix(0, nrow = (M - n_ind_total), ncol = J), stringsAsFactors = FALSE)

# M_persite <- list(200,200,200,300,1000,400,500,200,200,800,800,800)
# sum(200,200,200,300,1000,400,500,200,200,800,800,800)

## USE for loop to stack EMs per site? OR have all together
# for (g in 1:num_sites) {
#   EM_array_CPIC[g] <- array(NA, dim = c(M_persite[g], n_traps[g], K))
#   EM_stack <- stack(EM_array_CPIC[g], EM_array_CPIC[g-1])
# }

###############
###############
#19_4_19

EM_array <- array(NA, dim = c(M, (max(max_trap) + 1), K, G))

# make 4D array: individual (M) x trap (n_traps) x day (K) x site (G)
# split by day
for(k in 1:K) {
  for(g in 1:G) {
  foo <- EM[(which(EM[]$day == k & EM$site_num == g)), ]
  foo_less <- select(foo, -c(site_num, ind, day, trap_id_edited, max_trap))
  df_aug <- as.data.frame(matrix(0, nrow = (M - nrow(foo_less)), ncol = (max(max_trap) + 1)), stringsAsFactors = FALSE)
  # df_aug$site_num <- g
  # df_aug <- df_aug[ , c(ncol(df_aug), 1:(ncol(df_aug)-1))]
  colnames(df_aug) <- colnames(foo_less)
  foo_augment <- bind_rows(foo_less, df_aug)
  foo_augment$id <- ifelse(foo_augment$id == 0, NA, foo_augment$id)
  EM_array[ , , k, g] <- as.matrix(foo_augment)
  }
}

foob <- EM_array[ , -1, , ]

target <- c(1:M)

  for (k in 1:K) {
    for (g in 1:G) {
    food <- EM_array[ , , k, g]
    foob_arranged <- food[match(target, food[ ,1]), ]
    foob_arranged[is.na(foob_arranged)] <- 0
    foob_arranged[ , 1] <- ifelse(foob_arranged[ , 1] == 0, NA, foob_arranged[ ,1])
    foob_arranged <- foob_arranged[ , -1]
    #foob_arranged <- select()
    #foob_arranged <- food[order(food[ , 1]), ]
    foob[ , , k, g] <- as.matrix(foob_arranged)
    }
  }

EM_array <- foob

## Need to divide M by 4, thus change ids?
#### 500 per day per site? or 500 per site (500/4 per day?), also it does not keep the same id for the same indiiduals between days... so recap calc will not work?

#EM_array_2 <- array(NA, dim = c(M, n_traps + 1, G))

# make 4D array: individual (M) x trap (n_traps) x day (K) x site (G)
# split by day

# for(g in 1:G) {
#     foo <- EM_2[(which(EM_2$site_num == g)), ]
#     foo_less <- select(foo, -c(site_num, EM_2.id, EM_2.site_num))
#     df_aug <- as.data.frame(matrix(0, nrow = (M - nrow(foo_less)), ncol = J + 1), stringsAsFactors = FALSE)
#     # df_aug$site_num <- g
#     # df_aug <- df_aug[ , c(ncol(df_aug), 1:(ncol(df_aug)-1))]
#     colnames(df_aug) <- colnames(foo_less)
#     foo_augment <- bind_rows(foo_less, df_aug)
#     foo_augment$id <- ifelse(foo_augment$id == 0, NA, foo_augment$id)
#     EM_array_2[ , , g] <- as.matrix(foo_augment)
#   }

# foo <- EM_2
# foo_less <- select(foo, -c(site_num, EM_2.id, EM_2.site_num))
# df_aug <- as.data.frame(matrix(0, nrow = (6000 - nrow(foo_less)), ncol = J + 1), stringsAsFactors = FALSE)
# colnames(df_aug) <- colnames(foo_less)
# foo_augment$id <- ifelse(foo_augment$id == 0, NA, foo_augment$id)
# EM_array_2 <- select(foo_augment, -id)


## EM_array_2 --> collapsed day so could make sst vector with all unique individuals caught per site; made sure each individual was only counted (recaps not counted)

# get starting values 1 if indiviudal caught on any day at any trap for each site


z <- matrix(NA, G, M)
bar <- matrix(NA, K, M)
for(g in 1:G) {
  for(k in 1:K) {
    foog <- as.matrix(EM_array[ , , k, g])
    bar[k, ] <- apply(foog, 1, max, na.rm = TRUE)
  }
  z[g, ] <- apply(bar, 2, max, na.rm = TRUE)
}


# z <- c(rep(1, n_ind), rep(0, M_allsites-N))

 # df_aug <- as.data.frame(matrix(0, nrow = (M - n_ind_total), ncol = n_traps + 1), stringsAsFactors = FALSE)

## Create list of EM array per site

# EM_array <- array(NA, dim = c(M, n_traps, K))

# for(i in 1:K){
#   foo2 <- EM[(which(EM[]$day == i)), ]
#   foo_less <- select(foo2, -id, -ind, -trap_id_edited, -max_traps, -day)
#   colnames(foo_less) <- colnames(df_aug)
#   foo_augment <- bind_rows(foo_less, df_aug)
#   EM_array[1:(M), 1:n_traps + 1, i] <- as.matrix(foo_augment)
# }

## getting error due to deleted column names not matching

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

sum_caps <- apply(EM_array, c(1,2,4), sum)  ## c(1,2): dimensions to apply function to; 1 = rows, 2 = columns; collapsed day in this instance
## this is wrong, it doesn't distinguish rows per day as different individuals thus the max number of individuals caught one day becomes the total number of caught inviduals here... Need to sum each individual per for each site


traplocsE <- as.matrix(trap_locs[4, ])
row.names(traplocsE) <- NULL
colnames(traplocsE) <- NULL

unif_array <- array(NA, dim = c(M, G))

for (g in 1:G){
x <- runif(M, min = xlim[g, ], max = xlim[g, ])
unif_array[ , g] <- as.matrix(x)
}


sst <- matrix(NA, M, G)

for(g in 1:G){
    sst[ , g] <- (sum_caps[ , , g] %*% traplocsE) / (ifelse(rowSums(sum_caps[ , , g]) > 0, rowSums(sum_caps[ , , g]), 1))
}

for(i in 1:M){
  for(g in 1:G){
sst[i, g] <- ifelse(sst[i , g] == 0, unif_array[i, g], sst[i, g])
  }
}



##### SEX VECTOR with sex (as a random variable) indicated for caught individuals and NA for augmented individuals #####


# sex_list <- EDF_CPIC$sex
# sex_vector <- ifelse(sex_list == "F", 1, 2)
# Sex <- c(sex_vector-1, rep(NA, M - n_ind_total))



##### Sex vector divided by site

sex_array <- array(NA, dim = c(M, G))

# make 4D array: individual (M) x trap (n_traps) x day (K) x site (G)
# split by day
EM_CPIC_sex <- EDF_CPIC %>%
  group_by(site_num, ind, trap_id_edited, day, sex) %>%
  select(site_num, ind, trap_id_edited, day, sex) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  #spread(trap_id_edited, count, fill = 0) %>%
  ungroup() %>%
  mutate(id = as.integer(as.factor(ind)))

# sex_id <- select(EM_CPIC_sex, sex, id)

# sex_id <- sex_id %>% 
#   distinct




######
EM_array2 <- matrix(NA_integer_, M, G)
target <- c(1:M)

# make 4D array: individual (M) x trap (n_traps) x day (K) x site (G)
# split by day
for(k in 1:K) {
  for(g in 1:G) {
    foo <- EM_CPIC_sex[(which(EM_CPIC$site_num == g)), ]
    foo_less <- select(foo, -c(site_num, ind, day, trap_id_edited, count))
    foo_less <- foo_less %>%
      distinct %>%
      mutate(sex = ifelse(sex == "U", NA, sex),
             sex = ifelse(sex == "M", 0, sex),
             sex = ifelse(sex == "F", 1, sex)) %>%
      mutate(sex = as.integer(sex))
    df_aug <- as.data.frame(matrix(NA_integer_, nrow = (M - nrow(foo_less)), ncol = 2), stringsAsFactors = FALSE)
    # df_aug$site_num <- g
    # df_aug <- df_aug[ , c(ncol(df_aug), 1:(ncol(df_aug)-1))]
    colnames(df_aug) <- colnames(foo_less)
    foo_augment <- bind_rows(foo_less, df_aug)
    target <- as.data.frame(1:M)
    colnames(target) <- "id"
    foo_augment <- left_join(target, foo_augment, by = "id")
    foo_augment <- select(foo_augment, -id)
    #foob_arranged <- foo_augment[match(target, foo_augment[ , 2]), ]
    EM_array2[ , g] <- as.matrix(foo_augment)
  }
}

Sex <- t(EM_array2)

##############
#############
#######
####

#### Behavior Matrix ######

BM <- EDF_CPIC %>%
  group_by(site_num, ind, day, recap) %>%
  select(site_num, ind, day, recap) %>%
  ungroup() %>%
  mutate(id = as.integer(as.factor(ind)))

str(BM)
BM

BM <- as.data.frame(BM, stringsAsFactors = FALSE)
BM$behav <- ifelse(BM$recap == "R", 1, 0)
BM_less <- select(BM, -recap)


# make Cgik with 1 if a recap and 0 otherwise
# C_obs <- BM_less %>%
#   tidyr::expand(site_num, ind, day) %>%
#   left_join(BM_less) %>%
#   group_by(site_num) %>%
#   mutate(behav = ifelse(is.na(behav), 0, behav),
#          cumu = cumsum(behav)) %>%
#   mutate(cgij = ifelse(cumu > 0, 1, 0)) %>% # if you stop it here you can see what it's doing 
#   select(site_num, ind, day, cgij) %>%
#   spread(key = site_num, value = cgij, sep = "_") %>% # stop here if you want to see individual ID before dropping it
#   ungroup %>%
#   select(-ind)
# 
# # augment unobserved individuals
# C_unobs <- as.data.frame(matrix(0, nrow = M-nrow(C_obs), ncol = 4))
# colnames(C_unobs) <- colnames(C_obs)
# C <- bind_rows(C_obs, C_unobs)

## Error: couldn't find arguments (site_num, ind, day)
## Fixed error

B_array <- array(NA, dim = c(M, 2, K, G))

for (i in 1:M){
  for(k in 1:K){
    for(g in 1:G){
      foo <- BM_less[(which(BM_less[]$day == k & BM_less$site_num == g)), ]
      foo_less <- select(foo, -c(site_num, ind, day))
      df_aug <- as.data.frame(matrix(0, nrow = (M - nrow(foo_less)), ncol = 2), stringsAsFactors = FALSE)
      colnames(df_aug) <- colnames(foo_less)
      foo_augment <- bind_rows(foo_less, df_aug)
      foo_augment$id <- ifelse(foo_augment$id == 0, NA, foo_augment$id)
      B_array[ , , k, g] <- as.matrix(foo_augment)
    }
  }
}

foob <- array(NA, dim = c(M, K, G))

target <- c(1:M)

for (k in 1:K) {
  for (g in 1:G) {
    food <- B_array[ , , k, g]
    foob_arranged <- food[match(target, food[ ,1]), ]
    foob_arranged <- foob_arranged[ , -1]
    foob_arranged[is.na(foob_arranged)] <- 0
    #foob_arranged[ , 1] <- ifelse(foob_arranged[ , 1] == 0, NA, foob_arranged[ ,1])
    #foob_arranged <- select()
    #foob_arranged <- food[order(food[ , 1]), ]
    foob[ , k, g] <- as.matrix(foob_arranged)
  }
}

B_array <- foob
C <- B_array

n_sites <- G

########## SAVE ALL OBJECTS NEEDED FOR MODEL ##########

if(!dir.exists("Data/Derived")) dir.create("Data/Derived", recursive = TRUE)

save(z, sst, n_sites, EM_array, Sex, trap_locs, K, M, xlim, max_trap, C, G, file = "Data/Derived/all_site.RData") # other objects needed?

#########

##### separate into separate model scripts
# 
# cat ("
#      model {
# 
#      mu_a1 ~ dnorm(0, 1 / (25^2))I(0, ) ## half normal
#      sd_a1 ~ dunif(0, 5)
#      mu_a2 ~ dnorm(0, 0.01)
#      # sd_a2 ~ dt(0, pow(5, -2), 1)T(0, ) # half cauchy prior with scale = 5 (25?)
#      sd_a2 ~ dunif(0, 5)
# 
#     for(g in 1:n_sites) {
# 
# for(i in 1:M) {
#      Sex[g, i] ~ dbern(psi.sex[g])
#      Sex2[g, i] <- Sex[g, i] + 1
# }
# 
#      for(t in 1:2){
#       alpha1[g, t] ~ dnorm(0, 1 / (25^2))I(0, ) ## half normal independent across sites and sexes
#      # alpha1[g, t] ~ dnorm(mu_a1, 1 / sd_a1 / sd_a1 )I(0, ) # BUGS I(,) notation is only allowed if all parameters are fixed
#       sigma[g, t] <- pow(1 / (2*alpha1[g, t]), 0.5) # sd of half normal
#      } # t
# 
#      psi[g] ~ dunif(0, 1) # prob of individual being in the population (for augmentation since N unknown)
# # logit(psi[g]) ~ dnorm(mu_psi, 1 / sd_psi / sd_psi) # consider drawing from a normal distribution across sites
# # mu_psi ~ dnorm(0, 0.01)
# # sd_psi ~ dunif(0, 10)
#      psi.sex[g] ~ dunif(0, 1)
#      
#      
#      sigma_ind[g] ~ dt(0, 1 / (25^2), 1)I(0, ) 	## implies half-cauchy with scale of 25 - maybe reduce to something more reasonable
#      
#     for(i in 1:M) {
#      for(k in 1:K) {
#      eta[g,i,k] ~ dnorm(0, 1 / (sigma_ind[g] * sigma_ind[g]))
#      } # i
#      } # k
#      
#      for(t in 1:2) {
#      alpha0[g, t] ~ dnorm(0, 0.04) # sd = 5 - could constrain more potentially
#      } # t
#      
#      
#      for(i in 1:M) {
#      alpha2[g, i] ~ dnorm(mu_a2, 1 / sd_a2 / sd_a2) # take out g here? Trap behavior universal b/w sites?
#      } # m
#      
#      for(i in 1:M) {
#      z[g, i] ~ dbern(psi[g])
#      s[g, i] ~ dunif(xlim[g, 1], xlim[g, 2]) ##??
#      
#      for(j in 1:max_trap[g]) { 
#      d[g,i,j] <- abs(s[g, i] - trap_locs[g, j])
#      
#      for(k in 1:K) {
#      for(t in 1:2) {
#      logit(p0[g, i, j, k, t]) <- alpha0[g, t] + (alpha2[g, i] * C[i, k, g]) + eta[g, i, k]  # alpha2*C to rep. global behav. response
#      } # t
#      } # k
#      } # j
#      } # i    
#      
#      for(i in 1:M) {
#      for (j in 1:max_trap[g]) {
#      for (k in 1:K) {
#      y[i, j, k, g] ~ dbern(p[g, i, j, k])
#      p[g, i, j, k] <- z[g, i] * p0[g, i, j, k, Sex2[g, i]] * exp(- alpha1[g, Sex2[g, i]] * d[g, i,j] * d[g, i,j])
#      } # i
#      } # j
#      } # k
#      
#      # Derived parameters
#     N[g] <- sum(z[g , ])
#      # N[g] <- inprod(z[1:M_allsites], sitedummy[ , t]) ## see panel 9.2
#      density[g] <- N[g] / (xlim[g, 2] - xlim[g, 1]) # divided distances by 100 so calculates turtles per 100 m of canal
# 
#     } # g
# }
#      ", file = "Code/JAGS/SCR_Allsites.txt")
# 
# 
# jags_data_site <- list(y = EM_array, 
#                      Sex = Sex, 
#                      trap_locs = trap_locs, 
#                      K=K, 
#                      M=M, 
#                      xlim=xlim, 
#                      max_trap = max_trap, 
#                      C = C, 
#                      n_sites = G) #, n_ind = n_ind)
# # "initial values for the observed data have to be specified as NA"
# inits <- function() {
#   list(alpha0 = matrix(rnorm(n_sites * 2, -2, 0.5), n_sites, 2), 
#        alpha1 = matrix(abs(rnorm(n_sites * 2, 1, 2)), n_sites, 2),
#        alpha2 = matrix(rnorm(n_sites * 2, 1, 2), n_sites, M),
#        s = t(sst), 
#        z = z, 
#        psi = runif(n_sites), 
#        psi.sex = runif(n_sites)) #, Sex = c(rep(NA, n_ind))) ## Error = "Invalid parameters for chain 1: non-numeric intial values supplied for variable(s) Sex"   #### ALPHA2????
# }
# 
# parameters <- c("sigma", "N", "density", "s", "sigma_ind", "alpha2", "alpha0", "alpha1", "sigma") # "C", maybe C or a summary stat
# 
# testing <- TRUE
# if(testing) {
#   na = 500
#   ni = 100
#   nt = 1
#   nc = 2
# } else {
#   na = 100000
#   ni = 600000
#   nt = 60
#   nc = 4
# }
# 
# cl <- makeCluster(nc)                        # Request # cores
# clusterExport(cl, c("jags_data_site", "inits", "parameters", "n_ind", "z", "sst", "Sex", "ni", "na", "nt", "K", "C", "M", "n_sites")) # Make these available
# clusterSetRNGStream(cl = cl, 54354354)
# 
# system.time({ # no status bar (% complete) when run in parallel
#   out <- clusterEvalQ(cl, {
#     library(rjags)
#     jm <- jags.model("Code/JAGS/SCR_Allsites.txt", jags_data_site, inits = inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
#     out <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
#     return(as.mcmc(out))
#   })
# }) #
# 
# 
# 
# stopCluster(cl)
# 
# out2 <- mcmc.list(out)
# plot(out2[ , c("N[1]", "density[1]", "sigma_ind[1]", "alpha2[1,1]", "alpha0[1,1]")])
# plot(out2[ , c("N[2]", "density[2]", "sigma_ind[2]", "alpha2[2,1]", "alpha0[2,1]")])
# 
# 
# ########## Behavioral model but without individual responses #############
# # Complicated model doesn't look like it will converge based on very short test (non-identifiabilies)
# 
# jags_data_site <- list(y = EM_array, 
#                        Sex = Sex, 
#                        trap_locs = trap_locs, 
#                        K=K, 
#                        M=M, 
#                        xlim=xlim, 
#                        max_trap = max_trap, 
#                        C = C, 
#                        n_sites = G) #, n_ind = n_ind)
# # "initial values for the observed data have to be specified as NA"
# inits <- function() {
#   list(alpha0 = rnorm(n_sites, -2, 0.5), 
#        alpha1 = matrix(abs(rnorm(n_sites * 2, 1, 2)), n_sites, 2),
#        alpha2 = matrix(rnorm(n_sites * 2, 1, 2), n_sites, M),
#        s = t(sst), 
#        z = z, 
#        psi = runif(n_sites), 
#        psi.sex = runif(n_sites)) #, Sex = c(rep(NA, n_ind))) ## Error = "Invalid parameters for chain 1: non-numeric intial values supplied for variable(s) Sex"   #### ALPHA2????
# }
# 
# parameters <- c("sigma", "density", "s", "alpha2", "alpha0", "mu0", "sigma0", "mu_a2") # "C", maybe C or a summary stat
# 
# testing <- TRUE
# if(testing) {
#   na = 500
#   ni = 500
#   nt = 1
#   nc = 2
# } else {
#   na = 100000
#   ni = 600000
#   nt = 60
#   nc = 4
# }
# 
# cl <- makeCluster(nc)                        # Request # cores
# clusterExport(cl, c("jags_data_site", "inits", "parameters", "n_ind", "z", "sst", "Sex", "ni", "na", "nt", "K", "C", "M", "n_sites")) # Make these available
# clusterSetRNGStream(cl = cl, 54354354)
# 
# system.time({ # no status bar (% complete) when run in parallel
#   out <- clusterEvalQ(cl, {
#     library(rjags)
#     jm <- jags.model("Code/JAGS/scr_all_sites_simple.txt", jags_data_site, inits = inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
#     out <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
#     return(as.mcmc(out))
#   })
# }) #
# 
# 
# 
# stopCluster(cl)
# 
# scr_simple <- mcmc.list(out)
# plot(scr_simple[ , c("N[1]", "density[1]", "sigma_ind[1]", "alpha2[1,1]", "alpha0[1,1]")])
# plot(scr_simple[ , c("N[2]", "density[2]", "sigma_ind[2]", "alpha2[2,1]", "alpha0[2,1]")])
# 

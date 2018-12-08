
##### SCR Analysis Data Compilation and Model For Each CO-Canal Site ####


library(dplyr)
library(tidyr)
# library(AHMbook)
# library(R2jags)  #rjags could not be loaded?
# library(jagsUI)
# library(R2WinBUGS)

#### Sites: A, C, D, E, F, G, J, K, L, M, N, O

EDF <- read.csv(file = "Data/EDF.csv", stringsAsFactors = FALSE)
head(EDF)

##Add a new column for integer session values (session = site)
EDF$site_num <- as.integer(as.factor(EDF$site))
summary(EDF)

# Create a Trap Location Matrix (integers = distance apart in m); 1 Matrix per Site
traplocsA <- c(0,25,50,75,100,125,150,175) # create trap location file
traplocsC <- c(0,25,50,75,100,125,150,175,200,225)
traplocsD <- c(0,25,50,75,100,125,150,175)
traplocsE <- c(0,25,50,75,100,125,150,175,200,225,250,275,300,325)
traplocsF <- c(0,25,50,75,100,125,150)
traplocsG <- c(0,25,50,75,100,125,150)
traplocsJ <- c(0,25,50,75,100,125,150,175,200,225)
traplocsK <- c(0,25,50,75,100,125,150,175,200,225)
traplocsL <- c(0,25,50,75,100,125,150,175)
traplocsM <- c(0,25,50,75,100,125,150,175,200,225,250,275)
traplocsN <- c(0,25,50,75,100,125,150,175,200,225)
traplocsO <- c(0,25,50,75,100,125,150,175,200,225)
#thess are in a vertical format

#### MatrixA
matrixA <- matrix(NA, ncol = length(traplocsA), nrow = length(traplocsA))

matrixA[ ,1] <- c(0,25,50,75,100,125,150,175)
matrixA[ ,2] <- c(25,0,25,50,75,100,125,150)
matrixA[ ,3] <- c(50,25,0,25,50,75,100,125)
matrixA[ ,4] <- c(75, 50,25,0,25,50,75,100)
matrixA[ ,5] <- c(100,75, 50,25,0,25,50,75)
matrixA[ ,6] <- c(125,100,75, 50,25,0,25,50)
matrixA[ ,7] <- c(150,125,100,75, 50,25,0,25)
matrixA[ ,8] <- c(175,150,125,100,75, 50,25,0)
matrixA  # will need to use coordinates if use all sites in 1 model! or figure out distance b/w sites

#### Matrix C
matrixC <- matrix(NA, ncol = length(traplocsC), nrow = length(traplocsC))
matrixC[ ,1] <- c(0,25,50,75,100,125,150,175,200,225)
matrixC[ ,2] <- c(25,0,25,50,75,100,125,150,175,200)
matrixC[ ,3] <- c(50,25,0,25,50,75,100,125,150,175)
matrixC[ ,4] <- c(75, 50,25,0,25,50,75,100,125,150)
matrixC[ ,5] <- c(100,75,50,25,0,25,50,75,100,125)
matrixC[ ,6] <- c(125,100,75,50,25,0,25,50,75,100)
matrixC[ ,7] <- c(150,125,100,75,50,25,0,25,50,75)
matrixC[ ,8] <- c(175,150,125,100,75,50,25,0,25,50)
matrixC[ , 9] <- c(200,175,150,125,100,75,50,25,0,25)
matrixC[ , 10] <- c(225,200,175,150,125,100,75,50,25,0)

#### Matrix D
matrixD <- matrixA

#### Matrix E
matrixE <- matrix(NA, ncol = length(traplocsE), nrow = length(traplocsE))
matrixE[ ,1] <- c(0,25,50,75,100,125,150,175,200,225,250,275,300,325)
matrixE[ ,2] <- c(25,0,25,50,75,100,125,150,175,200,225,250,275,300)
matrixE[ ,3] <- c(50,25,0,25,50,75,100,125,150,175,200,225,250,275)
matrixE[ ,4] <- c(75, 50,25,0,25,50,75,100,125,150,175,200,225,250)
matrixE[ ,5] <- c(100,75,50,25,0,25,50,75,100,125,150,175,200,225)
matrixE[ ,6] <- c(125,100,75,50,25,0,25,50,75,100,125,150,175,200)
matrixE[ ,7] <- c(150,125,100,75,50,25,0,25,50,75,100,125,150,175)
matrixE[ ,8] <- c(175,150,125,100,75,50,25,0,25,50,75,100,125,150)
matrixE[ , 9] <- c(200,175,150,125,100,75,50,25,0,25,50,75,100,125)
matrixE[ , 10] <- c(225,200,175,150,125,100,75,50,25,0,25,50,75,100)
matrixE[ , 11] <- c(250,225,200,175,150,125,100,75,50,25,0,25,50,75)
matrixE[ , 12] <- c(275,250,225,200,175,150,125,100,75,50,25,0,25,50)
matrixE[ , 13] <- c(300,275,250,225,200,175,150,125,100,75,50,25,0,25)
matrixE[ , 14] <- c(325,300,275,250,225,200,175,150,125,100,75,50,25,0)


#### Matrix F
matrixF <- matrix(NA, ncol = length(traplocsF), nrow = length(traplocsF))
matrixF[ ,1] <- c(0,25,50,75,100,125,150)
matrixF[ ,2] <- c(25,0,25,50,75,100,125)
matrixF[ ,3] <- c(50,25,0,25,50,75,100)
matrixF[ ,4] <- c(75, 50,25,0,25,50,75)
matrixF[ ,5] <- c(100,75, 50,25,0,25,50)
matrixF[ ,6] <- c(125,100,75, 50,25,0,25)
matrixF[ ,7] <- c(150,125,100,75, 50,25,0)

#### Matrix G
matrixG <- matrixF

#### Matrix J
matrixJ <- matrixC

#### Matrix K
matrixK <- matrixC

#### Matrix L
matrixL <- matrixA

#### Matrix M
matrixM <- matrix(NA, ncol = length(traplocsM), nrow = length(traplocsM))

matrixM[ ,1] <- c(0,25,50,75,100,125,150,175,200,225,250,275)
matrixM[ ,2] <- c(25,0,25,50,75,100,125,150,175,200,225,250)
matrixM[ ,3] <- c(50,25,0,25,50,75,100,125,150,175,200,225)
matrixM[ ,4] <- c(75, 50,25,0,25,50,75,100,125,150,175,200)
matrixM[ ,5] <- c(100,75,50,25,0,25,50,75,100,125,150,175)
matrixM[ ,6] <- c(125,100,75,50,25,0,25,50,75,100,125,150)
matrixM[ ,7] <- c(150,125,100,75,50,25,0,25,50,75,100,125)
matrixM[ ,8] <- c(175,150,125,100,75,50,25,0,25,50,75,100)
matrixM[ , 9] <- c(200,175,150,125,100,75,50,25,0,25,50,75)
matrixM[ , 10] <- c(225,200,175,150,125,100,75,50,25,0,25,50)
matrixM[ , 11] <- c(250,225,200,175,150,125,100,75,50,25,0,25)
matrixM[ , 12] <- c(275,250,225,200,175,150,125,100,75,50,25,0)

#### Matrix N
matrixN <- matrixC

#### Matrix O
matrixO <- matrixC

# Trap location and distance matrices / 100
# scale for computational purposes
traplocsA <- traplocsA / 100
traplocsC <- traplocsC / 100
traplocsD <- traplocsD / 100
traplocsE <- traplocsE / 100
traplocsF <- traplocsF / 100
traplocsG <- traplocsG / 100
traplocsJ <- traplocsJ / 100
traplocsK <- traplocsK / 100
traplocsL <- traplocsL / 100
traplocsM <- traplocsM / 100
traplocsN <- traplocsN / 100
traplocsO <- traplocsO / 100
matrixA <- matrixA / 100
matrixC <- matrixC / 100
matrixD <- matrixD / 100
matrixE <- matrixE / 100
matrixF <- matrixF / 100
matrixG <- matrixG / 100
matrixJ <- matrixJ / 100
matrixK <- matrixK / 100
matrixL <- matrixL / 100
matrixM <- matrixM / 100

## Subset Data for Sites and Species (Density of all turtles per site)

# Site A, CPIC
EDFA <- EDF %>%
  filter(site_num == 1 & species == "CPIC")
EDFA

# Site C, CPIC
EDFC <- EDF %>%
  filter(site_num == 2 & species == "CPIC")
EDFC

# Site D, CPIC
EDFD <- EDF %>%
  filter(site_num == 3 & species == "CPIC")
EDFD

# Site E, CPIC
EDFE <- EDF %>%
  filter(site_num == 4 & species == "CPIC")
EDFE

# Site F, CPIC
EDFF <- EDF %>%
  filter(site_num == 5 & species == "CPIC")
EDFF

# Site G, CPIC
EDFG <- EDF %>%
  filter(site_num == 6 & species == "CPIC")
EDFG

# Site J, CPIC
EDFJ <- EDF %>%
  filter(site_num == 9 & species == "CPIC")
EDFJ

# Site K, CPIC
EDFK <- EDF %>%
  filter(site_num == 10 & species == "CPIC")
EDFK

# Site L, CPIC
EDFL <- EDF %>%
  filter(site_num == 11 & species == "CPIC")
EDFL

# Site M, CPIC
EDFM <- EDF %>%
  filter(site_num == 12 & species == "CPIC")
EDFM

# Site N, CPIC
EDFN <- EDF %>%
  filter(site_num == 13 & species == "CPIC")
EDFN

# Site O, CPIC
EDFO <- EDF %>%
  filter(site_num == 14 & species == "CPIC")
EDFO




########## Site A CPIC Objects for Model ########
n_trapsA <- ncol(matrixA) # number of traps
# as.character(EDFA$recap)
N_A <- nrow(EDFA[which(EDFA$recap == "N"), ])
K <- max(EDFA$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimA <- c(min(matrixA)-buffer, max(matrixA) + buffer)
n_indA <- length(unique(EDFA$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFA)
EDFA

EMA <- EDFA %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EM)
EMA

full_dfA <- tidyr::expand(EMA, ind, day)

EMA <- left_join(full_dfA, EMA)
EMA <- as.data.frame(EMA, stringsAsFactors = FALSE)
EMA[is.na(EMA)] <- 0

####################################################

########## Site C CPIC Objects for Model ########
n_trapsC <- ncol(matrixC) # number of traps
# as.character(EDFA$recap)
N_C <- nrow(EDFC[which(EDFC$recap == "N"), ])
K <- max(EDFC$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimC <- c(min(matrixC)-buffer, max(matrixC) + buffer)
n_indC <- length(unique(EDFC$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFC)
EDFC

EMC <- EDFC %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMC)
EMC

full_dfC <- tidyr::expand(EMC, ind, day)

EMC <- left_join(full_dfC, EMC)
EMC <- as.data.frame(EMC, stringsAsFactors = FALSE)
EMC[is.na(EMC)] <- 0

########################################################

########## Site D CPIC Objects for Model ###############

n_trapsD <- ncol(matrixD) # number of traps
# as.character(EDFA$recap)
N_D<- nrow(EDFD[which(EDFD$recap == "N"), ])
K <- max(EDFD$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimD <- c(min(matrixD)-buffer, max(matrixD) + buffer)
n_indD <- length(unique(EDFD$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFD)
EDFD

EMD <- EDFD %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMD)
EMD

full_dfD <- tidyr::expand(EMD, ind, day)

EMD <- left_join(full_dfD, EM)
EMD <- as.data.frame(EMD, stringsAsFactors = FALSE)
EMD[is.na(EMD)] <- 0

#########################################################


########## Site E CPIC Objects for Model ################

n_trapsE <- ncol(matrixE) # number of traps
# as.character(EDFA$recap)
N_E <- nrow(EDFE[which(EDFE$recap == "N"), ])
K <- max(EDFE$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimE <- c(min(matrixE)-buffer, max(matrixE) + buffer)
n_indE <- length(unique(EDFE$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFE)
EDFE

EME <- EDFE %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EME)
EME

full_dfE <- tidyr::expand(EME, ind, day)

EME <- left_join(full_dfE, EME)
EME <- as.data.frame(EME, stringsAsFactors = FALSE)
EME[is.na(EME)] <- 0

###################################################

########## Site F CPIC Objects for Model ################

n_trapsF <- ncol(matrixF) # number of traps
# as.character(EDFA$recap)
N_F <- nrow(EDFF[which(EDFF$recap == "N"), ])
K <- max(EDFF$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimF <- c(min(matrixF)-buffer, max(matrixF) + buffer)
n_indF <- length(unique(EDFF$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFF)
EDFF

EMF <- EDFF %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMF)
EMF

full_dfF <- tidyr::expand(EMF, ind, day)

EMF <- left_join(full_dfF, EMF)
EMF <- as.data.frame(EMF, stringsAsFactors = FALSE)
EMF[is.na(EMF)] <- 0

#######################################################

########## Site G CPIC Objects for Model ##############

n_trapsG <- ncol(matrixG) # number of traps
# as.character(EDFA$recap)
N_G <- nrow(EDFG[which(EDFG$recap == "N"), ])
K <- max(EDFG$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimG <- c(min(matrixG)-buffer, max(matrixG) + buffer)
n_indG <- length(unique(EDFG$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFG)
EDFG

EMG <- EDFG %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMG)
EMG

full_dfG <- tidyr::expand(EMG, ind, day)

EMG <- left_join(full_dfG, EMG)
EMG <- as.data.frame(EMG, stringsAsFactors = FALSE)
EMG[is.na(EMG)] <- 0

###################################################

########## Site J CPIC Objects for Model ###########

n_trapsJ <- ncol(matrixJ) # number of traps
# as.character(EDFA$recap)
N_J <- nrow(EDFJ[which(EDFJ$recap == "N"), ])
K <- max(EDFJ$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimJ <- c(min(matrixJ)-buffer, max(matrixJ) + buffer)
n_indJ <- length(unique(EDFJ$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFJ)
EDFJ

EMJ <- EDFJ %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMJ)
EMJ

full_dfJ <- tidyr::expand(EMJ, ind, day)

EMJ <- left_join(full_dfJ, EMJ)
EMJ <- as.data.frame(EMJ, stringsAsFactors = FALSE)
EMJ[is.na(EMJ)] <- 0

####################################################

########## Site K CPIC Objects for Model ###########

n_trapsK <- ncol(matrixK) # number of traps
# as.character(EDFA$recap)
N_K <- nrow(EDFK[which(EDFK$recap == "N"), ])
K <- max(EDFK$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimK <- c(min(matrixK)-buffer, max(matrixK) + buffer)
n_indK <- length(unique(EDFK$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFK)
EDFK

EMK <- EDFK %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMK)
EMK

full_dfK <- tidyr::expand(EMK, ind, day)

EMK <- left_join(full_dfK, EMK)
EMK <- as.data.frame(EMK, stringsAsFactors = FALSE)
EMK[is.na(EMK)] <- 0

####################################################

########## Site L CPIC Objects for Model ###########

n_trapsL <- ncol(matrixL) # number of traps
# as.character(EDFA$recap)
N_L <- nrow(EDFL[which(EDFL$recap == "N"), ])
K <- max(EDFL$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimL <- c(min(matrixL)-buffer, max(matrixL) + buffer)
n_indL <- length(unique(EDFL$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFL)
EDFL

EML <- EDFL %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EML)
EML

full_dfL <- tidyr::expand(EML, ind, day)

EML <- left_join(full_dfL, EML)
EML <- as.data.frame(EML, stringsAsFactors = FALSE)
EML[is.na(EML)] <- 0

####################################################

########## Site M CPIC Objects for Model ###########

n_trapsM <- ncol(matrixM) # number of traps
# as.character(EDFA$recap)
N_M <- nrow(EDFM[which(EDFM$recap == "N"), ])
K <- max(EDFM$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimM <- c(min(matrixM)-buffer, max(matrixM) + buffer)
n_indM <- length(unique(EDFM$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFM)
EDFM

EMM <- EDFM %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMM)
EMM

full_dfM <- tidyr::expand(EMM, ind, day)

EMM <- left_join(full_dfM, EMM)
EMM <- as.data.frame(EMM, stringsAsFactors = FALSE)
EMM[is.na(EMM)] <- 0

####################################################

########## Site N CPIC Objects for Model ###########

n_trapsN <- ncol(matrixN) # number of traps
# as.character(EDFA$recap)
N_N <- nrow(EDFN[which(EDFN$recap == "N"), ])
K <- max(EDFN$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimN <- c(min(matrixN)-buffer, max(matrixN) + buffer)
n_indN <- length(unique(EDFN$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFN)
EDFN

EMN <- EDFN %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMN)
EMN

full_dfN <- tidyr::expand(EMN, ind, day)

EMN <- left_join(full_dfN, EMN)
EMN <- as.data.frame(EMN, stringsAsFactors = FALSE)
EMN[is.na(EMN)] <- 0

####################################################

########## Site O CPIC Objects for Model ###########

n_trapsO <- ncol(matrixO) # number of traps
# as.character(EDFA$recap)
N_O <- nrow(EDFO[which(EDFO$recap == "N"), ])
K <- max(EDFO$day) # trap nights per session
buffer <- 1 # check literature to make sure doesn't need to be larger
#xlimA <- c(min(traplocsA[1,] - buffer), max(traplocs[1,] + buffer))
xlimO <- c(min(matrixO)-buffer, max(matrixO) + buffer)
n_indO <- length(unique(EDFO$ind))

# Make encounter histories with number of times each individual is captured in each trap
str(EDFO)
EDFO

EMO <- EDFO %>%
  group_by(ind, trap, day) %>%
  select(ind, trap, day) %>%
  mutate(count = 1) %>%
  summarise_all(sum) %>%
  spread(trap, count, fill = 0) %>%
  ungroup()
# EM <- data.frame(select(EM, -ind))
str(EMO)
EMO

full_dfO <- tidyr::expand(EMO, ind, day)

EMO <- left_join(full_dfO, EMO)
EMO <- as.data.frame(EMO, stringsAsFactors = FALSE)
EMO[is.na(EMO)] <- 0

####################################################

##### DATA AUGMENTATION #####
M <- 200 # max population size, change with site
# J <- n_traps
# y <- rbind(EM[ , 2], matrix(0, nrow = M-n_ind, ncol = n_ind))
# y <- rbind(EM, matrix(0, nrow = M - n_ind, ncol = n_traps))

n_ind <- n_indA
n_traps <- n_trapsA

z <- c(rep(1, n_ind), rep(0, M-n_ind))
df_aug <- as.data.frame(matrix(0, nrow = (M - n_ind), ncol = n_traps), stringsAsFactors = FALSE)

# Convert to 3D array (n_individuals + augmented individuals) x n_traps x n_days
EM_array <- array(NA, dim = c(M, n_traps, K))
for(i in 1:K){
  foo <- EM[(which(EM[]$day == i)), ]
  foo_less <- select(foo, -ind, -day)
  colnames(foo_less) <- colnames(df_aug)
  foo_augment <- bind_rows(foo_less, df_aug)
  EM_array[1:(M), 1:n_traps, i] <- as.matrix(foo_augment)
}

##### SEX VECTOR with sex (as a random variable) indicated for caught individuals and NA for augmented individuals #####

#sex_list <- EDFA$sex
#sex_vector <- ifelse(sex_list == "F", 1, 2)
#Sex <- c(sex_vector-1, rep(NA, length = M-n_ind))


# EM <- cbind(1:n_ind, rowSums(EM))
# EM <- rowSums(EM)
# Read in trap hour file
#traphoursA <- read.csv(file = "Data/traphoursA.csv", stringsAsFactors = FALSE)
#traphoursA
# hours <- apply(traphoursA[ , 2:ncol(traphoursA)], 1, sum)
# hours

################# Function to Create Objects Needed Per SCR Model ###################

create_site_objects <- function(traplocs, n_ind, n_traps, M, EM) {
  X <- traplocs
  z <- c(rep(1, n_ind), rep(0, M-n_ind))
  df_aug <- as.data.frame(matrix(0, nrow = (M - n_ind), ncol = n_traps), stringsAsFactors = FALSE)
  EM_array <- array(NA, dim = c(M, n_traps, K))
  K <- 4
  for(i in 1:K){
    foo <- EM[(which(EM[]$day == i)), ]    # Error in EM[(which(EM[]$day == i)), ] : incorrect number of dimensions
    foo_less <- select(foo, -ind, -day)
    colnames(foo_less) <- colnames(df_aug)
    foo_augment <- bind_rows(foo_less, df_aug)
    EM_array[1:(M), 1:n_traps, i] <- as.matrix(foo_augment)
  }
  
  sum_caps <- apply(EM_array, c(1,2), sum)
  sst <- (sum_caps %*% traplocs) / (ifelse(rowSums(sum_caps) > 0, rowSums(sum_caps), 1))
  
  for(m in (n_ind+1):M) {
    sst[m] <- c(runif(1, xlimA[1], xlimA[2])) #parameters, n, max, min
  }
}

###########################################################################################


###### For Loop to Go Through Each Site and Each Species with SCR Model ######

Sites <- c(1:12)
M <- c(200, 200, 200, 300, 1000, 400, 500, 200, 200, 800, 800, 800)
traplocs <- rep(list(traplocsA, traplocsC, traplocsD, traplocsE, traplocsF, traplocsG, traplocsJ, traplocsK, traplocsL, traplocsM, traplocsN, traplocsO), length(Sites))
n_traps <- c(n_trapsA, n_trapsC, n_trapsD, n_trapsE, n_trapsF, n_trapsG, n_trapsJ, n_trapsK, n_trapsL, n_trapsM, n_trapsN, n_trapsO)
n_ind <- c(n_indA, n_indC, n_indD, n_indE, n_indF, n_indG, n_indJ, n_indK, n_indL, n_indM, n_indN, n_indO)
EM <- rep(list(EMA, EMC, EMD, EME, EMF, EMG, EMJ, EMK, EML, EMM, EMN, EMO), length(Sites))

site_objects <- rep(list(NA), length(Sites))


for(s in 1:length(Sites)){
  site_objects[s] <- create_site_objects(traplocs = traplocs[s], n_ind = n_ind[s], n_traps = n_traps[s], M = M[s], EM = EM[s])
}


# for(s in 1:length(Sites)){
#   X[s] <- traplocs[s]
#   z[s] <- c(rep(1, n_ind[s]), rep(0, M[s]-n_ind[s]))
#   df_aug[s] <- as.data.frame(matrix(0, nrow = (M[s] - n_ind[s]), ncol = n_traps[s]), stringsAsFactors = FALSE)
#   EM_array[s] <- array(NA, dim = c(M[s], n_traps[s], K))
#   for(i in 1:K){
#     foo[s] <- EM[s][(which(EM[s][]$day == i)), ]
#     foo_less[s] <- select(foo[s], -ind, -day)
#     colnames(foo_less[s]) <- colnames(df_aug[s])
#     foo_augment[s] <- bind_rows(foo_less[s], df_aug[s])
#     EM_array[s][1:(M[s]), 1:n_traps[s], i] <- as.matrix(foo_augment[s])
#   }
#   
#   
#   sum_caps[s] <- apply(EM_array[s], c(1,2), sum)
#   sst[s] <- (sum_caps[s] %*% traplocs[s]) / (ifelse(rowSums(sum_caps[s]) > 0, rowSums(sum_caps[s]), 1))
#   
#   for(m in (n_ind+1):M[i]) {
#     sst[s, m] <- c(runif(1, xlimA[1], xlimA[2])) #parameters, n, max, min
#   }
#   sst
#   
# }

#Start values for s (activity centers) of augments (from random uniform constrained by state space size)
# X <- traplocsA
# sst # Now populated by starting positions uniformally placed within state space
# For every individual that is not 0, change starting x point to mean of traps associated with encounters for that individual; leaves 0's there from the augmented population and also puts in activity center for augmented individuals that were randomly given an encounter history (caught at least 1 time)

# sum_caps <- apply(EM_array, c(1,2), sum)
# sst <- (sum_caps %*% traplocsA) / (ifelse(rowSums(sum_caps) > 0, rowSums(sum_caps), 1))
# 
# for(i in (n_ind+1):M) {
#   sst[i] <- c(runif(1, xlimA[1], xlimA[2])) #parameters, n, max, min
# }
# sst


# Create model function in BUGS code named X
# Example: SimpleModelA <- X(EM, traps, nb = 2000, ni = 12000, buffer = 1, M = 300)
# buffer = determines size of state space model in scaled units
# Use jags.model function to intitialize model and coda.samples to obtain posterior samples for analysis
# Example: InitialA <- jags.model("X", data = data, inits = inits, n.chains = 3, n.adapt = 1000)
#PostA <- coda.samples(InitialA, parameters, n.iter = 1000, thin = 1)

#### BUS Model Specification ####

# alpha0 = set prob. of baseline encounter (model will alter?)
# logit = constraining prob b/w 0 and 1
# alpha1 = set prob. of encounter at 1 unit distance away from alpha0
# sigma = scale parameter of a half-normal
# why ~ ?
# psi ?
# for each individual for number of augments (M), find z[i] - likelihood that augment is part of sampled population, activity center (acs assumed to be uniformally distributed) , and 
# for each trap
 # calculate distance of each individual i from trap j using distance equation
  # estimate parameters affecting the observed number of times caught per trap reflecting a      # binomial distribution with probability of encounter and trapping effort (might need to change this to fraction of a day instead of in hours?)
  # estimate encounter probability parameters including likelihood is part of population...,     # baseline encounter prob., and the distance coefficient (prob decay rate)
# Calculate population size by summing all 1s associated with individuals within the population that were sampled (prob. of z = 1)
# Calculate density by dividing pop. size by state space size (trapping length + buffers)

if(!dir.exists("Code/JAGS")) dir.create("Code/JAGS", recursive = TRUE)

d <- seq(xlimA[1], xlimA[2], length.out = 100)

cat ("
  model {
    # alpha1 ~ dgamma(0.1, 0.1) # consider appropriate prior
    alpha1 ~ dt(0, 1 / (5^2), 1)I(0, ) 	## implies half-cauchy with scale of 5
    sigma <- pow(1 / (2*alpha1), 0.5) # sd of half normal
    psi ~ dunif(0, 1)
    for(k in 1:K) {
      alpha0[k] ~ dnorm(0, 0.1)
      logit(p0[k]) <- alpha0[k]
    }
    for(i in 1:M) {
      z[i] ~ dbern(psi)
      s[i] ~ dunif(xlimA[1], xlimA[2])
      for(j in 1:n_traps) {
        d[i,j] <- abs(s[i] - traplocsA[j])
        for(k in 1:K) {
          y[i,j, k] ~ dbern(p[i,j, k])
          p[i,j, k] <- z[i]*p0[k]*exp(- alpha1 * d[i,j] * d[i,j])
        }
      }
    }
    
    # Derived parameters
    N <- sum(z[ ])
    density <- N / (xlimA[2] - xlimA[1]) # divided distances by 100 so calculates turtles per 100 m of canal
  }
", file = "Code/JAGS/SCR_Time.txt")

#### Model with individual heterogeneity in capture probability ####
cat ("
  model {
     # alpha1 ~ dgamma(0.1, 0.1) # consider appropriate prior
     # alpha1 ~ dt(0, 1 / (5^2), 1)I(0, ) 	## implies half-cauchy with scale of 5
     alpha1 ~ dnorm(0, 1 / (25^2))I(0, ) 	## half normal
     sigma <- pow(1 / (2*alpha1), 0.5) # sd of half normal
     psi ~ dunif(0, 1)

sigma_ind ~ dt(0, 1 / (25^2), 1)I(0, ) 	## implies half-cauchy with scale of 25
for(i in 1:M) {
for(k in 1:K) {
eta[i,k] ~ dnorm(0, 1 / (sigma_ind * sigma_ind))
}
}

     for(k in 1:K) {
     alpha0[k] ~ dnorm(0, 0.1)
     }
     for(i in 1:M) {
     z[i] ~ dbern(psi)
     s[i] ~ dunif(xlimA[1], xlimA[2])
     for(j in 1:n_traps) {
     d[i,j] <- abs(s[i] - traplocsA[j])
     for(k in 1:K) {
logit(p0[i,j,k]) <- alpha0[k] + eta[i,k]
     y[i,j, k] ~ dbern(p[i,j,k])
     p[i,j, k] <- z[i]*p0[i,j,k]*exp(- alpha1 * d[i,j] * d[i,j])
     }
     }
     }
     # Derived parameters
     N <- sum(z[ ])
     density <- N / (xlimA[2] - xlimA[1]) # divided distances by 100 so calculates turtles per 100 m of canal
     }
     ", file = "Code/JAGS/SCRA_Ind_Time.txt")

jags_data <- list(y = EM_array, traplocsA = traplocsA, K=K, M=M, xlimA=xlimA, n_traps = n_traps)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sst), z=z, psi = runif(1))
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "z")


#### Model with Sex Covariate #####
cat ("
  model {
     # alpha1 ~ dgamma(0.1, 0.1) # consider appropriate prior
     # alpha1 ~ dt(0, 1 / (5^2), 1)I(0, ) 	## implies half-cauchy with scale of 5
     psi ~ dunif(0, 1)
     psi.sex ~ dunif(0, 1)
     
     for(t in 1:2){
     alpha1[t] ~ dnorm(0, 1 / (25^2))I(0, ) 	## half normal
     sigma[t] <- pow(1 / (2*alpha1[t]), 0.5) # sd of half normal
     } # t

     
     sigma_ind ~ dt(0, 1 / (25^2), 1)I(0, ) 	## implies half-cauchy with scale of 25
     for(i in 1:M) {
     for(k in 1:K) {
     eta[i,k] ~ dnorm(0, 1 / (sigma_ind * sigma_ind))
     } # i
     } # k
     
     for(k in 1:K) {
     for(t in 1:2) {
     alpha0[k, t] ~ dnorm(0, 0.1)
     } # k
     } # t

     for(i in 1:M) {
     z[i] ~ dbern(psi)
     s[i] ~ dunif(xlimA[1], xlimA[2])

     for(j in 1:n_traps) {
     d[i,j] <- abs(s[i] - traplocsA[j])

     for(k in 1:K) {
     for(t in 1:2) {
     logit(p0[i, j, k, t]) <- alpha0[k, t] + eta[i,k]
     } # i
     } # j
     } # k
     } # t

     for(i in 1:M) {
     Sex[i] ~ dbern(psi.sex)
     Sex2[i] <- Sex[i] + 1
     for (j in 1:n_traps) {
     for (k in 1:K) {
     y[i, j, k] ~ dbern(p[i,j,k])
     p[i, j, k] <- z[i]*p0[Sex2[i], j, k]*exp(- alpha1[Sex2[i]] * d[i,j] * d[i,j])
     } # i
     } # j
     } # k

     # Derived parameters
     N <- sum(z[ ])
     density <- N / (xlimA[2] - xlimA[1]) # divided distances by 100 so calculates turtles per 100 m of canal
     }
     ", file = "Code/JAGS/SCR_Ind_Time_Sex.txt")


jags_data <- list(y = EM_array, Sex = Sex, traplocsA = traplocsA, K=K, M=M, xlimA=xlimA, n_traps = n_traps)
inits <- function() {
  list(alpha0=rnorm(4,-2,.4), alpha1=runif(1,1,2), s=as.numeric(sst), z=z, psi = runif(1), psi.sex = runif(1), Sex = Sex)
}

parameters <- c("alpha0", "alpha1", "sigma", "N", "density", "s", "sigma_ind", "psi", "psi.sex", "z") # 

# cpic_1_mcmc <- jagsUI(model.file = "Code/JAGS/SCRA.txt", parameters.to.save = parameters, data=jags_data, inits=inits, n.iter = 1000, n.chains = 3, n.adapt =500) # jagsUI is nice but the plotting is interactive which is obnoxious 

# plot(cpic_1_mcmc)
# traceplot(cpic_1_mcmc)
# cpic_1_mcmc

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

# run in parallel explicitly
library(rjags)
library(parallel)

cl <- makeCluster(nc)                       # Request # cores
clusterExport(cl, c("jags_data", "inits", "parameters", "z", "sst", "ni", "na", "nt")) # Make these available
clusterSetRNGStream(cl = cl, 54354354)

system.time({ # no status bar (% complete) when run in parallel
  out <- clusterEvalQ(cl, {
    library(rjags)
    jm <- jags.model("Code/JAGS/SCR_Ind_Time.txt", jags_data, inits, n.adapt = na, n.chains = 1) # Compile model and run burnin
    out <- coda.samples(jm, parameters, n.iter = ni, thin = nt) # Sample from posterior distribution
    return(as.mcmc(out))
  })
}) # 

stopCluster(cl)

# Results
cpic_1_mcmc <- mcmc.list(out)
plot(cpic_1_mcmc[ , c("alpha1")])
plot(cpic_1_mcmc[ , c("alpha1", "sigma_ind")])
plot(cpic_1_mcmc[ , c("alpha1", "density", "N")]) #
par(mfrow = c(1,1))
summary(cpic_1_mcmc[ , c("alpha0[1]", "alpha1", "density")])
# summary(cpic_1_mcmc)

if(!dir.exists("Results/JAGS")) dir.create("Results/JAGS/", recursive = TRUE)
save(cpic_1_mcmc, file = "Results/JAGS/cpic_1_mcmc.RData")


#------ Prior check on alpha1 -----
dhalfcauchy <- function(x, scale=1, log=FALSE)
  # probability density function of a half-Cauchy distribution
{
  stopifnot(scale>0)
  result <- rep(-Inf, length(x))
  result[x>=0] <- log(2) + dcauchy(x[x>=0], location=0, scale=scale, log=TRUE)
  if (!log) result <- exp(result)
  return(result)  
}

df <- data.frame(chain = 1, prob = "posterior", alpha1 = as.numeric(out[[1]][ , "alpha1"]), stringsAsFactors = FALSE)
for(i in 2:nc) {
  df <- bind_rows(df, data.frame(chain = i, prob = "posterior", alpha1 = as.numeric(out[[1]][ , "alpha1"]), stringsAsFactors = FALSE))
}
tmp <- data.frame(chain = NA_integer_, prob = "prior", alpha1 = abs(rcauchy(nrow(df), 0, 5)), stringsAsFactors = FALSE)
df <- bind_rows(df, tmp)
# alpha1 <- rt(ncol(df), df = 1, 1/(5^2))
# alpha1 <- alpha1[alpha1 >= 0]
ggplot(df) + geom_density(aes(x = alpha1, fill = as.factor(prob)), alpha = 0.2) + xlim(0, 15)


dhalfnorm <- function(x, scale=1, log=FALSE)
  # probability density function of a half-normal distribution
{
  stopifnot(scale>0)
  result <- rep(-Inf, length(x))
  result[x>=0] <- log(2) + dnorm(x[x>=0], mean=0, sd=scale, log=TRUE)
  if (!log) result <- exp(result)
  return(result)  
}
df <- data.frame(chain = 1, prob = "posterior", alpha1 = as.numeric(out[[1]][ , "alpha1"]), stringsAsFactors = FALSE)
for(i in 2:nc) {
  df <- bind_rows(df, data.frame(chain = i, prob = "posterior", alpha1 = as.numeric(out[[1]][ , "alpha1"]), stringsAsFactors = FALSE))
}
tmp <- data.frame(chain = NA_integer_, prob = "prior", alpha1 = dhalfnorm(seq(0, max(df$alpha1), length.out = nrow(df)), 5), stringsAsFactors = FALSE)
df <- bind_rows(df, tmp)
# alpha1 <- rt(ncol(df), df = 1, 1/(5^2))
# alpha1 <- alpha1[alpha1 >= 0]
ggplot(df) + geom_density(aes(x = alpha1, fill = as.factor(prob)), alpha = 0.2) + xlim(0, 10)



df <- data.frame(chain = 1, prob = "posterior", alpha1 = as.numeric(out[[1]][ , "alpha1"]), stringsAsFactors = FALSE)
for(i in 2:nc) {
  df <- bind_rows(df, data.frame(chain = i, prob = "posterior", alpha1 = as.numeric(out[[1]][ , "alpha1"]), stringsAsFactors = FALSE))
}
tmp <- data.frame(chain = NA_integer_, prob = "prior", alpha1 = abs(rnorm(nrow(df), 0,  5)), stringsAsFactors = FALSE)
df <- bind_rows(df, tmp)
# alpha1 <- rt(ncol(df), df = 1, 1/(5^2))
# alpha1 <- alpha1[alpha1 >= 0]
ggplot(df) + geom_density(aes(x = alpha1, fill = as.factor(prob)), alpha = 0.2) + xlim(0, 5)

#-----

# library(R2jags)
# jinitA <- R2jags::jags(model.file = "Code/JAGS/SCRA.txt", parameters.to.save = parameters, data=jags_data, inits=inits, n.chains = 3, n.burnin = 500, n.iter = 1000)

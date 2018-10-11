#######################################################################################################################################################################################################
# Place the:
#
# 'minkData.Rd' 
# 'Pilot_streams_mhk.shp' [<- rivers] and
# 'Pilot_SSbuff_mhk.shp' [<- boundary] 
# 
# files in a folder (i.e. replace 'XXX')
#
# Sourcing this into the R workspace will make the following available:
#
# stateSpaceData: 	a dataframe containing the X Y coordinates (UTM divided by 1000 to convert to km) of the centerpoint of each 200m x 200m grid cell, and the associated standardized covariates.
# trapData: 		a dataframe containing the X Y coordinates (UTM divided by 1000 to convert to km) of each of the 599 traps and the associated standardized covariates.
# encHistories:		a 37(rows) x 599(cols) matrix (individual by trap). A value '0' indicates that the individual was not captured at a trap and '1' indicates that it was.
#
# install the raster library:
  library(raster) # load this library!

# source in the data
   source('XXX//minkData.Rd')
   buff <- shapefile("XXX\\Pilot_SSbuff_mhk.shp")
   rivers <- shapefile("XXX\\Pilot_streams_mhk.shp")
   head(minkData$stateSpaceData)
# look at the state space data:

   par(mfrow=c(2,2))
   for( i in 1:4){
     plot(rasterFromXYZ(cbind(minkData$stateSpaceData[,c(1,2)]*1000,
                              minkData$stateSpaceData[,i+2])))
     plot(buff,lwd=2,add=T)
     lines(rivers,lwd=1,col=4)
   }

# look at the trapping data:
  par(mfrow=c(1,1))
  plot(rivers,col=4)
  plot(buff,lwd=2,add=T)
  points(minkData$trapData[,c(1,2)]*1000,pch=3)
  head(minkData$trapData)
  
# look at encounter hisories:

  head(minkData$encHistories)
  # or reduce to just traps with captures:
  head(minkData$encHistories[,apply(minkData$encHistories,2,sum)>0])

###################################################################################
# Functions for fitting the model:

# e2dist - makes a distance matrix between traps and state space points:

e2dist <- function (x, y){ i <- sort(rep(1:nrow(y), nrow(x)))
                           dvec <- sqrt((x[, 1] - y[i, 1])^2 + (x[, 2] - y[i, 2])^2)
                           matrix(dvec, nrow = nrow(x), ncol = nrow(y), byrow = F) }



# the integrated likelihood function that was minimized and use to obtain mle's
#  - please direct any questions to chrissuthy@gmail.com

intLikFunction <- 

function (start = NULL, y = y, K = NULL, Xdata = trapdata, Gdata = NULL, Pformula=~1, Dformula=~1,
          ssbuffer = 2, delta = 0.3, model="B", predict=FALSE){

  X <- as.matrix(Xdata[,c("X","Y")])
  pars <- rep(0,3 + length(all.vars(Pformula)) + length(all.vars(Dformula)))

  if(is.null(start))
   start <- rep(0,length(pars))

  if (is.null(K))
    return("need sample size")
  if (is.null(Gdata)) {
    Xl <- min(X[, 1]) - ssbuffer
    Xu <- max(X[, 1]) + ssbuffer
    Yu <- max(X[, 2]) + ssbuffer
    Yl <- min(X[, 2]) - ssbuffer
    SSarea <- (Xu - Xl) * (Yu - Yl)
    xg <- seq(Xl + delta/2, Xu - delta/2, delta)
    yg <- seq(Yl + delta/2, Yu - delta/2, delta)
    npix.x <- length(xg)
    npix.y <- length(yg)
    area <- (Xu - Xl) * (Yu - Yl)/((npix.x) * (npix.y))
    G <- cbind(rep(xg, npix.y), sort(rep(yg, npix.x)))
  }
  else {
    G <- as.matrix(Gdata[,c("X","Y")])
    G <- G
    SSarea <- nrow(G)
  }

  nG <- nrow(G)
  D <- e2dist(X, G)
  n0 <- exp(start[1])
  alpha1 <- exp(start[2])
  alpha0 <- start[3]

  if(Pformula==~1){
    P_cov <- NULL
    modelP <- NULL
    npp <- 0
   if(model=="B") # Bernoulli
     probcap <- plogis(alpha0) * exp(-alpha1 * D * D)
   if(model=="P") # Poisson
     probcap <- exp(alpha0) * exp(-alpha1 * D * D)
  }
  else{
    P_cov <- data.frame(Xdata[,which(colnames(Xdata) %in% all.vars(Pformula))])
    colnames(P_cov) <- all.vars(Pformula)
    modelP <- model.matrix(Pformula, P_cov)
    P_beta <- c(alpha0,start[3+1:max(attr(modelP,"assign"))])
    npp <- max(attr(modelP,"assign"))
   if(model=="B") # Bernoulli
     probcap <- c(plogis(modelP %*% P_beta)) * exp(-alpha1 * D * D)
   if(model=="P") # Poisson
     probcap <- c(exp(modelP %*% P_beta)) * exp(-alpha1 * D * D)
  }

  if(Dformula==~1){
    D_cov <- NULL
    modelD <- NULL
    probs <- (1/nG)
  }
  else{
    D_cov <- data.frame(Gdata[,which(colnames(Gdata) %in% all.vars(Dformula))])
    colnames(D_cov) <- all.vars(Dformula)
    modelD <- model.matrix(Dformula, D_cov)
    D_beta <- start[3 + npp + 1:max(attr(modelD,"assign"))]
    probs <- exp(modelD %*% c(0,D_beta))
    probs <- probs/sum(probs)
  }

  Pm <- matrix(NA, nrow = nrow(D), ncol = ncol(D))
  ymat <- y
  ymat <- rbind(ymat,rep(0,nrow(X)))
  lik.marg <- rep(NA,nrow(ymat))


   for (i in 1:nrow(ymat)){
    if(model=="B")
      Pm[1:length(Pm)] <- (dbinom(rep(ymat[i, ], nG), rep(K, nG), probcap[1:length(Pm)], log = TRUE))
    if(model=="P")
      Pm[1:length(Pm)] <- (dpois(rep(ymat[i, ], nG), rep(K, nG)*probcap[1:length(Pm)], log = TRUE))
    lik.cond <- exp(colSums(Pm))
    lik.marg[i] <- sum(lik.cond * probs)
   }

  if(predict==FALSE){
    nv <- c(rep(1, length(lik.marg) - 1), n0)
    part1 <- lgamma(nrow(y) + n0 + 1) - lgamma(n0 + 1)
    part2 <- sum(nv * log(lik.marg))
    out <- -1 * (part1 + part2)
    attr(out, "SSarea") <- SSarea
    return(out)
  }

  if(predict==TRUE){
    posterior<-matrix(NA,nrow=nG,ncol=nrow(ymat))
   for(i in 1:nrow(ymat)){
    if(model=="B")
      Pm[1:length(Pm)] <- (dbinom(rep(ymat[i, ], nG), rep(K, nG), probcap[1:length(Pm)], log = TRUE))
    if(model=="P")
      Pm[1:length(Pm)] <- (dpois(rep(ymat[i, ], nG), rep(K, nG)*probcap[1:length(Pm)], log = TRUE))
     lik.cond <- exp(colSums(Pm))
     posterior[,i]<- (lik.cond*probs)/lik.marg[i]
   }
    return(cbind(G,posterior))
  }
}



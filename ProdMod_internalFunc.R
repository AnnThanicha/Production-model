# Internal functions
#~~~~~~~~~~~~~~~~~~~#


# Body weight calibrartion function
BW <- function(parCAT, cow){
  parCAT <- parCAT
  cow <- cow
  p1 <- rnorm(length(parCAT[parCAT==1]), 540, 6) + cow[["dim"]][which(parCAT == 1)] * ADG
  p2 <- rnorm(length(parCAT[parCAT==2]), 595, 6) + cow[["dim"]][which(parCAT == 2)] * ADG
  p3 <- rnorm(length(parCAT[parCAT==3]), 650, 6)
  out <- rep(NA, n_cows)
  out[parCAT==1] <- p1 ; out[parCAT==2] <- p2 ; out[parCAT==3] <- p3
  return(out)
}

# relative production level for each cow; variation in milk production between cows
RPL<- function(n){
  n<- length(n)
  RPL<- rnorm(n, 0, 0.1)
  return(RPL)
}

# number of milking days drawn from a normal distribution (standardised DM of 305 used as mean and preliminary SD of 10 [19/07/2019])
MDL_func<- function(n){
  n<- length(n)
  MDL<-  round(rnorm(n, 285, 0))
  return(MDL)
}

# pert distribution function to draw stochastic seasonal probabilities
PERTseason <- function(season){ 
  sapply(seq_along(DisName), function(x){
    rpert(1, seasonals[[season]][x,1], seasonals[[season]][x,2], seasonals[[season]][x,3])/(365/2)
  })
}

# Soujourn time
sj2 <- function(n){                              # soujorn time from M2 to M4
  round((rnorm(n, 4.895, 0.13735) + rnorm(n, 1.175, 0.03))  * 14.08)
}
sj2b <- function(n){                              # soujorn time from M4.1 to M4
  round((rnorm(n, 4.895, 0.13735) + rnorm(n, 0.209, 0.038))  * 14.08)
}
sj2_clb <- function(n){                            # soujorn time of calibration and intialisation
  round(sample(65:135,  length(n), replace = TRUE))
}

# function to predict score regression interval
ms_progrFUN <- function(x, y){
  round(runif(1, min = ms_intervals[[x]][y, 1], max = ms_intervals[[x]][y, 2]))
}

# function to predict score regression interval
ms_recoFUN <- function(x, y){
  round(runif(1, min = ms_reco[[x]][y, 1], max = ms_reco[[x]][y, 2]))
}

# re-infection per sisorder per score
rinFUN <- function(x, y){
  rbinom(1, 1, prob = p_Rinf[x, as.character(y)])
}

# convert binomial log odds to relative risks with the function
blr_RR <- function(b0 = intercept, bx = odds){ 
  b0 <- b0 ; bx <- log(bx)
  dmtr <- sapply(bx, function(x){1+exp(-b0-x)})
  RR <- (1+exp(-b0))/dmtr
  return(RR)
}


# plotting
base_breaks_x_continuous <- function(x){
  b <- pretty(x)
  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_continuous(breaks=b))
}

base_breaks_x_discrete <- function(x){
  b <- levels(x)
  d <- data.frame(y=-Inf, yend=-Inf, x=b[1], xend=tail(b, 1))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_x_discrete(breaks=b))
}

base_breaks_y <- function(x){
  b <- pretty(x)
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b))
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
       scale_y_continuous(breaks=b))
}


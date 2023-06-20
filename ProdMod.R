setwd(paste0("C:/Users/", Sys.info()["user"], "/OneDrive - Wageningen University & Research/PhD/Modelling/ProdMod"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Load packages -----------------------------------------------------------

library(data.table)
library(profvis)
library(directlabels)
library(esquisse)
library(Hmisc)
library(varhandle)
library(tidyverse)
library(beepr)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(mc2d)
library(devtools)

# Source inputs -----------------------------------------------------------


source("ProdMod_internalFunc.R")                                                    
xPRM         <- mget(load("ProdMod_defaultParameters.RData", 
                          envir=(NE. <- new.env())), envir=NE.)$xPRM; rm(NE.)  # input parameters in list
daysOpen.mat <- fread("daysOpen_mat.csv")                                      # related to fertility culling decisions

# control herd size, number of simulations and time horizon here
xPRM$n_cows <- 10      # number of simulated cows per time step
xPRM$n_sim  <- 1      # number of simulated replications
xPRM$n_yrs  <- 1

# ensure daysOpen.mat is set to correct herd size
if(xPRM$n_cows != nrow(daysOpen.mat)){
  print("The number of cows you want to simulate is not equal to the cows used to generate `daysOpen.mat`. 
        Please, ensure the the number of cows in `daysOpen.mat` is equal to the number of cows you want to simulate. 
        Your `daysOpen.mat` will be removed from the environment now!")
  rm(daysOpen.mat)
}

# Simulation model --------------------------------------------------------

mkFrames <- function(X = xPRM) {

  n <- with(X, ts*n_yrs)
  list2env(X, globalenv())

# Object initialisation ---------------------------------------------------
       
  cowsList  <- vector(mode = "list", length = n) # list to collect daily data for cows
  cullsList <- vector(mode = "list", length = n) # list to collect cull data
  
  # initialise cow-spaces before simulation
  cow      <- vector(mode = "list")
  cullcows <- list() #~ used for validation purposes, collects culled cow data
  
  cd1 <- paste0(paste0(rep(0, 5-1), sep="", collapse=""), 1, "_")
  
  cow[["year"]]           <- rep(0, n_cows)
  cow[["day"]]            <- rep(0, n_cows)
  cow[["UID"]]            <- sprintf("%s%0*d", cd1, 11 - nchar(cd1), 1:n_cows)
  cow[["space"]]          <- c(1:n_cows)    # cow space
  cow[["status"]]         <- rep(1, n_cows) # categorical variable (1 = healthy; 2 = culled for fertility reasons; 3 = culled at the end of useful life(max par);
                                            # 4 = culled for general reasons; 5 = mortality; 6 = lame; 0 = no cow in space)
  cow[["par"]]            <- sample(c(1:max_par), size = n_cows, prob = p_par, replace = TRUE) ; parCAT <- cow[["par"]] ; parCAT[parCAT > 3] <- 3# parity
  cow[["dim"]]            <- sample(1:(285+DPL), n_cows, replace = TRUE) # days in milk
  cow[["mdl"]]            <- replicate(n_cows, MDL_func(n_cows))       # milking days left
  cow[["rpl"]]            <- replicate(n_cows, RPL(n_cows))            # relative production level
  cow[["yield"]]          <- rep(0, n_cows)
  cow[["yield.rdc"]]      <- rep(0, n_cows)    # reduced daily yield
  cow[["detoest"]]        <- rep(0, n_cows)    # detect oestrus variable
  cow[["calf"]]           <- rep(0, n_cows)    # variable indicating whether a calf is born or not
  cow[["cull"]]           <- rep(0, n_cows)    # variable to tag cull cows (2 = fert; 3 = max.par; 4 = gen.; 5 = mortality; 6 = lame)
  cow[["oestrus"]]        <- rep(0, n_cows)    # variable scheduling day of oestrus
  cow[["daycalf"]]        <- (cow[["mdl"]] + DPL) - (cow[["dim"]]) + 1 # day of calving (initialisation)
  cow[["prevdaycalf"]]    <- rep(0, n_cows)
  cow[["pregnant"]]       <- ifelse(cow[["daycalf"]] > cow[["dim"]], 1, 0)        # variable indicating pregnancy status 
  cow[["dayendvwp"]]      <- rep(0, n_cows)    # variable scheduling the day end of VWP
  cow[["daycullfert"]]    <- rep(0, n_cows)    # variable scheduling the day a decision is made to stop inseminating and to tag cow as cull for fertility reasons
  cow[["n_insem"]]        <- rep(0, n_cows)    # counter variable regarding the number of inseminations
  cow[["m_oest"]]         <- rep(0, n_cows)    # counter variable regarding number of missed oestrus
  cow[["rhenter"]]        <- rep(0, n_cows)    # variable scheduling the day of replacement heifer entering the herd
  cow[["weight"]]         <- BW(parCAT, cow)
  cow[["act.weight"]]     <- cow[["weight"]]
  cow[["energy"]]         <- rep(0, n_cows)
  cow[["act.energy"]]     <- cow[["energy"]]
  cow[["disc.milk"]]      <- rep(0, n_cows)
  cow[["cost.insem"]]     <- rep(0, n_cows)
  cow[["cost.feed"]]      <- rep(0, n_cows)
  cow[["cost.cull"]]      <- rep(0, n_cows)
  cow[["cost.milk"]]      <- rep(0, n_cows)
  cow[["cost.milk.disc"]] <- rep(0, n_cows)
  cow[["rev.milk"]]       <- rep(0, n_cows)
  
  cow.entries   <- n_cows
  day           <- 0
  
  #~ Other initialisation processes to follow...
  
  # For loop ----------------------------------------------------------------  

  ptm <- proc.time()
  for(i in 1:n){
    # set.seed(i)
    day <- day + 1
    # model simulation starts on March 1st to account for seasonality
    season <- (ifelse(day %% 365 == 0, 365, day %% 365) %/% 92) + 1 

    cow[["day"]]  <- ifelse(cow[["day"]] == ts, 1, cow[["day"]] + 1) # update day in calendar year
    cow[["i"]]    <- i # rolling time steps
    cow[["year"]] <- ceiling(day/365)
    

# Culling and replacement -------------------------------------------------

    # replacement heifers will enter the herd the following day a cow as been removed from the milking herd  due to the above mentioned culling reasons
    culls1<- which(cow[["cull"]] %nin% c(0, 5)) 
    rhe<- which(cow[["cull"]] == 5 & day == cow[["rhenter"]])
    culls <- c(culls1, rhe)
    if(length(culls) > 0){
      
      if(validate == TRUE){
        # collect cull cow data 
        cullcows<- lapply(cow, "[", culls)
        cullcows[["cull"]]<- cow[["cull"]][culls]
        cullcows[["i"]]<- day
        cullsList[[i]]<- do.call(cbind, cullcows)
      }
      
      # reset variables in cow list
      z<- c("UID", "dim", "mdl", "yield", "yield.rdc", "rpl", "detoest", "pregnant", "calf", "cull", 
            "status", "oestrus", "daycalf", "prevdaycalf", "dayendvwp", "daycullfert", 
            "n_insem", "m_oest", "weight", "act.weight", "energy", "act.energy", "cost.insem", "cost.cull") #~  (add variables as you bring them to life in the model)
      cow[z] <- lapply(cow[z], function(x) {x[culls] <- 0; x})
      
      # replacement heifer will enter milking herd on the assumption that cows to be culled are removed from the milking herd when a heifer is available
      z<- c("status", "par") # variables to reset with same value in cow list
      cow[z]<- lapply(cow[z], function(x) {x[culls] <- 1; x})
      cow[["mdl"]][culls]      <-  MDL_func(length(culls))
      cow[["rpl"]][culls]      <-  RPL(culls)
      cow[["dayendvwp"]][culls]<-  VWP + day + 1
      cow[["oestrus"]][culls]  <-  sample(14:27, length(culls), replace = TRUE) + day
      cow[["weight"]][culls]   <- rnorm(length(culls), 540, 6)
      cow[["act.weight"]][culls] <- cow[["weight"]][culls] 
      # determine day which a cow can remain open until being flagged as a cull
  
      for(cullsLoop in culls){
        cow[["daycullfert"]][cullsLoop] <- ifelse(cow[["rpl"]][cullsLoop] < daysOpen.mat$mean.rpl[1], cow[["oestrus"]][cullsLoop] + (21*2) + 1 - day, # accounting for cows with lowest RPL; 3 oestrus cycles; day is subtracted here since it is added at the end
                                                  ifelse(cow[["rpl"]][cullsLoop] > daysOpen.mat$mean.rpl[nrow(daysOpen.mat)], daysOpen.mat$days.open[nrow(daysOpen.mat)],
                                                         round(median(c(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cullsLoop])][length(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cullsLoop])])],
                                                                        daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl > cow[["rpl"]][cullsLoop])][1]), na.rm = TRUE)))) + day + 1
      }
      
      
      
      # for replacement heifers that still have a culling decision day before the first oestrus, it is adjusted here
      adjcull2 <- which(cow[["daycullfert"]][culls] <= (cow[["oestrus"]][culls] + (21*2)))
      if(length(adjcull2) > 0){
        cow[["daycullfert"]][culls[adjcull2]] <-  (cow[["oestrus"]][culls[adjcull2]] + (21*2)) + 1
      }
      
      #unique identification of cows
      cd1 <- paste0(paste0(rep(0, 5 - nchar(as.character(day))), sep="", collapse=""), day, "_" )
      cow[["UID"]][culls] <- sprintf("%s%0*d", cd1, 11 - nchar(cd1), cow.entries + 1:length(culls))
      cow.entries <- cow.entries + length(culls)
    }
    
    
# Production --------------------------------------------------------------

    cow[["dim"]] <- cow[["dim"]] + 1
    # update oestrus cycles during VWP for healthy cows since farmer will not be interested in the oestrus cycles of a cow flagged to be culled
    oestwait <- which(cow[["oestrus"]] < cow[["dayendvwp"]])
    if(length(oestwait) > 0){
      cow[["oestrus"]][oestwait]<- cow[["oestrus"]][oestwait] + 21 # fixed 21 day oestrus interval
    }
    
    # when a cow is flagged for general culling before it has conceived, no need to find the day in which a fertility culling decision needs to be made
    nodaycf<- which(cow[["status"]] %in% c(4,5) & cow[["oestrus"]] >= cow[["dayendvwp"]])
    if(length(nodaycf) > 0){
      cow[["daycullfert"]][nodaycf]<- 0
    }
    
    # find cows in max parity since we are not interested in detecting for oestrus (maybe there is a neater way to do this..)
    mxp<- which(cow[["par"]] == max_par)
    if(length(mxp) > 0){
      cow[c("oestrus", "daycalf", "dayendvwp", "daycullfert")]<-  lapply(cow[c("oestrus", "daycalf", "dayendvwp", "daycullfert")], function(x) {x[mxp]<- 0; x})
    }
    
    # if oestrus detection in the previous day was successful, no need to detect in the current day
    nodetect<- which(cow[["detoest"]] == 1)
    if(length(nodetect) > 0){
      cow[["detoest"]][nodetect]    <- 0 
      cow[["cost.insem"]][nodetect] <- 0 # cost only occurs on insemination  
    }
    
    # find day that cows goes into oestrus after VWP and healthy (i.e. cow[["status"]] == 1) cows the farmer would be willing to detect
    do<- which(day == cow[["oestrus"]] & cow[["status"]] %in% c(1))
    # once found the day that the cow will go into oestrus
    if(length(do) > 0){
      # predict detection of oestrus by binomial process and assume insemination occurs after successful detection
      cow[["detoest"]][do]<- rbinom(length(do), 1, prob = p_dtoes)
    }
    
    # where oestrus has been detected, count insemination 
    ins<- which(cow[["detoest"]] == 1)
    if(length(ins) > 0){
      cow[["n_insem"]][ins]    <- cow[["n_insem"]][ins] + 1
      cow[["cost.insem"]][ins] <- cost.insem 
    }
    
    # once cow is successfully detected/inseminated, predict conception by binomial process
    # by first finding successfully inseminated cows
    si<- which(cow[["detoest"]] == 1)
    if(length(si) > 0){
      n_ins <- cow[["n_insem"]][si] ; n_ins[n_ins > 6] <- 6
      sc<- si[rbinom(length(si), 1, prob = p_conc[n_ins]) == 1] # successful conception
      # tag cow as pregnant after successful conception
      if(length(sc) > 0){
        cow[["pregnant"]][sc]    <- 1
        cow[["daycalf"]][sc]     <- round(rnorm(length(sc), 281, 3)) + day # predict day of calving (calibrated to Inchaisri)
        cow[["mdl"]][sc]         <- ((cow[["daycalf"]][sc] - DPL - day) - (cow[["mdl"]][sc] - cow[["dim"]][sc])) + cow[["mdl"]][sc] - 1 # adjust number of milking days to achieve fixed DPL
        cow[["daycullfert"]][sc] <- 0 # no culling day decision since cow is pregnant (this scheduling variable is set on the day of calving)
      }
    }
    
    # where oestrus has not been detected or unsuccessful insemination occurred for cows that have not yet been flagged for culling reasons (=2 etc.), add another oestrus cycle
    missedoest<- which(cow[["status"]] %in% c(1, 6) & cow[["oestrus"]] == day & cow[["pregnant"]] == 0)   # detected and not pregnant
    if(length(missedoest) > 0){
      cow[["oestrus"]][missedoest] <- cow[["oestrus"]][missedoest] + 21 
    }
    missedoest2 <- which(cow[["detoest"]][missedoest] == 0) # not detected 
    if(length(missedoest2) > 0){
      cow[["m_oest"]][missedoest[missedoest2]] <- cow[["m_oest"]][missedoest[missedoest2]] + 1
    }
    
    # if calf was born in the previous day, calf cannot be born in the current day (i.e., reset variables)
    calfborn<- which(cow[['calf']] == 1)
    if(length(calfborn) > 0){
      cow[["calf"]][calfborn]<- 0
      cow[["prevdaycalf"]][calfborn] <- cow[["daycalf"]][calfborn]
      cow[["daycalf"]][calfborn] <- 0 # reset pregnant variable to 0 since it is not know when the cow will next conceive at this point in time
    }
    
    # find the day of calving; this will also be first day of milking
    cd<- which(cow[["daycalf"]] == day)
    if(length(cd) > 0){
      cow[["calf"]][cd]<- rbinom(length(cd), 1, 1-p_stlbrn)
      cow[["pregnant"]][cd]<- 0
      # find cows that have started milking to predict number of days until first heat cycle (order of oestrus and oestrus detection is important)
      cow[["dayendvwp"]][cd]<- VWP + day # find the first day after the end of the voluntary waiting period
      cow[["oestrus"]][cd[cow[["par"]][cd] == 1]]<- sample(14:27, length(cd[cow[["par"]][cd] == 1]), replace = TRUE) + day # find day that cow will come into first oestrus (parity 1) (this is probably obsolete here since this is run when a replacement heifer enters the herd above)
      cow[["oestrus"]][cd[cow[["par"]][cd] != 1]]<- sample(18:21, length(cd[cow[["par"]][cd] != 1]), replace = TRUE) + day # find day that cow will come into first oestrus (parity 2+)
      cow[["par"]][cd]<- cow[["par"]][cd] + 1
      cow[["dim"]][cd]<- 1
      cow[["mdl"]][cd]<- MDL_func(cd)
      # estimate milk yield (not yet corrected) to account for the first day of milking
      
      # reset insemination count for following lactation
      cow[["n_insem"]][cd] <- 0
      cow[["m_oest"]][cd]  <- 0

      
      # determine day which day a cow can remain open until being flagged as a cull
      
      #~ @Xiaomei: 
      #~ I imagine that the Chinese dairy system has a different approach of culling for fertility reasons
      #~ if not, then remember to set the n_cows in the `daysOpen_func.R` to your desired herd size
      for(cdLoop in cd){
        cow[["daycullfert"]][cdLoop] <- ifelse(cow[["rpl"]][cdLoop] < daysOpen.mat$mean.rpl[1], cow[["oestrus"]][cdLoop] + (21*2) + 1 - day, # accounting for cows with lowest RPL; 3 oestrus cycles; day is subtracted here since it is added at the end
                                               ifelse(cow[["rpl"]][cdLoop] > daysOpen.mat$mean.rpl[nrow(daysOpen.mat)], daysOpen.mat$days.open[nrow(daysOpen.mat)], 
                                                      round(median(c(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cdLoop])][length(daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl <= cow[["rpl"]][cdLoop])])], 
                                                                     daysOpen.mat$days.open[which(daysOpen.mat$mean.rpl > cow[["rpl"]][cdLoop])][1]), na.rm = TRUE)))) + day + 1
      }
      # for cows that still have a culling decision day before the first oestrus, it is adjusted here
      adjcull <- which(cow[["daycullfert"]][cd] <= (cow[["oestrus"]][cd] + (21*2)))
      if(length(adjcull) > 0){
        cow[["daycullfert"]][cd[adjcull]] <-  (cow[["oestrus"]][cd[adjcull]] + (21*2)) + 1
      }
    }
    
    # stop milking after last milking day 
    stopmilk<- which(cow[["dim"]] > cow[["mdl"]])
    if(length(stopmilk) > 0){
      cow[["yield"]][stopmilk] <- 0
    }
    
    # continue milking while cow is still in milk
    inmilk<- which(cow[["dim"]] <= cow[["mdl"]])
    if(length(inmilk) > 0){
      parCATdim <- cow[["par"]][inmilk] ; parCATdim[parCATdim > 3] <- 3
      # estimate milk yield (not yet Fat & Protein corrected)
      cow[["yield"]][inmilk] <- a[parCATdim] + b[parCATdim] * cow[["dim"]][inmilk] + 
        d * exp(-K * cow[["dim"]][inmilk]) + cow[["rpl"]][inmilk] * ady[parCATdim]
    }
    
    
    # Average daily weight gain
    bwg  <- which(cow[["par"]] < 3)
    if(length(bwg) > 0){
      cow[["weight"]][bwg] <- cow[["weight"]][bwg] + ADG 
    }
    
    # Energy requirements
    
    # maintenance and FPCM
    EparCat <- cow[["par"]] ; EparCat <- EparCat[EparCat > 3] <- 3
    cow[["energy"]] <- 42.4 * cow[["weight"]]^0.75 + 442 * (cow[["yield"]] * 0.337 + 0.116 * fat[EparCat]*100 + 0.06 * prot[EparCat]*100)
    
    # growth
    Eg <- which(cow[["par"]] < 3)
    if(length(Eg) > 0){
      cow[["energy"]][Eg] <- cow[["energy"]][Eg] + G.ENGY[cow[["par"]][Eg]]
    }
    
    # stage of pregnancy
    Ep <- which(cow[["pregnant"]] == 1 & (cow[["daycalf"]] -  day) <= 4 * 30.5 & (cow[["daycalf"]] -  day) > 0)
    if(length(Ep) > 0){
      pregstage <- ceiling((cow[["daycalf"]][Ep] - day)/30.5)
      if(any(pregstage == 0)){print("There is a ZERO in your pregstage") ; browser()}
      cow[["energy"]][Ep] <- cow[["energy"]][Ep] + P.ENGY[pregstage]
    }
    
   
# General culling ---------------------------------------------------------
    # general culling rates will increase with age
    
    # find healthy cows
    hc<- which(cow[["status"]] == 1) # find 'healthy' cows
    parCAtcull <- cow[["par"]][hc] ; parCAtcull[parCAtcull > 5] <- 5
    gencull<- hc[rbinom(length(hc), 1, p_gencull[parCAtcull]/ts) == 1]
    # flag cows to be removed at lactaion end for general culling reasons
    if(length(gencull) > 0){
      cow[["status"]][gencull]<- 4
    }
    
    # probability of a general cull cow sucumbing to mortality
    # find general cull cows
    pmort<- which(cow[["status"]] == 4)
    mort<-  pmort[rbinom(length(pmort), 1, p_mort/ts) == 1]
    if(length(mort) > 0){
      cow[["status"]][mort] <- 5
      cow[["cull"]][mort] <- 5
      cow[["rhenter"]][mort] <- rgeom(length(mort), p_rh) + 1 + day
      
      if(validate == TRUE){
        cullcows<- lapply(cow, "[", mort)
        cullcows[["cull"]]<- cow[["cull"]][mort]
        cullcows[["i"]]<- day
        cullsList[[i]]<- do.call(cbind, cullcows)
      }
      
      z<- c("dim", "mdl", "yield", "yield.rdc", "rpl", "detoest", "pregnant", "status",
            "oestrus", "daycalf", "dayendvwp", "daycullfert") # variables to reset in cow list (add variables as you bring them to life in the model)
      cow[z]<- lapply(cow[z], function(x) {x[mort] <- 0; x})
      
    }
    
    gencull <- which(cow[["status"]] == 4)
    if(length(gencull) > 0){
      cow[["cull"]][gencull]<- 4
      cow[["yield"]][gencull] <- 0
      cow[["yield.rdc"]][gencull] <- 0
    }
    
    # find cows which are not pregnant after n days after first oestrus and/or failed to conceive after max inseminations (lameness status included 06/02/2020)
    # should these cows be milked until the produce below the threshold instead of up to the mdl?
    fertcull<- which(cow[["pregnant"]] == 0 & cow[["daycullfert"]] == day) # & (cow[["par"]] != max_par | cow[["status"]] == 6)
    # flag cow which will be culled at end of lactation for fertility reasons
    if(length(fertcull) > 0){
      cow[["status"]][fertcull] <- 2
      z<- c("oestrus", "daycalf", "dayendvwp") # variables to reset in lookup list
      cow[z] <-lapply(cow[z], function(x) {x[fertcull] <- 0; x})
    }
    
    # remove cow at end of lactation due to fertility culling reasons and give culling reason
    fc<- which(cow[["status"]] == 2 & cow[["yield"]] - cow[["yield.rdc"]] < yield_thresh)
    # fc2 <- which(cow[["status"]] == 6 & cow[["pregnant"]] == 0 & cow[["yield"]] < yield_thresh & cow[["calf"]] == 0) # remove cows that became lame after tagged as fert cull
    fc <- c(fc, fc2)
    if(length(fc) > 0){
      cow[["cull"]][fc]<- 2
    }
    # find max par cows
    mpc<- which(cow[["par"]] == max_par & cow[["yield"]] < yield_thresh)
    if(length(mpc) > 0){
      cow[["status"]][mpc] <- 3
      cow[["cull"]][mpc]   <- 3 # tag as cull for max_par reasons (=3)
      # cow[c("status", "par", "mdl", "dim", 'yield')]<- lapply(cow[c("status", "par", "mdl", "dim", 'yield')], function(x) {x[mpc] <- 0; x})
    }

    
    # adjust number of milking days left for cull cows if still producing above the threshold
    adjmd <- which(cow[["dim"]] == cow[["mdl"]] & cow[["pregnant"]] == 0 & cow[["yield"]] >= yield_thresh)
    if(length(adjmd) > 0){
      cow[["mdl"]][adjmd] <- cow[["mdl"]][adjmd] + 1 
    }
    

    
    
# Production impacts ------------------------------------------------------

    #~ I have left the variables here as used to calculate the economic variables below in `Economic calculations`
    
    # live weight gain adjusted in "CALVING AND MILKING"
    
    # reduced milk yield
    cow[["yield.rdc"]] <- cow[["yield"]] * 0 # adjusted by some parameter to calculate the reduced yield 
    
    # actual energy
    cow[["act.energy"]] <- cow[["energy"]] - (42.4 * cow[["weight"]]^0.75 + 442 * (cow[["yield"]] * 0.337 + 0.116 * fat[EparCat]*100 + 0.06 * prot[EparCat]*100)) +
      (42.4 * cow[["weight"]]^0.75 + 442 * ((cow[["yield"]]-cow[["yield.rdc"]]) * 0.337 + 0.116 * fat[EparCat]*100 + 0.06 * prot[EparCat]*100))
    
    #~ culling of lame cows:
    #~ @Xiaomei:
    #~ I have deactivated this code becuase I imagine you might follow a similar approach for the culling of cows with BVD::
    
    
    # plc <- which(cow[["vis.ms"]] > 1)
    # parCATcull <- cow[["par"]][plc]
    # parCATcull2 <- parCATcull ; parCATcull2[parCATcull2 > 6] <- 6
    # parCATcull[parCATcull > 5] <- 5
    # rplCATcull <- rep(5, n_cows) ; rplCATcull[cow[["rpl"]] < MiPrLe[4]] <- 4 ; rplCATcull[cow[["rpl"]] < MiPrLe[3]] <- 3
    # rplCATcull[cow[["rpl"]] < MiPrLe[2]] <- 2 ; rplCATcull[cow[["rpl"]] < MiPrLe[1]] <- 1
    # 
    # lc  <- plc[rbinom(length(plc), 1, prob = p_gencull[parCATcull]/ts * rr_cull[cow[["vis.ms"]][plc]] * hr_p305[rplCATcull[plc]] * rr_cull.par[parCATcull2]) == 1] # cull these lame cows
    # lc2 <- which(cow[["vis.ms"]] >= lameIV & cow[["det.ms"]] == 1 & cow[["lact.tr"]] == NLacTr)
    # lc <- unique(c(lc, lc2))
    # if(length(lc) > 0) {
    #   cow[["cull"]][lc]  <- 6
    #   cow[["yield"]][lc] <- 0
    #   cow[["yield.rdc"]][lc] <- 0
    # }
    
    

# Economic calculations ---------------------------------------------------

    # insemination done in `Production`
    
    # feed
    cow[["cost.feed"]] <- cow[["act.energy"]]/1000 * cost.EkVEM
    
    # culling
    cull.eur <- which(cow[["cull"]] %in% 2:4)
    if(length(cull.eur) > 0){
      cpcat <- cow[["par"]][cull.eur] ; cpcat[cpcat > use_par] <- use_par 
      x.dim <- cow[["dim"]][cull.eur]/cow[["mdl"]][cull.eur] ; x.dim[x.dim > 1] <- 1
      cow[["cost.cull"]][cull.eur] <- ((mean(rpert(length(cull.eur), cost.rear[1], cost.rear[2], cost.rear[3])) - 
                                          (cow[["weight"]][cull.eur]*perc.dress*mean(rpert(length(cull.eur), price.kg[1], price.kg[2], price.kg[3]))))/use_par) *
        (use_par - (cpcat-1 + 1*x.dim))
    }
    
    # mortality costs
    cullM.eur <- which(cow[["cull"]] == 5)
    if(length(cullM.eur) > 0){
      cpcatM <- cow[["par"]][cullM.eur] ; cpcatM[cpcatM > use_par] <- use_par 
      x.dimM <- cow[["dim"]][cullM.eur]/cow[["mdl"]][cullM.eur] ; x.dimM[x.dimM > 1] <- 1
      cow[["cost.cull"]][cullM.eur] <- (mean(rpert(length(cull.eur), cost.rear[1], cost.rear[2], cost.rear[3]))
                                        + (cow[["weight"]][cullM.eur]*perc.dress*mean(sample(price.kg, length(cullM.eur), replace = TRUE))/use_par)) * 
        (use_par - (cpcatM-1 + 1*x.dimM)) + RemCost     
    }
    
    #~ I have kept this placeholder for `some culling reason` culling costs
    #~ it relates to the culling of lame cows (inactive code) in `Production`
    cullBVD.euro <- which(cow[["cull"]] == 6)
    if(length(cullBVD.euro) > 0){
      cpcat <- cow[["par"]][cullBVD.eur] ; cpcat[cpcat > use_par] <- use_par 
      x.dim <- cow[["dim"]][cullBVD.eur]/cow[["mdl"]][cullBVD.eur] ; x.dim[x.dim > 1] <- 1
      cow[["cost.cull"]][cullBVD.eur] <- ((mean(rpert(length(cullBVD.eur), cost.rear[1], cost.rear[2], cost.rear[3])) - 
                                          (cow[["weight"]][cullBVD.eur]*perc.dress*mean(rpert(length(cullBVD.eur), price.kg[1], price.kg[2], price.kg[3]))))/use_par) *
        (use_par - (cpcat-1 + 1*x.dim))
    }
    
    #~ @ Xiaomei:
    #~ not sure if you will need this, but I have left the code availale in any case
    
    # discarded milk 
    # end.discard <- which(cow[["disc.milk"]] == day + 1)
    # if(length(end.discard) > 0){
    #   cow[["disc.milk"]][end.discard] <- 0
    #   cow[["cost.milk.disc"]][end.discard] <- 0
    # }
    
    # cost milk
    cow[["cost.milk"]] <- cow[["yield.rdc"]] * MilkPrice
    milk.cost <- which(cow[["disc.milk"]] != 0)
    if(length(milk.cost) > 0){
      cow[["cost.milk.disc"]][milk.cost] <- (cow[["yield"]][milk.cost] - cow[["yield.rdc"]][milk.cost]) * MilkPrice
    }
    
    # revenue milk
    milk.rev <- which(cow[["disc.milk"]] == 0)
    if(length(milk.rev) > 0){
      cow[["rev.milk"]][milk.rev] <- (cow[["yield"]][milk.rev] - cow[["yield.rdc"]][milk.rev]) * MilkPrice
    }
    # no revenue for discarding milk
    milk.rev2 <- which(cow[["disc.milk"]] != 0)
    if(length(milk.rev2) > 0){
      cow[["rev.milk"]][milk.rev2] <- 0
    }


# Data collection ---------------------------------------------------------

    # collect data for each day
    cowsList[[i]] <- cow
  } # :: end of For loop ::
  # return list of daily data
  cowsList
} # :: end of model ::

# Model replication functions ---------------------------------------------

limS <- 3     # simulation limits for RAM output 
clb <- TRUE   # calibration

# functions for `n_sim` simulations 
MySim1 <- function(i){
  frame <- data.table::rbindlist(mkFrames(X = xPRM))
  frame$.id <- i
  fwrite(frame, paste0("MySimCows_", str_pad(i, 5, pad = "0"), ".csv"))
}

MySim2<- function(nFrames = xPRM$n_sim) {
  if(xPRM$n_sim > limS){
    setwd(xPRM$storeWD)
    setwd("Results")
    if(!clb){
      setwd("Final/Actual")
      t <- format(Sys.time(), "%F %H-%M") ; t <- gsub(" ", "_", t)
      dir.create(file.path(getwd(), t))
      setwd(dir()[dir() == t])
      RAW.dir <- getwd()
    } else{
      setwd("Calibration")
      t <- format(Sys.time(), "%F %H-%M") ; t <- gsub(" ", "_", t)
      dir.create(file.path(getwd(), t))
      setwd(tail(dir(), 1))
    }
  } else{
    dl <- vector(mode = "list", length = as.numeric(nFrames))
  }
  
  set.seed(xPRM$seed)
  # fwrite(as.data.frame(.Random.seed), "seed.csv")
  t. <- format(Sys.time(), "%F %H-%M") ; t. <- gsub(" ", "_", t.)
  save(xPRM, file = paste0("default_Parameters_", t., ".RData"))
  for(i in seq.int(1,nFrames,1)) {
  if(xPRM$n_sim > limS){
    frame <- rbindlist(mkFrames(X = xPRM))
    frame$.id <- i
    fwrite(frame, paste0("MySimCows_", str_pad(i, 5, pad = "0"), ".csv"))
    # rm(frame)
  }
  else{
    print(i)
    dl[[i]]<- rbindlist(beep_on_error(mkFrames(), 7))
  }
  }
  
  if(xPRM$n_sim <= limS){
    rbindlist(dl, idcol = TRUE)
  }
}






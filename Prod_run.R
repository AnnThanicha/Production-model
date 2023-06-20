setwd("D:/Backup/PhD/Modelling/RQ1")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

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

source("internal_functions.R")
source("impMS.R")
xPRM         <- mget(load("default_Parameters.RData", 
                          envir=(NE. <- new.env())), envir=NE.)$xPRM; rm(NE.)  # input parameters in list
daysOpen.mat <- fread("daysOpen_mat.csv")                                      # related to fertility culling decisions

# control herd size, number of simulations and time horizon here
xPRM$n_cows <- 125        # number of simulated cows per time step
xPRM$n_sim  <- 1000        # number of simulations
xPRM$n_yrs  <- 10          # number of simulated years

limS <- xPRM$n_sim - 1    # always ensure that this limit is less than number of simulations so that data is stored locally and not in RAM
clb <- FALSE              # calibration

set.seed(xPRM$seed)
ptm <- proc.time()        # records time before simulation is run
MySim2()                  # run simulation and store data locally
(proc.time() - ptm)[3]

# save parameters in directory
t. <- format(Sys.time(), "%F %H-%M") ; t. <- gsub(" ", "_", t.)
save(xPRM, file = paste0("default_Parameters_", t., ".RData"))

# Directories
RAW.dir <- getwd()
YR6.dir <- "D:/Backup/PhD/Modelling/RQ1/Results/Final/Actual/YEAR6" 
ANA.dir <- "D:/Backup/PhD/Modelling/RQ1/Results/Final/Actual/Analysed_yr6" 


# get data files
setwd(RAW.dir)
data.files <- grep("MySimCows", dir(RAW.dir), value = TRUE)
file.rename(from = data.files,
            to = paste0(rep("MySimCows_", length(data.files)),
                        str_pad(unlist(regmatches(data.files, gregexpr("[[:digit:]]+", data.files))), 5, pad = "0"),
                        rep(".csv", length(data.files))))
Vars <- colnames(fread(data.files[1], nrows = 0))

files.to.read <- list.files(path = getwd(), full.names = TRUE)

pattern = "^[6],"                    #  burn-in period of 5 year. year 6 is first year of stable results.
string.rm <- paste0(RAW.dir, "/")
setwd(YR6.dir)
lapply(files.to.read, function(x){
  print(x)
  y <- fread(cmd = paste0( 'findstr /R "', pattern, '" ',  gsub( "\\/", "\\\\", x )),
        sep = ",",
        header = FALSE)
  colnames(y) <- Vars
  fwrite(y, gsub(string.rm, "", x))
})

# update files to read
files.to.read <- list.files(path = getwd(), full.names = TRUE)

# Start from big and move into detail
setwd(YR6.dir)
data.files <- grep("MySimCows", dir(YR6.dir), value = TRUE)
Vars <- colnames(fread(data.files[1], nrows = 0))

# plotting palettes
palette <- c("#2b2821", "#624c3c", "#d9ac8b", "#e3cfb4", 
             "#243d5c", "#5d7275", "#5c8b93", "#b1a58d", 
             "#b03a48", "#d4804d", "#e0c872", "#3e6958")    #https://lospec.com/palette-list/japanese-woodblock
palette2 <- c("#46a6e8", "#26dc73", "#28d4b1", "#27394c",   #blue green turquoise dark-navy
              "#f04130", "#f1882d", "#f5cc2a", "#32506d")   #red orange yellow light-navy


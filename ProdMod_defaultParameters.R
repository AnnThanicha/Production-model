setwd(paste0("C:/Users/", Sys.info()["user"], "/OneDrive - Wageningen University & Research/PhD/Modelling/ProdMod"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

xPRM <- list()

# Generic inputs ----------------------------------------------------------

xPRM$storeWD <- paste0("C:/Users/", Sys.info()["user"], "/OneDrive - Wageningen University & Research/PhD/Modelling/ProdMod")

xPRM$n_cows <- 125   # number of cows in herd
xPRM$n_sim  <- 0001  # number of simulations 
xPRM$n_yrs  <- 10    # time horizon in years
xPRM$ts     <- 365   # time-steps per year

source("ProdMod_internalFunc.R")


# Production inputs -------------------------------------------------------

xPRM$max_par      <- 10                                   # maximum parity for last lactation (Authors' expertise)
xPRM$use_par      <- 6                                    # expected lactaions of cow - depreciation over useful life mkFrames (Authors expertise)
xPRM$p_par        <- setNames(c(0.31, 0.26, 0.20, 0.12, c(rep(0.11/6, 6))), paste0("par", seq(xPRM$max_par))) # preliminary probabilty of a cow being in 1:max_par (CRV, 2019)
xPRM$p_dtoes      <- 0.55                                 # probability of oestrus detection (Based on Rutten et al., 2014; doi:10.3168/jds.2014-7948) 
xPRM$p_conc       <- c(0.45, 0.42, 0.42, 0.38, 0.33, 0.27) # success of insemination for insemination number (Inchiasri et al., 2011; doi:10.1111/j.1439-0531.2011.01782.x)
xPRM$p_stlbrn     <- 0.0                                  # probability of cow having a still born [preliminary 20/11/19]
xPRM$p_gencull    <- c(0.03, 0.07, 0.07, 0.1, 0.2)        # calibrated so that overall culling rate ~30%  
xPRM$p_mort       <- 0.02                                 # mortality rate of general cull cows (Authors' expertise)
xPRM$p_rh         <- 0.3                                  # calibrated probability that a replacement heifer will enter the herd on the following day that a cow dies (geometric distribution: rgeom())
# xPRM$max_insem    <- 4                                    # culling rule: maximum number of inseminations a farmer will choose to inseminate a cow that is in oestrus (Inactive parameter since 01/01/2020)
xPRM$yield_thresh <- 15                                   # kg/day; infertile cow will be culled if producing below this parameter (Authors' expertise)
xPRM$VWP          <- 84                                   # voluntary waiting period in days before next AI (Inchiasri et al., 2010; doi:10.1016/j.theriogenology.2010.04.008)
xPRM$DPL          <- 56                                   # dry period length in days (Inchiasri et al., 2010; doi:10.1016/j.theriogenology.2010.04.008)
xPRM$ADG          <- ((650-540)/2)/410                    # average daily gain (Based on Kok et al., 2017; https://doi.org/10.1371/journal.pone.0187101)

# milk yield parameters (Kok et al., 2017; https://doi.org/10.1371/journal.pone.0187101) 
# parameter vectors are arranged according to parity order
xPRM$a   <- c(31.6, 40.6, 44.1)
xPRM$b   <- c(-0.0447, -0.0708, -0.0835)
xPRM$d   <- -16.1
xPRM$K   <- 0.06
xPRM$ady <- with(xPRM, 
                 sapply(1:length(a), function(x){mean(a[x] +b[x] * 1:305 + d *exp(-K * 1:305))})) # average daily 305-d milk yield in kg
xPRM$fat  <- c(0.0448, 0.045, 0.0451, 0.0451,  0.0451, 0.0451, 0.0451, 0.0451,  0.0451, 0.0451)  # fat content
xPRM$prot <- c(0.0355, 0.0359, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351, 0.0351)   # protein content

# energy requirements
xPRM$G.ENGY <- c(660, 330)              # growth energy for cows in parity 1 & 2 (van Es, 1978; https://doi.org/10.1016/0301-6226(78)90029-5)
xPRM$P.ENGY <- c(2700, 1500, 850, 450)  # energy during stage of lactation in order of last month, to fourth month before calving (Remelink et al., 2016; table 6.10 Dairy farming handbook)

# Economic inputs ---------------------------------------------------------

xPRM$cost.EkVEM  <- 0.1766363636  # 09-2019 - 06-2020 (WECR, 2020; https://www.wur.nl/nl/Onderzoek-Resultaten/Onderzoeksinstituten/livestock-research/Producten/Voederwaardeprijzen-Rundvee.htm)
xPRM$cost.insem  <- 12.85     # cost per insemination (Blanken et al., 2016; KWIN)

# culling costs
xPRM$cost.rear  <- c(919, 1790, 3307)    # min, mean, max rearing costs (Nor et al., 2015; https://doi.org/10.1186/s13620-015-0058-x)
xPRM$perc.dress <- 0.6                   # carcass dressing percentage (Rutten et al., 2014; doi:10.3168/jds.2014-7948.)
xPRM$price.kg   <- c(2.06, 2.44, 2.77)   # Wageningen Economic Research, 2020. Agricultural Prices. https://agrimatie.nl/Prijzen.aspx?ID=15125. (average 2019-2010)

# Milk revenue
xPRM$MilkPrice <- 0.3502 # Wageningen Economic Research, 2020. Agricultural Prices. https://agrimatie.nl/Prijzen.aspx?ID=15125. c/kg average 2016 - may 2020

xPRM$RemCost <- 39 # Rendac (2015) https://www-cambridge-org.ezproxy.library.wur.nl/core/services/aop-cambridge-core/content/view/7977B21E1B8F78E002AE0A1196CA98D8/S1751731117001306a.pdf/estimating_the_economic_impact_of_subclinical_ketosis_in_dairy_cattle_using_a_dynamic_stochastic_simulation_model.pdf  

xPRM$validate    <- TRUE
xPRM$seed <- 123
xPRM$last.update <- date()


# Save inputs -------------------------------------------------------------
save(xPRM, file = "ProdMod_defaultParameters.RData") #~ *NB* remember to save the input data as an .Rdata file every time you add inputs in this script by running this line of code

# xPRM <- mget(load("default_Parameters.RData", envir=(NE. <- new.env())), envir=NE.)

# sink("default_Parameters.txt")
# print(xPRM)
# sink()

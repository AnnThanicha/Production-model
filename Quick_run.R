setwd(paste0("C:/Users/", Sys.info()["user"], "/OneDrive - Wageningen University & Research/PhD/Modelling/ProdMod"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
source("ProdMod.R")

# control herd size, number of simulations and time horizon here during preliminary runs
xPRM$n_cows <- 5      # number of simulated cows per time step
xPRM$n_sim  <- 1      # number of simulations
xPRM$n_yrs  <- 3

# run one simulation and store in RAM
set.seed(xPRM$seed)
ptm <- proc.time()
MySim_cows <- rbindlist(mkFrames())
proc.time() - ptm #~ time to run model for one simulation


#~ mean lactation curves
MySim_cows %>%
  mutate(par = ifelse(par > 3, ">3", par),
         par = factor(par, levels = c(1, 2, 3, ">3"))) %>%
  group_by(par, dim, year) %>%
  summarise(mu = mean(yield), sigma = sd(yield)) %>%
  ggplot(aes(x = dim, y = mu)) +
  geom_ribbon(aes(ymin = mu - sigma, ymax = mu + sigma, fill = par, alpha = 0.01)) +
  geom_line(aes(colour = par), size = 0.8) +
  facet_grid(cols = vars(par), rows = vars(year))

#~ herd demographic - parity distribution
MySim_cows %>%
  group_by(space, UID, par, year) %>%
  summarise(N = n()) %>%
  group_by(space, year) %>%
  filter(max(N) == N) %>%
  group_by(par, year) %>%
  summarise(share = n()/xPRM$n_cows) %>%
  group_by(par, year) %>%
  summarise(share = mean(share))

MySim_cows %>%
  filter(cull != 0) %>%
  group_by(cull, year) %>%
  summarise(n_culls_per_cat = n())


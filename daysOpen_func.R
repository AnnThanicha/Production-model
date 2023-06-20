daysOpen <- function(){
  library(dplyr)
  library(tidyr)
  
  # Days open acording to Inciasri (2011) 'Cow effects and insemination of success of first and following inseminations in Dutch dairy cows'
  
  tmp.runs <- 3000 # convergence after 3000 runs
  tmp.n_cows <- ifelse(xPRM$n_cows < 100, 100, xPRM$n_cows)
  tmp.list <- vector(mode = "list", length = tmp.runs)
  tmp.mat <- matrix(c(rep(1, tmp.n_cows), 1:tmp.n_cows, rep(0, tmp.n_cows)), nrow = tmp.n_cows); colnames(tmp.mat) <- c("iter", "id", "rpl")
  for(i in 1:tmp.runs){
    tmp.mat[, 1] <- i
    tmp.mat[, 3] <- sort(RPL(c(1:tmp.n_cows)))
    tmp.list[[i]] <- tmp.mat
  }

  tmp.mat2 <- do.call(rbind, tmp.list)
  rm(tmp.list); rm(tmp.mat)
  tmp.var <- matrix(ncol = length(unique(tmp.mat2[, 2])), nrow = tmp.runs); colnames(tmp.var) <- as.character(unique(tmp.mat2[, 2]))

  tmp.mat3 <- data.frame(tmp.mat2) %>%
    select_all() %>%
    group_by(id) %>%
    summarise(mean.rpl = mean(rpl)) %>%
    mutate(days.open = floor(seq(190, 215, length.out = tmp.n_cows)))

  return(tmp.mat3)
  
}

# daysOpen_mat <- daysOpen()
# plot(daysOpen_mat$mean.rpl, daysOpen_mat$days.open)
# write.csv(daysOpen_mat, "daysOpen_mat.csv")

# Data for the simulation of 
# Importance scores of 3 forecasters with the same bias but different dispersion of Forecaster 3
# To see the distribution of importance scores, 
# get the full data before averaging the importance scores obtained per iteration
# Here we get data for s from 0.1 to 2.

library(tidyverse)
source("main-Rcode/build_ensemble.R")
library("stringr")
library(here)
setwd(here())

simu_data_f3dispersion <- function(s){
        set.seed(2022)
        T <- 1000
        Y <- c()
        for (t in 1:T){
                Y[t] <- rnorm(1, 0, 1)
        }
        mu1 <- mu2 <- mu3 <- rep(0, T)
        mu <- cbind(mu1, mu2, mu3)
        sd <- cbind(rep(0.5, T) , rep(0.7, T), rep(s, T))
        
        probs <- c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
        
        df.list = list()
        i = 0
        for (t in 1:T){
                i = i+1
                forecast <- sapply(1:3, function(x) rnorm(100, mean = mu[t, x], sd = sd[t, x])) 
                quantiles <- t(apply(forecast, 2, function(x) quantile(x, probs = probs)))
                df0 <- as.data.frame(quantiles)
                colnames(df0) <- str_c("qt", probs)
                df.list[[i]] <- df0 %>% 
                        dplyr::mutate(model=str_c("forecaster", 1:3),
                               target_date=str_c("week", t)) %>%
                        dplyr::relocate(model, target_date)
        }
        
        df.23qt <- do.call(rbind.data.frame, df.list)
        
        df.23qt.long <- pivot_longer(df.23qt, 3:25, names_to = "quantile")
        models <- df.23qt$model %>% unique()
        target.date <- unique(df.23qt$target_date)
        n.mod <- length(models)
        y.val <- Y
        alpha0 <- 1
        alpha <- c(seq(0.9, 0.1, by=-0.1), 0.05, 0.02)
        K=length(alpha)
        w0=1/2
        w1=alpha/2
        w = c(w0*alpha0/2,w1)*2/c(alpha0,alpha)
        
        mod.impo.list = list()
        k=0
        for (i in target.date){
                print(paste0("s=",s, ", target date = ", i))
                k=k+1
                y = y.val[k]
                df <- build_mean_ensemble(df.23qt.long, i)
                
                df.wis <- as.data.frame(
                        df %>% 
                                dplyr::mutate(dispersion = 1/(K+1/2) * t( w1 %*% t(df[13+c(1:11)]-df[13-c(1:11)]) ),
                                       overpred = 1/(K+1/2) * t( w %*% t( (df[13-c(0:11)] - y)*as.numeric( y <df[13-c(0:11)]) ) ),
                                       underpred = 1/(K+1/2) * t( w %*% t( (y-df[13+c(0:11)])*as.numeric( y > df[13+c(0:11)]) ) ) ) %>%
                                dplyr::mutate(wis = dispersion + overpred + underpred) %>%
                                dplyr::relocate(c(model, wis, dispersion, overpred, underpred), .before = qt0.01)
                )
                
                ens.importance = round( -rep(df.wis[which(df.wis$'model' == 'Ensemble.all'), 2], n.mod) +
                                                df.wis[which(df.wis$'model' == 'Ensemble.all') + 1: + n.mod, 2], 2)
                
                df.wis.importance.all <- df.wis %>%
                        dplyr::mutate(importance = c(ens.importance, rep(NA, n.mod+1))) %>%
                        dplyr::relocate(c(importance), .before = wis) %>% 
                        dplyr::mutate(target_end_date = i, .after = wis)
                
                mod.impo.list[[k]] <- df.wis.importance.all
        }
        
        df.23qt.full <- do.call(rbind.data.frame, mod.impo.list)
        
        truth = rep(Y, rep(3, length(Y)) )
        data <- df.23qt.full %>%
                filter( !grepl('Ens', model) ) %>% 
                dplyr::mutate( t=as.integer(str_remove(target_end_date, "week")) )  %>%
                dplyr::mutate(error = qt0.5-truth, MAE = abs(error)) %>%
                dplyr::mutate(f3_sharpness=s) %>%
                dplyr::select(model, f3_sharpness, importance, wis, dispersion, overpred, underpred, error, MAE, t)
        
        return( data )
        
}

s <- seq(0.1, 3, 0.05)
list.df <- lapply(s, simu_data_f3dispersion)
list.df.23qt <- do.call(rbind, list.df)
print("created a dataset")

write.csv(list.df.23qt, 
          "../Data/simulation_f3sharp.csv", 
          row.names = FALSE)
saveRDS(list.df.23qt, 
        "../Data/simulation_f3sharp.rds")

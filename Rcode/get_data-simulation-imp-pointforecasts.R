# Data for the simulation of Importance scores of 3 point forecasters:
# two fixed forecasts (hat_y1, hat_y2) and one changing forecast (hat_y3)

library(tidyverse)
library("stringr")
library(here)
setwd(here())

simu_data_f3points <- function(b){
        set.seed(2022)
        T <- 1000
        Y <- rnorm(T, 0, 1)
        # Fixed forecasts
        hat_y1 <- rep(-1, T)
        hat_y2 <- rep(-0.5, T)

        # Changing forecast
        hat_y3 <- rep(b, T)
        # combine 3 forecasts into a data frame 
        forecasts <- cbind(hat_y1, hat_y2, hat_y3) 
        df <- data.frame(Y, forecasts) %>%
                mutate(ens_wo_1 = rowMeans(select(., -Y, -hat_y1)),
                       ens_wo_2 = rowMeans(select(., -Y, -hat_y2)),
                       ens_wo_3 = rowMeans(select(., -Y, -hat_y3)),
                       ens_all = rowMeans(select(., -Y))) %>% 
                mutate(err1 = Y-hat_y1,
                       err2 = Y-hat_y2,
                       err3 = Y-hat_y3) %>%
                mutate(phi_1 = (Y-ens_wo_1)^2-(Y-ens_all)^2,
                       phi_2 = (Y-ens_wo_2)^2-(Y-ens_all)^2,
                       phi_3 = (Y-ens_wo_3)^2-(Y-ens_all)^2) 


        # Calculate the importance scores
        Imp_score <- df %>% 
                select(phi_1, phi_2, phi_3) %>% 
                colMeans() %>% 
                as.data.frame() %>% 
                rownames_to_column(var = "Model") %>% 
                mutate(Model=str_replace(Model, 'phi_', 'forecaster')) %>% 
                rename(Importance_score = 2) %>%
                dplyr::mutate(f3_pred=b)

        return( Imp_score )
}


b <- seq(-1, 3, 0.05)
list.df <- lapply(b, simu_data_f3points)
list.df.pts <- do.call(rbind, list.df)

write.csv(list.df.pts, 
          "./Data/simulation_f3pts.csv", 
          row.names = FALSE)
saveRDS(list.df.pts, 
        "./Data/simulation_f3pts.rds")


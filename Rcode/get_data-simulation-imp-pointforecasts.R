# Data for the simulation of Importance scores of 3 point forecasters:
# two fixed forecasts (hat_y1, hat_y2) and one changing forecast (hat_y3)

library(tidyverse)
library("stringr")
library(here)
setwd(here())

b <- 0.5
set.seed(2022)
T <- 1000
Y <- rnorm(T, 0, 1)
# Fixed forecasts
hat_y1 <- rep(-1, T)
hat_y2 <- rep(-0.5, T)

# Changing forecast
hat_y3 <- rep(b, T)

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
        rename(Importance_score = 2) 

# Decomposition 
decomp <- df %>% select(err1, err2, err3) %>% 
        mutate(e1e2 = err1*err2,
               e1e3 = err1*err3,
               e2e3 = err2*err3) %>%
        colMeans() %>% 
        as.data.frame() %>% 
        rownames_to_column(var = "component") %>% 
        mutate(component=str_replace(component, 'err', 'MSPE')) %>% 
        rename(value = 2) 

n=3
e_phi1=-decomp[1,2]/n^2+ (2*n-1)*(decomp[2,2]+decomp[3,2])/(n^2*(n-1)^2) -2*(decomp[4,2]+decomp[5,2])/(n^2) +
        2*(2*n-1)*(decomp[6,2])/(n^2*(n-1)^2)
e_phi2=-decomp[2,2]/n^2+ (2*n-1)*(decomp[1,2]+decomp[3,2])/(n^2*(n-1)^2) -2*(decomp[4,2]+decomp[6,2])/(n^2) +
        2*(2*n-1)*(decomp[5,2])/(n^2*(n-1)^2)
e_phi3=-decomp[3,2]/n^2+ (2*n-1)*(decomp[1,2]+decomp[2,2])/(n^2*(n-1)^2) -2*(decomp[5,2]*decomp[6,2])/(n^2) +
        2*(2*n-1)*(decomp[4,2])/(n^2*(n-1)^2)
Imp_score_by_component <- data.frame(Model = c("forecaster1", "forecaster2", "forecaster3"),
                                     Importance_score = c(e_phi1, e_phi2, e_phi3))



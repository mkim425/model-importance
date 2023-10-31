## Calculate Importance score of a model using Shapley value

# @param forecast_data 'data.frame' containing all the forecasts of 
# a certain combination of location, forecast date, and horizon
# @param truth require data.frame
# @param ensemble_method method to create quantile ensemble, 
# either `median` (default) or `mean`.

# Note: I used 'use_median_as_point = TRUE' in score_forecasts when calculating WIS

library(covidData)
library(covidHubUtils)
library(hubEnsembles)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(lubridate)
library(furrr)
future::plan(multisession)


importance_score <- function(forecast_data, truth, ensemble_method){
        # Get model names in the dataset
        model_name <- forecast_data$model %>% unique() 
        n <- length(model_name)
        
        # Power set of {1,2,...,n} not including the empty set. 
        # We use this power set to get indices for subset of models
        subsets <- lapply(1:n, function(x) combn(n, x, simplify = F)) %>% 
                unlist(recursive = F) 
        
        dat_allens <- purrr::map_dfr(
                subsets,
                function(subset){
                        get_modelsubset <- model_name[subset]
                        i <- Position(function(x) identical(x, subset), subsets)
                        # calculate the weight given to this subset
                        weight <- 1/( (n-1)*choose(n-1, length(get_modelsubset)) )
                        
                        # WIS calculation for Ensemble from the subset S
                        data_subset <- forecast_data %>% 
                                filter(model %in% get_modelsubset)
                        
                        # Make forecast date to ensemble to Monday, if the forecast dates are not aligned to a common reference date.
                        # ens_forecast_date <- floor_date(max(forecast_data$forecast_date)+3, "week", 1)
                        ens_forecast_date <- data_subset$forecast_date %>% unique()
                        
                        ensemble_forecast <- build_quantile_ensemble(
                                data_subset,
                                method = ensemble_method,
                                forecast_date = ens_forecast_date,
                                model_name = paste0("ensemble_", i),  # ensemble model name
                                location_data = hub_locations)
                        
                        ens_dat <- ensemble_forecast %>% 
                                mutate(subset_idx=i, subset_weight = weight)
                        return(ens_dat)
                }) 
        
        idx_wt <- dat_allens %>% select(model, subset_idx, subset_weight) %>% unique()
        
        idx_wis <- dat_allens %>% 
                group_by(subset_idx) %>% 
                score_forecasts_wis(
                        truth %>%
                                filter(target_end_date == unique(forecast_data$target_end_date)),
                        metrics = "wis",
                        use_median_as_point = TRUE
                        ) %>% 
                select(model, wis)
        
        df <- left_join(idx_wt, idx_wis, by="model") %>% 
                select(subset_idx, wis, subset_weight) %>% 
                rename(subset_ensembleWIS=wis)
        # ------------------------------------------------------------------------------
        
        result <- NULL
        
        for (j in 1:n){
                # find subsets of indices including element j
                set_incl_j <- which(sapply(subsets, function(x) j %in% x))
                # find subsets of indices including more elements in addition to j
                set_incl_j_more <- set_incl_j[set_incl_j > n]
                # Shapley value calculation for the jth model
                score = 0
                for (k in set_incl_j_more){
                        set_k <- subsets[[k]]
                        k1 <- which(sapply(subsets, setequal, set_k[set_k !=j]))
                        marginal_contribution <- df$subset_ensembleWIS[k] - df$subset_ensembleWIS[k1]
                        score <- score - df$subset_weight[k1] * marginal_contribution
                }
                result <- rbind(result, data.frame(Model = model_name[j], 
                                                   Importance_score = score))
        }
        return(result)
}

## Calculate Importance score of a model using leave all subsets of models out algorithm
## This is to calculate the importance scores for single locations 
## where models in the training set is not the same as the model in the current set,
## which results in NA value.

# @param forecast_data 'data.frame' containing all the forecasts of 
# a certain combination of location, forecast date, and horizon
# @param truth require data.frame
# either `median` (default) or `mean`.

# Note: I used 'use_median_as_point = TRUE' in score_forecasts when calculating WIS

library(covidData)
library(covidHubUtils)
library(hubEnsembles)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(lubridate)

importance_score_shapley_trained_per_loc <- function(forecast_data, truth){
        # Get model names in the dataset
        current_forecasts_date <- max(forecast_data$forecast_date)
      
        models_in_train <- forecast_data %>% 
                dplyr::filter(forecast_date < current_forecasts_date) %>% 
                select(model) %>% pull() %>% unique()
        
        models_in_current <- forecast_data %>% 
                dplyr::filter(forecast_date == current_forecasts_date) %>% 
                select(model) %>% pull() %>% unique()
        # include models included in both the training data set and the current forecast data set
        incl_models <- intersect(models_in_train, models_in_current)
        
        forecast_data_reduced <- forecast_data %>% 
                dplyr::filter(model %in% incl_models)
        
        model_name <- forecast_data_reduced$model %>% unique() 
        n <- length(model_name)
        # Power set of {1,2,...,n} not including the empty set. 
        # We use this power set to get indices for subset of models
        subsets <- lapply(1:n, function(x) combn(n, x, simplify = F)) %>% 
                unlist(recursive = F) 
        
        # ---------------- new approach to resolve the issue that generate NA wrongly with the above df code
        dat_all_ens <- purrr::map_dfr(
                subsets,
                function(subset){
                        get_modelsubset <- model_name[subset]
                        i <- Position(function(x) identical(x, subset), subsets)
                        # calculate the weight given to this subset
                        weight <- 1/( (n-1)*choose(n-1, length(get_modelsubset)) )
                        
                        # WIS calculation for Ensemble from the subset S
                        data_subset <- forecast_data_reduced %>% 
                                filter(model %in% get_modelsubset)
                        
                        
                        # training set that will be used for estimating model weights: use prior 12 weeks  
                        train_forecasts <- data_subset %>%
                                dplyr::filter(forecast_date < current_forecasts_date)
                        
                        # a set of forecasts at the "current time”, which will be used to create prospective forecasts.
                        current_forecasts <- data_subset %>%
                                dplyr::filter(forecast_date == current_forecasts_date) 
                        
                        set.seed(2023)
                        qens_fit <- qens(predictions = train_forecasts,
                                         y = truth,
                                         model_id_vars = "model",
                                         task_id_vars = c("forecast_date", "target_end_date", "location"),
                                         tau_var = "quantile",
                                         q_var = "value",
                                         combine_method = "mean",
                                         weighted = TRUE)
                        
                        model_weights <- qens_fit$model_id %>%
                                dplyr::mutate(weight = qens_fit$weights[1, ])
                        
                        weighted_qens <- current_forecasts %>%
                                dplyr::left_join(model_weights, by = "model") %>%
                                dplyr::group_by(forecast_date, target_end_date, location, quantile) %>%
                                dplyr::summarize(value = weighted.mean(value, w = weight),
                                                 .groups = "drop") %>%
                                dplyr::mutate(model = paste0("weighted mean ensemble_", i),
                                              horizon = unique(current_forecasts$horizon),
                                              temporal_resolution = unique(current_forecasts$temporal_resolution),
                                              target_variable = unique(current_forecasts$target_variable),
                                              type = unique(current_forecasts$type))
                        
                        weighted_ens_dat <- weighted_qens %>% 
                                mutate(subset_idx=i, subset_weight = weight)
                        return(weighted_ens_dat)
                }) 
        
        idx_wt <- dat_all_ens %>% select(model, location, subset_idx, subset_weight) %>% unique()
        
        idx_wis <- dat_all_ens %>% 
                score_forecasts_wis(
                        truth %>%
                                filter(target_end_date == unique(dat_all_ens$target_end_date)),
                        metrics = "wis",
                        use_median_as_point = TRUE
                ) %>% 
                select(model, location, wis)
        
        df <- left_join(idx_wt, idx_wis, by=c("model", "location")) %>% 
                # select(subset_idx, location, wis, subset_weight) %>% 
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
                                                   location = unique(df$location),
                                                   Importance_score = score))
        }
        return(result)
}




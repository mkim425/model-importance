## Calculate Importance score of a component model using Leave one model out algorithm

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


importance_score_lomo_trained <- function(forecast_data, truth, ensemble_method){
        current_forecasts_date <- max(forecast_data$forecast_date)
        
        models_in_train <- forecast_data %>% 
                dplyr::filter(forecast_date < current_forecasts_date) %>% 
                select(model) %>% pull() %>% unique()
        
        models_in_current <- forecast_data %>% 
                dplyr::filter(forecast_date == current_forecasts_date) %>% 
                select(model) %>% pull() %>% unique()
        
        # include models included in both the training data set and the current forecast data set
        incl_models <- intersect(models_in_train, models_in_current)
        
        # include locations that have all the 'incl_models'
        loc_list <- forecast_data %>%
                filter(forecast_date == current_forecasts_date) %>%
                group_by(location) %>%
                group_split() %>%
                purrr::map(
                        .,
                        function(df){
                                # models in this location
                                mods <- df %>% select(model) %>% pull() %>% unique()
                                if(setequal(mods, incl_models)){
                                        loc <- df %>% select(location) %>% unique() %>% pull
                                } else{
                                        loc <- NA
                                }
                                return(loc)
                        }) %>%
                unlist() 
        incl_locs <- loc_list[!is.na(loc_list)]
        
        # Reduced data that have only the models included in both 
        # training data set and the current forecast data set
        forecast_data_reduced <- forecast_data %>% 
                dplyr::filter(model %in% incl_models) %>% 
                dplyr::filter(location %in% incl_locs)
        
        model_name <- forecast_data_reduced$model %>% unique() 
        n <- length(model_name)
        
        train_forecasts <- forecast_data_reduced %>%
                dplyr::filter(forecast_date < current_forecasts_date)
        
        # a set of forecasts at the "current time”, which will be used to create prospective forecasts.
        current_forecasts <- forecast_data_reduced %>%
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
        
        
        weighted_qens_all <- current_forecasts %>%
                dplyr::left_join(model_weights, by = "model") %>%
                dplyr::group_by(forecast_date, target_end_date, location, quantile) %>%
                dplyr::summarize(value = weighted.mean(value, w = weight),
                                 .groups = "drop") %>%
                dplyr::mutate(model = paste0("weighted_mean_ensemble_all"),
                              horizon = unique(current_forecasts$horizon),
                              temporal_resolution = unique(current_forecasts$temporal_resolution),
                              target_variable = unique(current_forecasts$target_variable),
                              type = unique(current_forecasts$type))
        
        
        # Build ensemble forecasts by leaving one model out
        dat_ens <- weighted_qens_all
        for (i in 1:length(model_name)){
                # training set without the ith model
                train_set <- train_forecasts %>%
                        filter(model != model_name[i])
                current_set <- current_forecasts %>%
                        filter(model != model_name[i])
                
                set.seed(2023)
                qens_fit <- qens(predictions = train_set,
                                 y = truth,
                                 model_id_vars = "model",
                                 task_id_vars = c("forecast_date", "target_end_date", "location"),
                                 tau_var = "quantile",
                                 q_var = "value",
                                 combine_method = "mean",
                                 weighted = TRUE)
                
                model_weights <- qens_fit$model_id %>%
                        dplyr::mutate(weight = qens_fit$weights[1, ])
                
                
                weighted_qens_lomo <- current_set %>%
                        dplyr::left_join(model_weights, by = "model") %>%
                        dplyr::group_by(forecast_date, target_end_date, location, quantile) %>%
                        dplyr::summarize(value = weighted.mean(value, w = weight),
                                         .groups = "drop") %>%
                        dplyr::mutate(model = paste0("weighted_mean_ensemble.wo.", model_name[i]),
                                      horizon = unique(current_forecasts$horizon),
                                      temporal_resolution = unique(current_forecasts$temporal_resolution),
                                      target_variable = unique(current_forecasts$target_variable),
                                      type = unique(current_forecasts$type))
                dat_ens <- rbind(dat_ens, weighted_qens_lomo)
        }
        
        # Calculate WIS of each ensemble forecasts by location
        wis_ens <- dat_ens %>% 
                group_by(model, location) %>% 
                score_forecasts_wis(truth %>%
                                            filter(target_end_date == unique(dat_ens$target_end_date)),
                                    metrics = "wis",
                                    use_median_as_point = TRUE) %>%
                select(model, location, wis)
        
        
        wis_by_loc <- wis_ens %>%
                group_by(location) %>%
                group_split()
        
        result_list <- purrr::map_dfr(
                wis_by_loc,
                function(df){
                        result <- df %>% 
                                mutate(Importance_score = wis - df[df$model=="weighted_mean_ensemble_all", ]$wis) %>% 
                                filter(model != "weighted_mean_ensemble_all") %>% 
                                mutate(Model = str_remove(model, "weighted_mean_ensemble.wo."),
                                       location = unique(df$location)) %>% 
                                select(Model, location, Importance_score)
                        return(result)
                })
        return(result_list)
}

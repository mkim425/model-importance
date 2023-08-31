## Calculate Importance score of a model using Leave one model out algorithm

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


importance_score_lomo <- function(forecast_data, truth, ensemble_method){
        model_name <- forecast_data$model %>% unique()
        ens_forecast_date <- forecast_data$forecast_date %>% unique()
        ensemble_method = "mean"
        
        # Ensemble forecasts constructed with all possible models
        ensemble_allmodels <- build_quantile_ensemble(
                forecast_data = forecast_data,
                method = ensemble_method,
                forecast_date = ens_forecast_date,
                model_name = paste0("Ensemble_all"),  # ensemble model name
                location_data = hub_locations)  
        
        # Build ensemble forecasts by leaving one model out
        dat_ens <- ensemble_allmodels
        for (i in 1:length(model_name)){
                # Ensemble forecasts constructed with models except ith model
                ensemble_lomo <- build_quantile_ensemble(
                        forecast_data = forecast_data %>%
                                filter(model != model_name[i]),
                        method = ensemble_method,
                        forecast_date = ens_forecast_date,
                        model_name = paste0("Ens.wo.", model_name[i]),  # ensemble model name
                        location_data = hub_locations)
                dat_ens <- rbind(dat_ens, ensemble_lomo)
        }
        
        # Calculate WIS of each ensemble forecast
        wis_ens <- dat_ens %>% 
                score_forecasts_wis(truth %>%
                                            filter(target_end_date == unique(forecast_data$target_end_date)),
                                    metrics = "wis",
                                    use_median_as_point = TRUE) %>%
                select(model, wis)
        
        result <- wis_ens %>% 
                mutate(Importance_score = wis - wis_ens[wis_ens$model=="Ensemble_all", ]$wis) %>% 
                filter(model != "Ensemble_all") %>% 
                mutate(Model = str_remove(model, "Ens.wo.")) %>% 
                select(Model, Importance_score)
        return(result)
}
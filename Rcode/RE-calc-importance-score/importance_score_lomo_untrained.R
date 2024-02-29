## Calculate Importance scores of component models using Leave one model out algorithm

# Note: I used 'use_median_as_point = TRUE' in score_forecasts when calculating WIS

library(covidHubUtils)
library(hubEnsembles)
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(lubridate)


importance_score_lomo <- function(forecast_data, truth){
        # Get a valid `model_out_tbl` object, the hubverse forecast format
        valid_tbl <- forecast_data %>% 
                select(-"forecast_date") %>%
                rename(model_id = model,
                       output_type = type,
                       output_type_id = quantile) %>%
                # Convert model output to a `model_out_tbl` class object
                hubUtils::as_model_out_tbl() %>%
                # Validate a `model_out_tbl` object
                hubUtils::validate_model_out_tbl()
        
       # Ensemble forecasts constructed with all possible models by task_id
        ensemble_allmodels <- hubEnsembles::simple_ensemble(valid_tbl) %>%
                mutate(model_id = "Ensemble.all")
        
        dat_ens <- ensemble_allmodels
        
        # Build ensemble forecasts by leaving one model out
        model_name <- valid_tbl$model_id %>% unique()
        for (i in 1:length(model_name)){
                # Ensemble forecasts constructed with models except ith model
                ensemble_lomo <- hubEnsembles::simple_ensemble(
                        valid_tbl %>%
                                filter(model_id != model_name[i])
                        ) %>%
                        mutate(model_id = paste0("Ens.wo.", model_name[i]))
                dat_ens <- rbind(dat_ens, ensemble_lomo)
        }
        
        # Calculate WIS of each ensemble forecast
        ## convert to the format supported by the scoring functions in covidhubutils
        joint_df <- dplyr::left_join(
                        x = dat_ens, 
                        y = truth,
                        by = c("location", "target_variable", "target_end_date") 
                ) %>%
                dplyr::select(-c("model")) %>%
                dplyr::rename(model = model_id, 
                              prediction = value.x, 
                              true_value = value.y) %>%
                dplyr::filter(!is.na(true_value)) %>% 
                rename(type = output_type,
                       quantile = output_type_id) 
       
        # score using scoringutil
        observation_cols <- c(
                "model",
                "location",
                "horizon", "temporal_resolution", "target_variable",
                "reference_date", "target_end_date" 
        )
        
        wis_ens <- scoringutils::score(data = joint_df) %>% 
                scoringutils::summarise_scores(
                        by = c(dplyr::all_of(observation_cols), "range")
                        ) %>%
                tidyr::pivot_wider(
                        id_cols = dplyr::all_of(observation_cols),
                        names_from = c("range"),
                        values_from = c("interval_score", "dispersion", 
                                        "overprediction", "underprediction")
                ) %>%
                ## Remove columns ending with NA to not affect WIS calculations
                dplyr::select(
                        -dplyr::ends_with("_NA")
                ) %>%
                dplyr::mutate(
                        n_interval_scores = 
                                rowSums(!is.na(
                                        dplyr::select(., 
                                                      dplyr::starts_with(
                                                              "interval_score"
                                                              )
                                                      )
                                                )
                                        ),
                        exists_interval_score_0 = 
                                "interval_score_0" %in% names(.),
                        interval_score_0 = 
                                ifelse(exists_interval_score_0, 
                                       0.5 * interval_score_0, 
                                       NA_real_),
                ) %>%
                dplyr::mutate(
                        wis = rowSums(
                                dplyr::select(
                                        ., 
                                        dplyr::starts_with("interval_score")
                                        ), 
                                na.rm = FALSE) 
                        / (n_interval_scores - 0.5 * (exists_interval_score_0)),
                ) 
        
        result <- wis_ens %>% 
                mutate(Importance_score = wis - wis_ens[wis_ens$model=="Ensemble.all", ]$wis) %>% 
                filter(model != "Ensemble.all") %>% 
                mutate(Model = str_remove(model, "Ens.wo.")) %>% 
                select(Model, location, Importance_score, horizon, target_end_date)
        return(result)
}

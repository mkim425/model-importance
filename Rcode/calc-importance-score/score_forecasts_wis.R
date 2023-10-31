## Calculate WIS 
## source: https://github.com/reichlab/covidHubUtils/blob/master/R/score_forecasts.R


score_forecasts_wis <- function(
                forecasts,
                truth,
                return_format = "wide",
                metrics = "wis",
                use_median_as_point = FALSE) {
        
        # forecasts data.frame format
        # columns: model, forecast_date, location, horizon, temporal_resolution,
        #          target_variable, target_end_date, type, quantile, value
        forecasts_colnames <- c(
                "model", "forecast_date", "location", "horizon", "temporal_resolution",
                "target_variable", "target_end_date", "type", "quantile", "value"
        )
        
        
        # get dataframe into scoringutil format
        joint_df <- dplyr::left_join(
                x = forecasts, y = truth,
                by = c("location", "target_variable", "target_end_date")
        ) %>%
                dplyr::select(-c("model.y")) %>%
                dplyr::rename(model = model.x, prediction = value.x, true_value = value.y) %>%
                dplyr::filter(!is.na(true_value))
        
        # score using scoringutil
        observation_cols <- c(
                "model",
                "location",
                "horizon", "temporal_resolution", "target_variable",
                "forecast_date", "target_end_date" 
        )
        
        
        # column names always included in final output
        col_names_include <- c("model", "location", "horizon", "temporal_resolution",
                               "target_variable", "forecast_date", "target_end_date")
        
        scores <- scoringutils::score(
                data = joint_df) %>% 
                scoringutils::summarise_scores(by = c(all_of(observation_cols), "range")) %>%
                tidyr::pivot_wider(
                        id_cols = all_of(observation_cols),
                        names_from = c("range"),
                        values_from = c("interval_score", "dispersion", "overprediction", "underprediction")
                ) %>%
                ## need to remove all columns ending with NA to not affect WIS calculations
                dplyr::select(
                        -dplyr::ends_with("_NA")
                ) %>%
                dplyr::mutate(
                        n_interval_scores = rowSums(!is.na(dplyr::select(., dplyr::starts_with("interval_score")))),
                        exists_interval_score_0 = "interval_score_0" %in% names(.),
                        interval_score_0 = ifelse(exists_interval_score_0, 0.5 * interval_score_0, NA_real_),
                        ) %>%
                        dplyr::mutate(
                                wis = rowSums(dplyr::select(., dplyr::starts_with("interval_score")), na.rm = FALSE) / (n_interval_scores - 0.5 * (exists_interval_score_0)),
                        ) 
        return(scores)
}

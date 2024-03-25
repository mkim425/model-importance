## Calculate Importance score of a model using Shapley value

# forecast_data 'data.frame' containing all the forecasts of
# a certain combination of location, forecast date, and horizon
# truth require data.frame

library(covidHubUtils)
library(hubEnsembles)
suppressPackageStartupMessages(library(dplyr))
library(magrittr)
library(lubridate)
library(furrr)
future::plan(multisession)


importance_score_lasomo <- function(forecast_data, truth) {
  # Get a valid `model_out_tbl` object, the hubverse forecast format
  valid_tbl <- forecast_data %>%
    rename(
      model_id = model,
      output_type = type,
      output_type_id = quantile
    ) %>%
    # Convert model output to a `model_out_tbl` class object
    hubUtils::as_model_out_tbl() %>%
    # Validate a `model_out_tbl` object
    hubUtils::validate_model_out_tbl() %>%
    select(-any_of("forecast_date"))

  # Get model names in the dataset
  model_name <- valid_tbl$model_id %>% unique()
  n <- length(model_name)

  # Power set of {1,2,...,n} not including the empty set.
  # We use this power set to get indices for subset of models
  subsets <- lapply(1:n, function(x) combn(n, x, simplify = F)) %>%
    unlist(recursive = F)

  # make data frame of all possible ensemble forecasts
  dat_allens <- purrr::map_dfr(
    subsets,
    function(subset) {
      get_modelsubset <- model_name[subset]
      # index of the subsets list that is identical to the current subset, S
      i <- Position(function(x) identical(x, subset), subsets)
      # calculate the weight given to this subset
      weight <- 1 / ((n - 1) * choose(n - 1, length(get_modelsubset)))

      # reduced data including the models in the subset S
      data_subset <- valid_tbl %>%
        filter(model_id %in% get_modelsubset)
      # build an ensemble forecast using the models in the subset S
      ensemble_forecast <- hubEnsembles::simple_ensemble(data_subset) %>%
        mutate(model_id = paste0("ensemble_", i))
      # add index and weight to the ensemble forecast
      ens_dat <- ensemble_forecast %>%
        mutate(subset_idx = i, subset_weight = weight)
      return(ens_dat)
    }
  )

  ## convert to the format supported by the scoring functions in covidhubutils
  joint_df <- dplyr::left_join(
    x = dat_allens,
    y = truth,
    by = c("location", "target_variable", "target_end_date")
  ) %>%
    dplyr::select(-c("model")) %>%
    dplyr::rename(
      model = model_id,
      prediction = value.x,
      true_value = value.y
    ) %>%
    dplyr::filter(!is.na(true_value)) %>%
    rename(
      type = output_type,
      quantile = output_type_id
    )
  # columns for the index and weight of each ensemble forecast
  idx_wt <- joint_df %>%
    select(model, subset_idx, subset_weight) %>%
    unique()

  # score using scoringutil
  observation_cols <- c(
    "model", "location", "horizon", "temporal_resolution",
    "target_variable", "reference_date", "target_end_date"
  )

  idx_wis <- joint_df %>%
    group_by(model) %>%
    scoringutils::as_forecast(
      observed = "true_value",
      predicted = "prediction",
      quantile_level = "quantile"
    ) %>%
    scoringutils::score() %>%
    select(all_of(observation_cols), "wis") %>%
    ungroup()

  df <- left_join(idx_wt, idx_wis, by = "model") %>%
    select(subset_idx, wis, subset_weight) %>%
    rename(subset_ensembleWIS = wis)
  # ---------------------------------------------------------------------------

  result <- NULL

  for (j in 1:n) {
    # find subsets of indices including element j
    set_incl_j <- which(sapply(subsets, function(x) j %in% x))
    # find subsets of indices including more elements in addition to j
    set_incl_j_more <- set_incl_j[set_incl_j > n]
    # Shapley value calculation for the jth model
    score <- 0
    for (k in set_incl_j_more) {
      set_k <- subsets[[k]]
      k1 <- which(sapply(subsets, setequal, set_k[set_k != j]))
      marginal_contribution <- df$subset_ensembleWIS[k] - df$subset_ensembleWIS[k1]
      score <- score - df$subset_weight[k1] * marginal_contribution
    }
    result <- rbind(result, data.frame(
      Model = model_name[j],
      Importance_score = score
    ))
  }
  result <- result %>%
    mutate(
            reference_date = unique(valid_tbl$reference_date), 
            location = unique(valid_tbl$location), 
            horizon = unique(valid_tbl$horizon), 
            target_end_date = unique(valid_tbl$target_end_date)
    )
  return(result)
}

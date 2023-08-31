library(covidHubUtils)

incl_locations <- covidData::fips_codes %>%
        dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
        dplyr::pull(location)


truth <- load_truth(
        truth_source = "JHU",
        target_variable = "inc death",
        locations = incl_locations
)

saveRDS(truth, "~/Documents/Dissertation/Ch1-model_importance/model-importance/data/leave-all-subsets-of-models-out/truth_data_upto2022.rds")
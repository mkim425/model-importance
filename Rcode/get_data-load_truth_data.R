library(covidHubUtils)
library(here)
setwd(here())

incl_locations <- covidData::fips_codes %>%
        dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
        dplyr::pull(location)


truth <- load_truth(
        truth_source = "JHU",
        target_variable = "inc death",
        locations = incl_locations
)

saveRDS(truth, "../Data/truth_data_upto2022.rds")
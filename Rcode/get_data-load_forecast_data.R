# Load forecast data

library(covidData)
library(covidHubUtils)
library(here)
setwd(here())

dates <- seq.Date(from = as.Date("2020-06-01"), 
                  to = as.Date("2022-11-28"),
                  by = 7) %>%
        as.character()

incl_locations <- covidData::fips_codes %>%
        dplyr::filter(nchar(location) == 2, location <= "56", location != "11") %>%
        dplyr::pull(location)

incl_models <- c("BPagano-RtDriven", "COVIDhub-baseline", "CU-select",
                 "GT-DeepCOVID", "Karlen-pypm", "MOBS-GLEAM_COVID",
                 "PSI-DRAFT", "RobertWalraven-ESG", "UCSD_NEU-DeepGLEAM",
                 "USC-SI_kJalpha")
death_targets <- paste(1:4, "wk ahead inc death")

forecast_data <- load_forecasts(
        models = incl_models,
        dates = dates,
        date_window_size = 6,
        locations = incl_locations,
        types = "quantile",
        targets = death_targets,
        source = "zoltar",
        verbose = FALSE
)


d <- forecast_data

colnames <- c("model", "forecast_date", "reference_date", "location", "horizon",
              "relative_horizon", "temporal_resolution", "target_variable", "target_end_date", "type",
              "quantile", "value")

forecast_data_aligned <- align_forecasts(d) %>% select(all_of(colnames))
saveRDS(forecast_data_aligned, "../Data/forecast_data_aligned_death_10models_june20-nov22.rds")


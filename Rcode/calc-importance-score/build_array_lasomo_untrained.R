## Build 2-dimensional array (forecast/reference date, location)

# dat is forecast data 'data.frame' containing all the forecasts of
# a certain combination of location, forecast date, and horizon
# truth require data.frame

build_array_lasomo <- function(dat, truth) {
  # we use reference date, so remove forecast_date column
  dat <- dat %>% select(-any_of("forecast_date"))
  # make location a character
  truth <- truth %>% mutate(location = as.character(location))
  # get unique dates, horizons, and locations
  dates <- dat$reference_date %>%
    unique() %>%
    as.character()
  h <- dat$horizon %>% unique()
  incl_locations <- truth$location %>% unique()

  models <- c(
    "BPagano-RtDriven", "COVIDhub-baseline", "CU-select",
    "GT-DeepCOVID", "Karlen-pypm", "MOBS-GLEAM_COVID",
    "PSI-DRAFT", "RobertWalraven-ESG", "UCSD_NEU-DeepGLEAM",
    "USC-SI_kJalpha"
  )

  # create empty array
  arr <- array(NA_real_,
    dim = c(length(dates), length(incl_locations)),
    dimnames = list(
      Time = dates,
      Location = incl_locations
    )
  )

  arr_list <- lapply(models, function(x) arr)
  names(arr_list) <- models

  # split the data by location
  dat_gp <- dat %>%
    group_by(reference_date, location) %>%
    group_split()
  # calculate importance score in LOMO for multiple locations at once
  out <- dat_gp %>%
    purrr::map_dfr(~ importance_score_lasomo(.x, truth = truth))

  # assign the importance scores to the array
  for (t in dates) {
    # print(t)
    for (l in incl_locations) {
      # print(l)
      for (i in models) {
        score <- out %>%
          filter(reference_date == t, location == l, Model == i) %>%
          select(Importance_score) %>%
          pull()
        if (length(score) == 0) {
          arr_list[[i]][t, l] <- NA
        } else {
          arr_list[[i]][t, l] <- score
        }
      }
    }
  }

  return(arr_list)
}

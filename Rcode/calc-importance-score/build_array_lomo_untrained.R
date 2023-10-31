## Build 2-dimensional array (forecast date, location)

# @param dat is forecast data 'data.frame' containing all the forecasts of 
# a certain combination of location, forecast date, and horizon
# @param truth require data.frame

build_array <- function(dat, truth){
        # if forecast dates are not aligned: 
        # dates <- dat$forecast_date[which(weekdays(dat$forecast_date) == "Monday")] %>% unique()
        dates <- dat$forecast_date %>% unique() %>% as.character()
        h <- dat$horizon %>% unique() 
        incl_locations <- dat$location %>% unique()
        models <- c("BPagano-RtDriven", "COVIDhub-baseline", "CU-select",
                    "GT-DeepCOVID", "Karlen-pypm", "MOBS-GLEAM_COVID",
                    "PSI-DRAFT", "RobertWalraven-ESG", "UCSD_NEU-DeepGLEAM",
                    "USC-SI_kJalpha") 
        
        # create empty array 
        arr <- array(NA_real_, dim=c(length(dates), length(incl_locations)),
                     dimnames = list(Time = dates,
                                     Location = incl_locations))
        
        arr_list <- lapply(models, function(x) arr)
        names(arr_list) <- models
        
        
        dat_gp <- dat %>%
                # if forecast dates are not aligned:
                # group_by(target_end_date, location) %>%
                group_by(location) %>%
                group_split()

        out <- dat_gp %>%
                purrr::map(importance_score_lomo, truth=truth, ensemble_method="mean")


        for (t in 1:length(dates)){
                # print(dates[t])

                for (l in 1:length(incl_locations)){
                        # print(incl_locations[l])

                        for(i in 1:length(models)){
                                if (! models[i] %in% out[[(t-1)*length(incl_locations)+l]]$Model){
                                        arr_list[[i]][t, l] <- NA
                                } else{
                                        score <- out[(t-1)*length(incl_locations)+l] %>%
                                                as.data.frame() %>%
                                                filter(Model == models[i] ) %>%
                                                select(Importance_score) %>% pull()
                                        arr_list[[i]][t, l] <- score
                                }
                        }
                }
        }
        
        saveRDS(arr_list, "../output/array-horizon",h,"-",dates,".rds")
} 




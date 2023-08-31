# Build 3-dimensional array (forecast date, location)

build_array_shapley_trained <- function(dat, truth){
        dates <- dat$forecast_date %>% max() %>% as.character()
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
        
        ## calculate importance score shapley trained for multiple locations at once
        # result_list <- importance_score_shapley_trained(dat, truth=truth)
        
        ## calculate importance score shapley trained for single location
        result_list <- importance_score_shapley_trained_per_loc(dat, truth)
        
        ## get the locations in the result_list
        result_locations <- result_list %>% select(location) %>% unique() %>% pull()

        out <- result_list %>% 
                group_by(location) %>%
                group_split()

        
        for (t in 1:length(dates)){
                for (l in 1:length(result_locations)){
                        loc_number <- result_locations[l]
                        
                        for(i in 1:length(models)){
                                if (! models[i] %in% out[[(t-1)*length(result_locations)+l]]$Model){
                                        arr_list[[i]][t, loc_number] <- NA
                                } else{
                                        score <- out[[(t-1)*length(result_locations)+l]] %>%
                                                filter(Model == models[i] ) %>%
                                                select(Importance_score) %>% pull()
                                        arr_list[[i]][t, loc_number] <- score
                                }
                        }
                }
        }
        
        # path <- "~/Documents/Dissertation/Ch1-model_importance/model-importance/unity-Shapley-trained"
        path <- "/work/pi_nick_umass_edu/minsu_project"
        # saveRDS(arr_list, paste0(path, "/output/array-horizon",h,"-",dates,".rds"))
        
        ## when calculating scores for single location
        saveRDS(arr_list, paste0(path, "/output/array-loc",incl_locations,"-h",h,"-",dates,".rds"))
} 




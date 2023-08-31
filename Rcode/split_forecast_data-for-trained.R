suppressPackageStartupMessages(library(tidyverse))
library(dplyr)
library(magrittr)
library(lubridate)

base_path <- "~/Documents/Dissertation/Ch1-model_importance/model-importance"
# Read data
forecast_data <- readRDS(paste0(base_path,"/data/leave-all-subsets-of-models-out/forecast_data_aligned_death_10models_june20-nov22.rds")) %>% 
        select(c(1, 3:5, 7:12)) %>% 
        rename(forecast_date = reference_date) %>% # Change the col name for convenient calculation using importance score function
        arrange(forecast_date, model)

# list of forecast dates
current_forecasts_dates <- forecast_data %>% 
        filter(forecast_date >= "2020-10-31") %>% 
        select(forecast_date) %>% 
        pull() %>% unique()

# split the whole data set by horizon and forecast date
for (h in 1:4){
        for (i in 1:length(current_forecasts_dates)){
                date <- current_forecasts_dates[i]
                dat <- forecast_data %>%
                        filter(horizon == h) %>% 
                        dplyr::filter(forecast_date >= date - weeks(12),
                                      forecast_date <= date)
                saveRDS(dat, paste0(base_path,"/unity-Shapley-trained/data/forecast_data-horizon",h,"-",date,".rds"))
        }
}





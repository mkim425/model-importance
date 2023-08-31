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


# forecast dates: Nov 2020 - Nov 2022 (Mondays)
dates <- unique(forecast_data$forecast_date) %>% as.character()


# split the whole data set by horizon and forecast date
for (h in 1:4){
        for (i in 1:length(dates)){
                date <- dates[i]
                dat <- forecast_data %>%
                        filter(horizon == h,
                               forecast_date == date)
                saveRDS(dat, paste0(base_path,"/unity-Shapley-untrained/data/forecast_data-horizon",h,"-",date,".rds"))
        }
                
}








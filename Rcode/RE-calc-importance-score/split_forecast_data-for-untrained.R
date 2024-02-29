library(dplyr)
library(magrittr)

# Read data
forecast_data <- readRDS("Data/forecast_data_aligned_death_10models_june20-nov22.rds") %>% 
        filter(forecast_date >= "2020-10-31") 

# forecast reference dates
dates <- unique(forecast_data$reference_date) %>% as.character()


# split the whole data set by horizon and forecast (reference) date
for (h in 1:4){
        for (i in 1:length(dates)){
                date <- dates[i]
                dat <- forecast_data %>%
                        filter(horizon == h,
                               reference_date == date)
                saveRDS(dat, paste0("RE-forecastData/forecast_data-horizon",h,"-",date,".rds"))
        }
                
}








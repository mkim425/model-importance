library(tidyverse)

# function for ensemble models from individual forecasts 
build_mean_ensemble <- function(data, target_date1){ 
        # Input is a long data frame with cols: model, target_date, quantile, value
        
        dat <- data %>% 
                filter(target_date == target_date1)
        # Calculate equally-weighted mean ensemble from all models
        ens.all <- dat %>% 
                group_by(quantile) %>% 
                summarise(value = mean(value)) %>% 
                mutate(model="Ensemble.all") %>% 
                relocate(model) 
        
        fdat.ens <- rbind(dat %>% select(model, quantile, value), ens.all)
        model.names = unique(data$model)
        for (i in model.names){
                ens.part <- dat %>% 
                        filter(model != i) %>% 
                        group_by(quantile) %>% 
                        summarise(value=mean(value)) %>% 
                        mutate(model=paste0("Ens.wo.", i)) %>% 
                        relocate(model)
                fdat.ens <- rbind(fdat.ens, ens.part)
        }
        # wide data frame
        fdat.ens.wide <- fdat.ens %>% 
                pivot_wider(names_from = quantile)
        return(fdat.ens.wide)
}


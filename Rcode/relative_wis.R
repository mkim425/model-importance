# Compute relative WIS given a complete data frame 
# replacing the missing values either with mean or with minimum, or dropping them.
# This code is modified from 
# https://github.com/reichlab/covid19-forecast-evals/blob/main/code/pairwise_wis.R
# where a permutation test is performed.


# function for pairwise comparison of models
pairwise_wis_comparison <- function(data, mx, my){
        scores <- data %>% select("model", "forecast_date", "location", "horizon", "wis")
        subset <- rep(TRUE, nrow(scores))
        
        # apply subset:
        scores <- data[subset, ]
        
        # subsets of available scores for both models:
        subx <- subset(scores, model == mx)
        suby <- subset(scores, model == my)
        
        # merge together and restrict to overlap:
        sub <- merge(subx, suby, 
                     by = c("forecast_date", "location", "horizon"),
                     all.x = FALSE, all.y = FALSE)
        
        # compute ratio:
        ratio <- sum(sub$wis.x) / sum(sub$wis.y)
        
        return(list(ratio = ratio, mx = mx, my = my))
}

relative_wis <- function(data){
        models <- unique(data$model)
        # matrices to store:
        results_ratio <- matrix(ncol = length(models),
                                nrow = length(models),
                                dimnames = list(models, models))
        
        for(mx in seq_along(models)){
                for(my in 1:mx){
                        pwc <- pairwise_wis_comparison(data, mx = models[mx], my = models[my])
                        results_ratio[mx, my] <- pwc$ratio
                        results_ratio[my, mx] <- 1/pwc$ratio
                }
        }
        
        index_baseline <- which(rownames(results_ratio) == "COVIDhub-baseline")
        geom_mean_ratios <- exp(rowMeans(log(results_ratio), na.rm = TRUE))
        ratios_baseline <- results_ratio[, "COVIDhub-baseline"]
        ratios_baseline2 <- geom_mean_ratios/geom_mean_ratios["COVIDhub-baseline"]
        
        tab <- data.frame(model = names(geom_mean_ratios),
                          geom_mean_ratios = geom_mean_ratios,
                          ratios_baseline = ratios_baseline,
                          ratios_baseline2 = ratios_baseline2)
        
        tab <- tab[order(tab$ratios_baseline2), ]
        rownames(tab) <- NULL
        return(tab %>% select(model, ratios_baseline2) %>% rename(relWIS = ratios_baseline2))
}


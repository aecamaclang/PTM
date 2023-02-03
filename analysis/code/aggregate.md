Aggregate Benefit Estimates
================
Abbey Camaclang
02 Feb 2023

This code a) calculates benefits of each strategy (strategy - baseline)
for each ecological group,  
b) aggregates (averages) across experts, and  
c) calculates expected performance (probability of persistence) under
each strategy based on the aggregated estimates.

Based on first part of Step 2 section of 1_Cost-Effectiveness.R code
from the Fraser River Estuary PTM project.  
This script uses **Estimates_wide.csv** from *import.R*.

If some of the expert estimates for a given ecological group are based
on only a subset of species in that group, this estimate need to be
weighted accordingly. To do this, you need  
1) a .csv file *EcolGroupsList.csv* with ecological groups in columns
and the list of species for each group in rows, and  
2) a .csv file *SpecialCases.csv* listing only the estimates (from which
expert, and for which ecol group and strategy) that require different
weighting, along with the number of species in that group that the
estimate is based on.

``` r
# Load packages
library(tidyverse)
library(here)

# Specify paths to subfolders within current working directory
input <- here("analysis", "data", "raw") # where .csv files of benefit estimates are saved
derived <- here("analysis", "data") # where compiled data tables should be saved
results <- here("analysis", "results") # where results of analysis should be saved

# Specify how estimates should be aggregated - this should be based on the
# experts' comments, i.e., whether all species in the ecological group were
# considered in making the estimate, or only a subset of species. Select:  
# (1) if weighting each expert estimate based on the number of species in each group that they scored,  
# (0) if assuming all species in the group were considered in the estimate
wt.by.numspp <- 1 

# Read in and prepare data
long <- read_csv(paste0(derived, "/Estimates_tidy.csv"))
grp.levels <- unique(long$Ecological.Group)

wide <- read_csv(paste0(derived, "/Estimates_wide.csv"))
wide$Expert <- as_factor(wide$Expert)
wide$Ecological.Group <- factor(wide$Ecological.Group, levels = grp.levels)
```

Calculate benefit

``` r
# Subtract baseline performance from strategy performance
baseline <- wide[3:5]
strategies <- wide[6:ncol(wide)]  
benefit <- strategies - as.matrix(baseline)

# benefit[benefit < 0] <- 0 # if there are any logical errors that result in negative values

# Add row labels back to the tables
benefit <- cbind(wide[,1:2], benefit )
baseline <- cbind(wide[,1:2], baseline)
```

Aggregate (weighted average) benefit and the baseline estimates for each
ecol group x strategy

``` r
if (wt.by.numspp == 1) {
  
  # Re-organize benefits table to make it easier to weight estimates
  benefit.long <- gather(benefit, 
                         key = Est.type, 
                         value = Value, 
                         -c(1:2)) %>%
    separate(., 
             Est.type, 
             c("Strategy", "Estimate"), 
             sep = "[.]", 
             remove = TRUE)
  
  strat.levels <- c("Baseline", unique(benefit.long$Strategy))
  benefit.long$Strategy <- factor(benefit.long$Strategy, levels = strat.levels)
  benefit.long$Estimate <- as_factor(benefit.long$Estimate)
  
  benefit.wide <- spread(benefit.long, 
                         key=Estimate, 
                         value = Value)
  
  # Get number of species in each group and number of species scored by each expert for each strategy
  grplist <- read_csv(paste0(input, "/EcolGroupsList.csv")) 
  numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )
  grpwts <- data.frame(Ecological.Group=names(numspp), numspp) 
  grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = unique(names(numspp)))

  spcases <- read_csv(paste0(derived, "/SpecialCases.csv")) 
  spcases$Strategy <- factor(spcases$Strategy, levels = levels(benefit.wide$Strategy))
  spcases$Expert <- factor(spcases$Expert, levels = levels(benefit.wide$Expert))
  spcases$`Ecological Group`<- factor(spcases$`Ecological Group`, levels = levels(benefit.wide$Ecological.Group))
  names(spcases)[which(str_detect(names(spcases), "Ecological Group")==1)] <- "Ecological.Group"  

  # Join tables to calculate weights for each estimate
  benefit.joined <- left_join(benefit.wide, spcases, # new column for number of species that estimate is based on
                              by=c("Expert", "Ecological.Group", "Strategy")) %>% 
    left_join(., grpwts, by = "Ecological.Group") # total number of species in ecol. group
  
  fullwts.idx <- which(is.na(benefit.joined$NumSppScored)) # NOTE: NAs indicate estimates that are based on all species in the ecol group (i.e., full weights)
  benefit.joined$NumSppScored[fullwts.idx] <- benefit.joined$numspp[fullwts.idx] # fills in the column with the total number of species

  fullwts <- aggregate(benefit.joined$NumSppScored, # sums the number of spp scored across all experts
                       by = list(Ecological.Group = benefit.joined$Ecological.Group, 
                                 Strategy = benefit.joined$Strategy), 
                       FUN = sum, na.rm = TRUE)
  
  benefit.joined <- benefit.joined %>%
    left_join(., fullwts, by = c("Ecological.Group", "Strategy")) %>% # adds a column with values (sums) from the aggregate() function above
    mutate(Wts = NumSppScored/x) %>% # calculate the weights to assign (NOTE: x is the sum from above)
    mutate(Wt.Best = Best*Wts,  # applies the weights to the benefit values
           Wt.Low = Low*Wts, 
           Wt.High = High*Wts)
  
  # Aggregate (sum) the weighted estimates and re-organize table for calculating performance
  benefit.avg <- aggregate(benefit.joined[,11:13], 
                           by = list(Ecological.Group = benefit.joined$Ecological.Group, 
                                     Strategy = benefit.joined$Strategy), 
                           FUN = sum, na.rm = TRUE) %>%
    gather(., key = "Est.Type", 
           value = "Wt.Avg", 
           Wt.Best, Wt.Low, Wt.High)
  
  benefit.avg$Est.Type <- as_factor(benefit.avg$Est.Type)

  benefit.avg <- benefit.avg %>%
    arrange(Ecological.Group, Strategy, Est.Type) %>%
    unite(., col = "Estimate", 
          c("Est.Type", "Strategy"), 
          sep = "_", remove = TRUE)
  
  benefit.avg$Estimate <- as_factor(benefit.avg$Estimate)

  benefit.avg <- benefit.avg %>%
    spread(., Estimate, Wt.Avg)
  
  # Prepare the baseline estimates table
  baseline <- baseline %>%
    rename(Best = Baseline.Best, Low = Baseline.Low, High = Baseline.High) %>%
    add_column(Strategy = rep("Baseline", nrow(baseline)), 
               .before = "Best")
  baseline$Strategy <- factor(baseline$Strategy, levels = strat.levels)
  
  # Calculate weights as for benefits (above)
  baseline.joined <- left_join(baseline, spcases, 
                               by = c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  
  base.fullwts.idx <- which(is.na(baseline.joined$NumSppScored))
  baseline.joined$NumSppScored[base.fullwts.idx] <- baseline.joined$numspp[base.fullwts.idx]

  base.fullwts <- aggregate(baseline.joined$NumSppScored, 
                            by = list(Ecological.Group = baseline.joined$Ecological.Group, 
                                      Strategy = baseline.joined$Strategy), 
                            FUN = sum, na.rm = TRUE)
  baseline.joined <- baseline.joined %>%
    left_join(., base.fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.Best_Baseline = Best*Wts,
         Wt.Low_Baseline = Low*Wts,
         Wt.High_Baseline = High*Wts)

  baseline.avg <- aggregate(baseline.joined[,11:13], 
                            by = list(Ecological.Group = baseline.joined$Ecological.Group, 
                                      Strategy = baseline.joined$Strategy), 
                            FUN = sum, na.rm = TRUE) %>%
    select(., -Strategy)
  
  } else {
    
    if (wt.by.numspp == 0) {
      
      # Calculate the simple average
      benefit.avg <- aggregate(benefit[,3:ncol(benefit)], 
                               by=list(benefit$Ecological.Group), 
                               FUN = mean, na.rm = TRUE) 
      baseline.avg <- aggregate(baseline[,3:ncol(baseline)], 
                                by=list(baseline$Ecological.Group), 
                                FUN = mean, na.rm = TRUE)
      
      names(benefit.avg)[1] <- "Ecological.Group"
      names(baseline.avg)[1] <- "Ecological.Group"
      
    }
  }

write_csv(benefit.avg, paste0(results, "/Estimates_avg_benefits.csv"))
write_csv(baseline.avg, paste0(results, "/Estimates_avg_baseline.csv"))
```

Calculate averaged performance (probability of persistence)

``` r
# Add averaged benefit estimates to the (averaged) baseline
persistence <- benefit.avg[,2:ncol(benefit.avg)] + as.matrix(baseline.avg[,2:ncol(baseline.avg)])
persistence <- cbind(baseline.avg, persistence)

write_csv(persistence, paste0(results, "/Estimates_avg_persistence.csv"))
```

Weight benefits by number of species in group (multiply) for calculating
CE scores

``` r
grpwtd_ben <- benefit.avg[,2:ncol(benefit.avg)]*numspp
grpwtd_ben <- cbind(benefit.avg[,1], grpwtd_ben)
names(grpwtd_ben)[1] <- "Ecological.Group"

write_csv(grpwtd_ben, paste0(results, "/Estimates_avg_benefits_groupwtd.csv"))
```

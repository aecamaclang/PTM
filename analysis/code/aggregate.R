#' ---
#' title: "Aggregate Standardized Benefit Estimates"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "24 July 2019"
#' output: github_document
#' ---
#' 
#' This code  
#' a) calculates benefits of each strategy (strategy performance - baseline performance) for each ecological group,  
#' b) aggregates (averages) across experts, and  
#' c) calculates expected performance under each strategy based on the aggregated benefit estimate.  
#'   
#' Based on first part of Step 2 section of 1_Cost-Effectiveness.R code from FRE PTM project and
#' uses **Estimates_std_wide.csv** from *standardizeConfidence.R*.  
#'   
#' If some of the expert estimates need to be weighted differently, must provide a table listing the species in each 
#' ecological group *EcolGroupsList.csv* and a table *SpecialCases.csv* indicating which expert estimates for which 
#' ecological groups and strategies require different weights, and the number of species scored for that estimate.
#' 
#+ warning = FALSE, message = FALSE
library(tidyverse)
library(here)

#may need to change some column names
#for each expert, subtract strategy from baseline? average across experts
#and then average baseline estimate, takes the average baseline estimate and adds to ___ to get new value
#plot this??? This gives data and table only?? plot mean performance? plot average performance? 
#play around with formatting and layout? remove references to standardization in references - should only get one figure

#adjust to not have standardization?? Or no? 
#' Specify how estimates should be aggregated - this should be based on the comments: Are their estimates for all species in the group or only a single or a few species (ie. birds or something)
# (1) if weighting each expert estimate based on the number of species in each group that they scored,  
# (0) if assuming all species in the group were considered in the estimate
wt.by.numspp <- 1 

#' Read in and prepare data
#+ warning = FALSE, message = FALSE
subfolder <- here("analysis", "data", "raw", "benefits") # specifies path to the subfolder where .csv files are saved
resultfolder <- here("analysis", "data", "derived")
wide <- read_csv(paste0(resultfolder, "/Estimates_wide.csv"))
wide$Expert <- as_factor(wide$Expert)
wide$Ecological.Group <- as_factor(wide$Ecological.Group)

# Remove confidence estimates from the table - don't need this?
#wide.colnames <- colnames(rlong.wide)
#idx.colnames <- which(str_detect(wide.colnames, "Confidence") == 1)
#DF <- rlong.wide[,-(idx.colnames)]

#' Calculate benefit: subtract baseline performance from strategy performance for each expert
baseline <- wide[3:5]
strategies <- wide[6:ncol(wide)]  

benefit <- strategies - as.matrix(baseline)
# benefit[benefit < 0] <- 0

benefit <- cbind(wide[,1:2], benefit )
baseline <- cbind(wide[,1:2], baseline)

#' Aggregate benefit estimates: average benefit estimates for each species group + strategy across experts
#+ warning = FALSE, message = FALSE
if (wt.by.numspp == 1) {
  
  # Re-organize benefits table to make it easier to weight estimates
  benefit.long <- gather(benefit, key = Est.type, value = Value, -c(1:2)) %>%
    separate(., Est.type, c("Strategy", "Estimate"), sep = "[.]", remove = TRUE)
  strat.levels <- c("Baseline", unique(benefit.long$Strategy))
  benefit.long$Strategy <- factor(benefit.long$Strategy, levels = strat.levels)
  benefit.long$Estimate <- as_factor(benefit.long$Estimate)
  
  benefit.wide <- spread(benefit.long, key=Estimate, value = Value)
  
  # Get number of species in each group and number of species scored by each expert for each strategy
  grplist <- read_csv(paste0(subfolder, "/EcolGroupsList.csv")) # Table of species in each ecological group
  numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )
  grpwts <- data.frame(Ecological.Group=names(numspp), numspp) 
  grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = unique(names(numspp)))

  spcases <- read_csv(paste0(subfolder, "/SpecialCases.csv")) # Table of number of species in each group scored by individual experts (if different from total)
  spcases$Strategy <- factor(spcases$Strategy, levels = levels(ben.mat.wide$Strategy))
  spcases$Expert <- factor(spcases$Expert, levels = levels(ben.mat.wide$Expert))
  spcases$`Ecological Group`<- factor(spcases$`Ecological Group`, levels = levels(ben.mat.wide$Ecological.Group))
  names(spcases)[which(str_detect(names(spcases), "Ecological Group")==1)] <- "Ecological.Group"  

  # Combine tables to calculate weights for each expert - group - strategy 
  ben.mat.joined <- left_join(ben.mat.wide, spcases, by=c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  fullwts.idx <- which(is.na(ben.mat.joined$NumSppScored))
  ben.mat.joined$NumSppScored[fullwts.idx] <- ben.mat.joined$numspp[fullwts.idx]

  fullwts <- aggregate(ben.mat.joined$NumSppScored, by = list(Ecological.Group = ben.mat.joined$Ecological.Group, Strategy = ben.mat.joined$Strategy), FUN = sum, na.rm = TRUE)
  ben.mat.joined <- ben.mat.joined %>%
    left_join(., fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.Best.guess = Best.guess*Wts, Wt.Lower = Lower*Wts, Wt.Upper = Upper*Wts)
  
  # Aggregate (sum) the weighted estimates and re-organize table for calculating performance
  ben.mat.agg <- aggregate(ben.mat.joined[,11:13], by = list(Ecological.Group = ben.mat.joined$Ecological.Group, Strategy = ben.mat.joined$Strategy), FUN = sum, na.rm = TRUE) %>%
    gather(., key = "Est.Type", value = "Wt.Avg", Wt.Best.guess, Wt.Lower, Wt.Upper)
  ben.mat.agg$Est.Type <- as_factor(ben.mat.agg$Est.Type)

  ben.mat.agg <- ben.mat.agg %>%
    arrange(Ecological.Group, Strategy, Est.Type) %>%
    unite(., col = "Estimate", c("Est.Type", "Strategy"), sep = "_", remove = TRUE)
  ben.mat.agg$Estimate <- as_factor(ben.mat.agg$Estimate)

  ben.mat.agg <- ben.mat.agg %>%
    spread(., Estimate, Wt.Avg)
  
  # Aggregate the baseline estimates
  base.mat <- base.mat %>%
    add_column(Strategy = rep("Baseline", nrow(base.mat)), .before = "Best.guess")
  base.mat$Strategy <- factor(base.mat$Strategy, levels = strat.levels)
  
  base.mat.joined <- left_join(base.mat, spcases, by = c("Expert", "Ecological.Group", "Strategy")) %>%
    left_join(., grpwts, by = "Ecological.Group")
  base.fullwts.idx <- which(is.na(base.mat.joined$NumSppScored))
  base.mat.joined$NumSppScored[base.fullwts.idx] <- base.mat.joined$numspp[base.fullwts.idx]

  base.fullwts <- aggregate(base.mat.joined$NumSppScored, by = list(Ecological.Group = base.mat.joined$Ecological.Group, Strategy = base.mat.joined$Strategy), FUN = sum, na.rm = TRUE)
  base.mat.joined <- base.mat.joined %>%
    left_join(., base.fullwts, by = c("Ecological.Group", "Strategy")) %>%
    mutate(Wts = NumSppScored/x) %>%
    mutate(Wt.Best.guess = Best.guess*Wts,
         Wt.Lower = Lower*Wts,
         Wt.Upper = Upper*Wts)

  base.mat.agg <- aggregate(base.mat.joined[,11:13], by = list(Ecological.Group = base.mat.joined$Ecological.Group, Strategy = base.mat.joined$Strategy), FUN = sum, na.rm = TRUE) %>%
    select(., -Strategy)
  
  } else {
    
    if (wt.by.numspp == 0) {
      
      # Calculate the simple average
      ben.mat.agg <- aggregate(ben.mat[,3:ncol(ben.mat)], by=list(ben.mat$Ecological.Group), FUN = mean, na.rm = TRUE) 
      base.mat.agg <- aggregate(base.mat[,3:ncol(base.mat)], by=list(base.mat$Ecological.Group), FUN = mean, na.rm = TRUE)
      
      names(ben.mat.agg)[1] <- "Ecological.Group"
      names(base.mat.agg)[1] <- "Ecological.Group"
      
    }
  }

#' Calculate averaged performance: add averaged benefit estimates to the (averaged) baseline
exp.pop <- ben.mat.agg[,2:ncol(ben.mat.agg)] + as.matrix(base.mat.agg[,2:ncol(base.mat.agg)])
exp.pop <- cbind(base.mat.agg, exp.pop)

# print(exp.pop)

#' Weight benefits by number of species in group (multiply) for calculating CE scores
grpwtd_ben <- ben.mat.agg[,2:ncol(ben.mat.agg)]*numspp
grpwtd_ben <- cbind(ben.mat.agg[,1], grpwtd_ben)
names(grpwtd_ben)[1] <- "Ecological.Group"

#' Output results
# write_csv(ben.mat.agg, "./results/Estimates_aggregated_benefits.csv")
# write_csv(base.mat.agg, "./results/Estimates_aggregated_baseline.csv")
# write_csv(exp.pop, "./results/Estimates_aggregated_performance.csv")
# write_csv(grpwtd_ben, "./results/Estimates_aggregated_benefits_groupwtd.csv") # for calculating CE scores

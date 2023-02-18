#' ---
#' title: "Calculate Expected Benefit and Performance"
#' author: "Abbey Camaclang"
#' date: "15 Feb 2023"
#' output: github_document
#' ---
#'
#' This code:  
#' 1) calculates expected benefit (benefit * feasibility)  
#' 2) calculates expected performance based on weighted benefit estimates  
#'   
#' Requires **Estimates_avg_benefits.csv** and **Estimates_avg_baseline.csv** from *aggregate.R*, and a
#' **CostFeas.csv** table of strategy cost and feasibility
#'
#+ warning = FALSE, message = FALSE
# Load packages
library(tidyverse)
library(here)

# Specify paths to subfolders within current working directory
derived <- here("analysis", "data") # where compiled data tables should be saved
results <- here("analysis", "results") # where results of analysis should be saved

# Read in and prep data
benefit <- read.csv(paste0(results, "/Estimates_avg_benefits.csv"))
baseline <- read.csv(paste0(results, "/Estimates_avg_baseline.csv"))

costfeas <- read.csv(paste0(derived, "/CostFeas.csv"))
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)

#' ### Calculate the expected benefit (benefit x feasibility)
# Tidy data
long <- gather(benefit, 
               key = Est.type, 
               value = Value, 
               colnames(benefit)[2]:colnames(benefit)[ncol(benefit)]) %>%
  separate(Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = FALSE) %>%
  mutate(Strategy = as_factor(Strategy), 
         Ecological.Group = as_factor(Ecological.Group),
         Est.type = as_factor(Est.type)) 

# Join with cost & feasibility table then weight benefit by feasibility
joined <- left_join(long, costfeas, by = "Strategy") %>%
  mutate(Avg.ExpBen = Value * Avg.Feas, Min.ExpBen = Value * Min.Feas)

# Reformat table and output results
exp.ben <- joined %>%
  select(c(Ecological.Group, Est.type, Min.ExpBen)) %>% 
  spread(key = Est.type, value = Min.ExpBen)

write.csv(exp.ben, paste0(results, "/ExpBenefits.csv"), row.names = FALSE)

#' Sample table:
#+ echo = FALSE
# knitr::kable(exp.ben, "simple")

#' ### Calculate expected performance (baseline probability of persistence + expected benefits)
# Join with baseline estimates to make sure the estimates line up correctly
joined.base <- left_join(baseline, exp.ben, by = "Ecological.Group") 

# Add expected benefit estimates to (averaged) baseline and get the expected performance
base.mat <- joined.base[,2:4]
perf.mat <- joined.base[,5:ncol(joined.base)] + as.matrix(base.mat)

exp.perf <- cbind(joined.base$Ecological.Group,base.mat,perf.mat)
names(exp.perf)[1] <- "Ecological.Group"

write.csv(exp.perf, paste0(results, "/ExpPerform_all.csv"), row.names = FALSE)

#' Sample table:
#+ echo = FALSE
# knitr::kable(exp.perf, "simple")

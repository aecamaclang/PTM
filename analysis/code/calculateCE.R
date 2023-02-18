#' ---
#' title: "Calculate Cost-effectiveness"
#' author: "Abbey Camaclang"
#' date: "17 Feb 2023"
#' output: github_document
#' ---

#' This code calculates cost-effectiveness (CE) scores and ranks strategies by
#' Benefit, Cost, and CE. Based on algorithm from Step 2 section of
#' *1_Cost-Effectiveness.R* code from FRE PTM project, but using a different way
#' to implement.  
#'   
#' Requires **Estimates_avg_benefits_groupwtd.csv** from *aggregate.R*, and a
#' **CostFeas.csv** table of strategy cost and feasibility.  
#'   
#+ warning = FALSE, message = FALSE
# Load packages
library(tidyverse)
library(here)

# Set scaling factor to get cost and benefits in roughly the same order of magnitude
a <- 10^6

# Specify paths to subfolders within current working directory
derived <- here("analysis", "data") # where compiled data tables should be saved
results <- here("analysis", "results") # where results of analysis should be saved

# Read in and prep data
ben.grpwtd <- read.csv(paste0(results, "/Estimates_avg_benefits_groupwtd.csv"))
costfeas <- read.csv(paste0(derived, "/CostFeas.csv"))
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)

#' ### Calculate total benefit of each strategy
#' Sum benefits across all species/ecological groups
bestben <- ben.grpwtd[,-1] %>%
  t() %>%
  data.frame() %>%
  setNames(ben.grpwtd[,1]) %>%
  filter(grepl("Best", rownames(.))) 

bestben.sum <- data.frame(rowSums(bestben)) %>%
  mutate(Est.type = rownames(.)) %>%
  separate(Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = TRUE) %>%
  mutate(Estimate = NULL) %>%
  relocate(Strategy) %>%
  setNames(c("Strategy", "Benefit")) %>%
  mutate(Strategy = as_factor(Strategy))
  
#' ### Calculate cost-effectiveness and rank strategies
#' CE = (Benefit * Feasibility)/Cost
CE_table <- full_join(bestben.sum, costfeas, by="Strategy") %>%
  mutate(Exp.Benefit = Benefit * Min.Feas) %>% # weight benefits by feasibility
  mutate(CE = (Exp.Benefit/Best.Cost)*a) %>% # divide by cost then scale
  select(c("Strategy", "Benefit", "Best.Cost", "Min.Feas", "Exp.Benefit", "CE")) %>%
  mutate(CE_rank = rank(-CE),
         ExpBenefit_rank = rank(-Exp.Benefit),
         Cost_rank = rank(Best.Cost))

write.csv(CE_table, paste0(results, "/CE_Scores.csv"), row.names = FALSE)

#' Sample table
#+ echo = FALSE
knitr::kable(CE_table, "simple", row.names = FALSE)

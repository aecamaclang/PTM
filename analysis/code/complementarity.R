#' ---
#' title: "Generate Benefits Matrix for Complementarity Analysis"
#' author: "Adapted for the SJR PTM by Abbey Camaclang"
#' date: "3 July 2019"
#' output: github_document
#' ---
#'
#' This code weights benefits by feasibility, recalculates expected performance based on weighted benefit estimates, 
#' and generates the benefit matrix for use in the complementarity analysis. Based on 1_Cost-Effectiveness.R code 
#' from FRE PTM project, but using a different (shorter) way to implement
#' 
#' Requires **Estimates_avg_benefits.csv** and **Estimates_avg_baseline.csv** from *aggregate.R*, and a
#' **CostFeas** table of strategy cost and feasibility
#'
#+ warning = FALSE, message = FALSE
# Load packages
library(tidyverse)
library(here)

# Specify paths to subfolders within current working directory
input <- here("analysis", "data", "raw") # where .csv files of benefit estimates are saved
derived <- here("analysis", "data") # where compiled data tables should be saved
results <- here("analysis", "results") # where results of analysis should be saved

benefit <- read_csv(paste0(results, "/Estimates_avg_benefits.csv"))
baseline <- read_csv(paste0(results, "/Estimates_avg_baseline.csv"))

costfeas <- read_csv(paste0(derived, "/CostFeas.csv"))
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)

#' ## Calculate the expected benefit (= benefit x feasibility)
# Tidy data
long <- gather(benefit, key = Estimate, value = Value, 
                    colnames(benefit)[2]:colnames(benefit)[ncol(benefit)]) %>%
  separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = FALSE)
long$Strategy <- as_factor(paste0("S", long$Strategy))
long$Ecological.Group <- as_factor(long$Ecological.Group)

# Join with cost & feasibility table then weight benefit by feasibility
joined <- left_join(long, costfeas, by = "Strategy") %>%
  mutate(., Best.ExpBen = Value * Best.Feas, Min.ExpBen = Value * Min.Feas)

# Spread table and output results
joined.wide <- joined %>%
  select(., c(Ecological.Group, Estimate, Min.ExpBen)) %>%
  spread(key = Estimate, value = Min.ExpBen)
est.levels <- unique(joined$Estimate)
joined.wide <- joined.wide[, c("Ecological.Group", est.levels)] # rearranges columns so strategies are in the correct order

write_csv(joined.wide, paste0(results, "/Expected_Benefits.csv"))

#' ## Calculate expected performance (= baseline probability of persistence + expected benefits)
# Join with baseline estimates to make sure the observations (Ecol. groups) line up correctly
# then split again to add weighted benefits to (averaged) baseline and get the expected performance
joined.base <- left_join(baseline, joined.wide, by = "Ecological.Group") 

base.mat <- joined.base[,2:4]
perf.mat <- joined.base[,5:ncol(joined.base)] + as.matrix(base.mat)

perf.mat <- cbind(joined.base$Ecological.Group,base.mat,perf.mat)
names(perf.mat)[1] <- "Ecological.Group"

write_csv(perf.mat, paste0(results, "/Expected_Performance.csv"))

# Create expected performance matrices for complementarity analysis (optimization) and uncertainty analysis
# Best guess (most likely) estimates
best <- perf.mat %>%
  select(., c(Ecological.Group, contains("Best"))) 
best.t <- data.frame(t(best[,-1]))
names(best.t) <- best$Ecological.Group 
best.t <- best.t %>%
  mutate(., Est.type = rownames(best.t)) %>%
  separate(., Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = TRUE) %>%
  relocate(., Strategy) %>%
  mutate(., Est.type = NULL, Estimate = NULL)

# Lowest (most pessimistic)
low <- perf.mat %>%
  select(., c(Ecological.Group, contains("Low"))) 
low.t <- data.frame(t(low[,-1]))
names(low.t) <- low$Ecological.Group 
low.t <- low.t %>%
  mutate(., Est.type = rownames(low.t)) %>%
  separate(., Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = TRUE) %>%
  relocate(., Strategy) %>%
  mutate(., Est.type = NULL, Estimate = NULL)

# Highest (most optimistic)
high <- perf.mat %>%
  select(., c(Ecological.Group, contains("High"))) 
high.t <- data.frame(t(high[,-1]))
names(high.t) <- high$Ecological.Group 
high.t <- high.t %>%
  mutate(., Est.type = rownames(high.t)) %>%
  separate(., Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = TRUE) %>%
  relocate(., Strategy) %>%
  mutate(., Est.type = NULL, Estimate = NULL)

# Output new tables
write_csv(best.t, paste0(results, "/Best.csv")) # use this table for the complementarity analysis
write_csv(low.t, paste0(results, "/Low.csv")) # for uncertainty analysis under most pessimistic scenario
write_csv(high.t, paste0(results, "/High.csv")) # for uncertainty analysis under most optimistic scenario

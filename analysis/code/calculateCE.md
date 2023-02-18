Calculate Cost-effectiveness
================
Abbey Camaclang
17 Feb 2023

This code calculates cost-effectiveness (CE) scores and ranks strategies
by Benefit, Cost, and CE. Based on algorithm from Step 2 section of
*1_Cost-Effectiveness.R* code from FRE PTM project, but using a
different way to implement.

Requires **Estimates_avg_benefits_groupwtd.csv** from *aggregate.R*, and
a **CostFeas.csv** table of strategy cost and feasibility.

``` r
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
```

### Calculate total benefit of each strategy

Sum benefits across all species/ecological groups

``` r
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
```

### Calculate cost-effectiveness and rank strategies

CE = (Benefit \* Feasibility)/Cost

``` r
CE_table <- full_join(bestben.sum, costfeas, by="Strategy") %>%
  mutate(Exp.Benefit = Benefit * Min.Feas) %>% # weight benefits by feasibility
  mutate(CE = (Exp.Benefit/Best.Cost)*a) %>% # divide by cost then scale
  select(c("Strategy", "Benefit", "Best.Cost", "Min.Feas", "Exp.Benefit", "CE")) %>%
  mutate(CE_rank = rank(-CE),
         ExpBenefit_rank = rank(-Exp.Benefit),
         Cost_rank = rank(Best.Cost))

write.csv(CE_table, paste0(results, "/CE_Scores.csv"), row.names = FALSE)
```

Sample table

| Strategy |   Benefit | Best.Cost | Min.Feas | Exp.Benefit |        CE | CE_rank | ExpBenefit_rank | Cost_rank |
|:---------|----------:|----------:|---------:|------------:|----------:|--------:|----------------:|----------:|
| S1       |  900.3190 |  13886934 |     0.64 |    576.2042 |  41.49254 |       4 |              10 |         7 |
| S2       |  302.8357 |   6676627 |     0.51 |    154.4462 |  23.13237 |      12 |              16 |         5 |
| S3       |  348.0248 |  17119817 |     0.74 |    257.5383 |  15.04329 |      16 |              14 |         8 |
| S4       |  232.8045 |   4535076 |     0.56 |    130.3705 |  28.74715 |       9 |              17 |         2 |
| S5       | 1082.5893 |  18514291 |     0.58 |    627.9018 |  33.91444 |       6 |               9 |         9 |
| S6       | 1003.8910 |   2032309 |     0.50 |    501.9455 | 246.98286 |       1 |              11 |         1 |
| S7       |  545.8904 |   5967926 |     0.68 |    371.2055 |  62.20008 |       2 |              12 |         4 |
| S8       |  191.6186 |   5309326 |     0.83 |    159.0435 |  29.95549 |       8 |              15 |         3 |
| S9       |  304.0821 |   6831409 |     0.94 |    285.8372 |  41.84162 |       3 |              13 |         6 |
| S10      | 1450.5971 |  20546600 |     0.54 |    783.3224 |  38.12419 |       5 |               6 |        10 |
| S11      | 1103.1225 |  31006751 |     0.69 |    761.1545 |  24.54803 |      11 |               7 |        12 |
| S12      | 1597.6216 |  34433534 |     0.57 |    910.6443 |  26.44644 |      10 |               5 |        13 |
| S13      | 1318.3709 |  22595870 |     0.55 |    725.1040 |  32.09011 |       7 |               8 |        11 |
| S14      | 1645.9664 |  47941570 |     0.64 |   1053.4185 |  21.97297 |      13 |               2 |        15 |
| S15      | 1639.3495 |  45645237 |     0.56 |    918.0357 |  20.11241 |      14 |               4 |        14 |
| S16      | 1682.8802 |  64197904 |     0.61 |   1026.5569 |  15.99051 |      15 |               3 |        16 |
| S17      | 1756.7700 |  80873715 |     0.66 |   1159.4682 |  14.33677 |      17 |               1 |        17 |

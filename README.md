# PTM
Aggregates and plots three-point benefit estimates and performs cost-effectiveness and complementarity analysis (in R) for Priority Threat Management

## /analysis
### /code/import.R
* reads individual expert estimate tables from */data/raw/benefits* subfolder and combines them into single */data/Estimates_clean.csv* file
* saves outputs into 2 tables: */data/Estimates_tidy.csv* (tidy version) and *data/Estimates_wide.csv*
* also counts the number of expert estimates for each strategy-group combination (*/data/Estimates_count_strategy.csv*)

### /code/plot.R
* uses */data/Estimates_tidy.csv* to create plots for expert review & feedback (plots not included in this archive)
  * boxplots of the best guess, lower, and upper estimates for each strategy, with separate plots for each ecological group
  * pointrange plots of each individual expert estimate for each strategy, with separate plots for each ecological group
* plots are saved in */figures* subfolder

### /code/aggregate.R
* uses */data/Estimates_std_wide.csv* to
  * calculate the average performance (probability of persistence) under the Baseline scenario (*results/Estimates_avg_baseline.csv*)
  * calculate benefit of each strategy, benefit = Strategy performance - Baseline performance, and average the benefit across experts (*results/Estimates_avg_benefits.csv*)
  * calculate the average performance (probability of persistence) under each strategy = avg benefits + avg baseline (*results/Estimates_avg_persistence.csv*)
  * weights aggregated benefit by number of species in each ecol group (*results/Estimates_avg_benefits_groupwtd.csv*) -- for calculating the CE score
  
### /code/plotAggregated.R
* can use either */results/Estimates_avg_persistence.csv* (unweighted by feasibility) or */results/ExpPerform_all.csv* (weighted by feasibility) to
  * create pointrange plots of (unweighted or weighted) averaged estimates of probability of persistence (y-axis) for each strategy (x-axis) and for each ecological group
  
### /code/expPerformance.R
* uses */results/Estimates_avg_benefits.csv*, */data/CostFeas.csv*, and a table (*/data/raw/Combinations.csv*) that lists which individual strategies are in the combination strategies 
  * calculate expected benefit (benefit x feasibility) (*/results/ExpBenefits.csv*)
  * calculate performance (probability of persistence) using expected benefit value (*/results/ExpPerform_all.csv*)

### /code/optimizeManagement.R
* performs complementarity analysis using consOpt package (https://github.com/ConservationDecisionsLab/consOpt)
  * creates benefit matrix needed for optimization under most likely (*/results/ExpPerform_best.csv*), most optimistic (*/results/ExpPerform_high,csv*), and most pessimistic (*/results/ExpPerform_low.csv*) scenarios.
  * outputs are a .csv table and a .pdf plot of optimal strategies for least cost for each scenario
  
### /code/calculateCEscore.R
* uses */results/Estimates_avg_benefits_groupwtd.csv* and a table of strategy Cost and Feasibility to calculate a cost-effectiveness (CE) score 
    CE = (Benefit*Feasibility)/Cost 
    and rank strategies by Benefit, Cost, and CE. Results are saved as *results/CostEffectiveness_Scores.csv*

## /data
### /raw/benefits/exp00.csv
* raw data files of expert estimates (*/raw/benefits/expXX.csv*, where XX are expert ID numbers) are not uploaded to maintain expert confidentiality. A blank table (*/raw/banefits/exp00.csv*) is provided instead for reference

### /raw/EcolGroupsList.csv
* table with ecological groups (columns) as headers, and the common names of species/communities included in the group (rows)

### /raw/Combinations.csv
* table with strategy names (columns) as headers, and the list of individual strategies included (rows)

### /SpecialCases.csv
* info on number of species in each group that individual experts based their estimates on, where this is less than the total number of species in the group

### /CostFeas.csv
* table listing the Best (most likely), Min, and Max cost estimates, and the Average, Min and Max feasibility estimates for each Strategy

## /results

## /figures

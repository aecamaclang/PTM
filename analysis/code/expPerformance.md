Calculate Expected Benefit and Performance
================
Abbey Camaclang
15 Feb 2023

This code:  
1) calculates expected benefit (benefit \* feasibility)  
2) calculates expected performance based on weighted benefit estimates  
3) and generates the benefit matrix for use in the complementarity
analysis.

Requires **Estimates_avg_benefits.csv** and
**Estimates_avg_baseline.csv** from *aggregate.R*, and a
**CostFeas.csv** table of strategy cost and feasibility

``` r
# Load packages
library(tidyverse)
library(here)

# Specify paths to subfolders within current working directory
input <- here("analysis", "data", "raw") # where .csv files of benefit estimates are saved
derived <- here("analysis", "data") # where compiled data tables should be saved
results <- here("analysis", "results") # where results of analysis should be saved

# Read in and prep data
benefit <- read.csv(paste0(results, "/Estimates_avg_benefits.csv"))
baseline <- read.csv(paste0(results, "/Estimates_avg_baseline.csv"))

costfeas <- read.csv(paste0(derived, "/CostFeas.csv"))
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)
```

Calculate the expected benefit (benefit x feasibility)

``` r
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

head(exp.ben)
```

    ##                Ecological.Group Wt.Best_S1 Wt.Low_S1 Wt.High_S1 Wt.Best_S2
    ## 1             Grassland species  12.160000  9.600000  10.880000   4.182000
    ## 2        Burrow and den species   9.088000  8.768000   6.912000   1.275000
    ## 3             Sand dune species   8.160000  3.200000   6.000000   1.083750
    ## 4 Wetland and shorebird species   6.440506  4.496203   4.860759   1.258861
    ## 5                    Amphibians   8.177778  5.688889   6.044444   3.400000
    ## 6                  Fish species   4.114286  3.200000   3.200000   0.000000
    ##   Wt.Low_S2 Wt.High_S2 Wt.Best_S3 Wt.Low_S3 Wt.High_S3 Wt.Best_S4 Wt.Low_S4
    ## 1  4.233000   3.162000  6.4380000     7.252  5.3280000   1.680000  2.240000
    ## 2  3.060000   0.765000  1.6280000     3.478  0.2220000  -0.168000  0.952000
    ## 3  0.956250   0.637500  4.8100000     1.850  3.2375000   0.000000  0.000000
    ## 4  1.517089   1.291139  1.4987342     0.000  0.7493671   5.465316  4.331139
    ## 5  2.946667   3.570000  0.8222222     0.000  1.2333333   8.848000  7.168000
    ## 6  0.000000   0.000000  0.0000000     0.000  0.0000000   6.000000  6.400000
    ##   Wt.High_S4 Wt.Best_S5 Wt.Low_S5 Wt.High_S5 Wt.Best_S6 Wt.Low_S6 Wt.High_S6
    ## 1   1.400000  12.876000 13.746000  12.006000   9.500000  8.750000   8.500000
    ## 2   0.168000  11.600000 10.730000  11.310000   9.921875  8.554688   9.609375
    ## 3   0.000000   7.757500  6.162500   7.250000   7.625000  5.625000   7.500000
    ## 4   5.557468   7.385823  7.936456   5.954177   6.550633  6.265823   4.810127
    ## 5   6.720000  10.955556 10.762222   9.795556   7.500000  6.944444   6.944444
    ## 6   6.000000   5.385714  6.214286   6.628571   3.571429  3.571429   3.571429
    ##   Wt.Best_S7 Wt.Low_S7 Wt.High_S7 Wt.Best_S8 Wt.Low_S8 Wt.High_S8 Wt.Best_S9
    ## 1   7.820000  7.140000   7.480000   0.000000  2.075000  -0.830000  3.7600000
    ## 2  11.900000 11.356000  11.220000   9.130000 10.790000   7.885000 11.4246154
    ## 3   2.720000  1.700000   2.125000   3.112500  2.593750   3.112500  5.8750000
    ## 4   2.892152  3.580759   3.236456   4.370633  5.211139   4.034430  3.3316456
    ## 5   4.911111  5.440000   5.137778   5.072222  4.150000   5.533333  2.6111111
    ## 6   2.428571  3.885714   1.457143   2.371429  2.964286   1.778571  0.6714286
    ##   Wt.Low_S9 Wt.High_S9 Wt.Best_S10 Wt.Low_S10 Wt.High_S10 Wt.Best_S11
    ## 1  6.580000   2.350000   16.524000  16.524000   15.174000   16.284000
    ## 2 11.222154   9.400000   15.120000  14.310000   14.040000   11.385000
    ## 3  6.462500   4.112500    9.382500   8.910000    9.247500   10.522500
    ## 4  3.807595   1.427848    8.735696   9.849873    8.004304    9.738608
    ## 5  3.133333   1.566667   11.580000  11.400000    9.600000    9.966667
    ## 6  2.014286   0.000000    6.171429   6.942857    6.942857    3.450000
    ##   Wt.Low_S11 Wt.High_S11 Wt.Best_S12 Wt.Low_S12 Wt.High_S12 Wt.Best_S13
    ## 1  14.214000   14.214000   19.209000  19.836000   17.214000   14.685000
    ## 2  11.730000   10.005000   17.043000  15.960000   15.390000   11.770000
    ## 3   6.900000    9.056250   11.685000   9.048750   10.473750   10.450000
    ## 4   4.498101    6.637975    9.740506  10.858861    8.622152    9.155063
    ## 5   7.283333    7.283333   13.806667  13.933333   12.160000   11.733333
    ## 6   3.942857    3.450000    6.514286   8.142857    6.107143    5.892857
    ##   Wt.Low_S13 Wt.High_S13 Wt.Best_S14 Wt.Low_S14 Wt.High_S14 Wt.Best_S15
    ## 1  14.190000   13.310000   22.080000   22.08000   19.712000    18.87200
    ## 2  11.550000   11.275000   19.840000   18.88000   18.880000    16.74400
    ## 3   7.562500    9.281250   13.920000    9.76000   12.960000    12.18000
    ## 4   8.563291    7.797468   11.325570   11.93316   10.134684    10.66835
    ## 5  10.511111   10.205556   15.288889   14.57778   13.297778    14.18667
    ## 6   6.285714    5.500000    7.771429    9.60000    7.314286     8.40000
    ##   Wt.Low_S15 Wt.High_S15 Wt.Best_S16 Wt.Low_S16 Wt.High_S16 Wt.Best_S17
    ## 1   19.20800   17.472000   20.557000   18.17800   18.117000    23.23200
    ## 2   15.62400   15.624000   18.422000   17.01900   17.568000    20.65800
    ## 3    8.54000   10.990000   13.801250   10.98000   11.742500    15.34500
    ## 4   11.48354    9.151392   14.277089   11.14987   11.574557    16.24937
    ## 5   13.37778   12.444444   14.436667   13.89444   12.674444    16.50000
    ## 6   10.00000    7.200000    9.585714   10.45714    9.585714    11.31429
    ##   Wt.Low_S17 Wt.High_S17
    ## 1   21.64800    20.59200
    ## 2   18.34800    19.07400
    ## 3   11.88000    13.11750
    ## 4   13.20000    13.12481
    ## 5   16.13333    15.03333
    ## 6   11.78571    11.31429

Calculate expected performance (baseline probability of persistence +
expected benefits)

``` r
# Join with baseline estimates to make sure the observations (Ecol. groups) line up correctly
# then split again to add weighted benefits to (averaged) baseline and get the expected performance
joined.base <- left_join(baseline, exp.ben, by = "Ecological.Group") 

base.mat <- joined.base[,2:4]
perf.mat <- joined.base[,5:ncol(joined.base)] + as.matrix(base.mat)

exp.perf <- cbind(joined.base$Ecological.Group,base.mat,perf.mat)
names(exp.perf)[1] <- "Ecological.Group"

write.csv(exp.perf, paste0(results, "/ExpPerform_all.csv"), row.names = FALSE)

head(exp.perf)
```

    ##                Ecological.Group Wt.Best_Baseline Wt.Low_Baseline
    ## 1             Grassland species         45.50000        29.50000
    ## 2        Burrow and den species         45.50000        29.50000
    ## 3             Sand dune species         52.87500        41.25000
    ## 4 Wetland and shorebird species         51.20253        38.73418
    ## 5                    Amphibians         50.55556        38.33333
    ## 6                  Fish species         55.00000        43.57143
    ##   Wt.High_Baseline Wt.Best_S1 Wt.Low_S1 Wt.High_S1 Wt.Best_S2 Wt.Low_S2
    ## 1         57.00000   57.66000  39.10000   67.88000   49.68200  33.73300
    ## 2         56.00000   54.58800  38.26800   62.91200   46.77500  32.56000
    ## 3         65.62500   61.03500  44.45000   71.62500   53.95875  42.20625
    ## 4         63.67089   57.64304  43.23038   68.53165   52.46139  40.25127
    ## 5         62.22222   58.73333  44.02222   68.26667   53.95556  41.28000
    ## 6         65.00000   59.11429  46.77143   68.20000   55.00000  43.57143
    ##   Wt.High_S2 Wt.Best_S3 Wt.Low_S3 Wt.High_S3 Wt.Best_S4 Wt.Low_S4 Wt.High_S4
    ## 1   60.16200   51.93800  36.75200   62.32800   47.18000  31.74000   58.40000
    ## 2   56.76500   47.12800  32.97800   56.22200   45.33200  30.45200   56.16800
    ## 3   66.26250   57.68500  43.10000   68.86250   52.87500  41.25000   65.62500
    ## 4   64.96203   52.70127  38.73418   64.42025   56.66785  43.06532   69.22835
    ## 5   65.79222   51.37778  38.33333   63.45556   59.40356  45.50133   68.94222
    ## 6   65.00000   55.00000  43.57143   65.00000   61.00000  49.97143   71.00000
    ##   Wt.Best_S5 Wt.Low_S5 Wt.High_S5 Wt.Best_S6 Wt.Low_S6 Wt.High_S6 Wt.Best_S7
    ## 1   58.37600  43.24600   69.00600   55.00000  38.25000   65.50000   53.32000
    ## 2   57.10000  40.23000   67.31000   55.42188  38.05469   65.60938   57.40000
    ## 3   60.63250  47.41250   72.87500   60.50000  46.87500   73.12500   55.59500
    ## 4   58.58835  46.67063   69.62506   57.75316  45.00000   68.48101   54.09468
    ## 5   61.51111  49.09556   72.01778   58.05556  45.27778   69.16667   55.46667
    ## 6   60.38571  49.78571   71.62857   58.57143  47.14286   68.57143   57.42857
    ##   Wt.Low_S7 Wt.High_S7 Wt.Best_S8 Wt.Low_S8 Wt.High_S8 Wt.Best_S9 Wt.Low_S9
    ## 1  36.64000   64.48000   45.50000  31.57500   56.17000   49.26000  36.08000
    ## 2  40.85600   67.22000   54.63000  40.29000   63.88500   56.92462  40.72215
    ## 3  42.95000   67.75000   55.98750  43.84375   68.73750   58.75000  47.71250
    ## 4  42.31494   66.90734   55.57316  43.94532   67.70532   54.53418  42.54177
    ## 5  43.77333   67.36000   55.62778  42.48333   67.75556   53.16667  41.46667
    ## 6  47.45714   66.45714   57.37143  46.53571   66.77857   55.67143  45.58571
    ##   Wt.High_S9 Wt.Best_S10 Wt.Low_S10 Wt.High_S10 Wt.Best_S11 Wt.Low_S11
    ## 1   59.35000    62.02400   46.02400    72.17400    61.78400   43.71400
    ## 2   65.40000    60.62000   43.81000    70.04000    56.88500   41.23000
    ## 3   69.73750    62.25750   50.16000    74.87250    63.39750   48.15000
    ## 4   65.09873    59.93823   48.58405    71.67519    60.94114   43.23228
    ## 5   63.78889    62.13556   49.73333    71.82222    60.52222   45.61667
    ## 6   65.00000    61.17143   50.51429    71.94286    58.45000   47.51429
    ##   Wt.High_S11 Wt.Best_S12 Wt.Low_S12 Wt.High_S12 Wt.Best_S13 Wt.Low_S13
    ## 1    71.21400    64.70900   49.33600    74.21400    60.18500   43.69000
    ## 2    66.00500    62.54300   45.46000    71.39000    57.27000   41.05000
    ## 3    74.68125    64.56000   50.29875    76.09875    63.32500   48.81250
    ## 4    70.30886    60.94304   49.59304    72.29304    60.35759   47.29747
    ## 5    69.50556    64.36222   52.26667    74.38222    62.28889   48.84444
    ## 6    68.45000    61.51429   51.71429    71.10714    60.89286   49.85714
    ##   Wt.High_S13 Wt.Best_S14 Wt.Low_S14 Wt.High_S14 Wt.Best_S15 Wt.Low_S15
    ## 1    70.31000    67.58000   51.58000    76.71200    64.37200   48.70800
    ## 2    67.27500    65.34000   48.38000    74.88000    62.24400   45.12400
    ## 3    74.90625    66.79500   51.01000    78.58500    65.05500   49.79000
    ## 4    71.46835    62.52810   50.66734    73.80557    61.87089   50.21772
    ## 5    72.42778    65.84444   52.91111    75.52000    64.74222   51.71111
    ## 6    70.50000    62.77143   53.17143    72.31429    63.40000   53.57143
    ##   Wt.High_S15 Wt.Best_S16 Wt.Low_S16 Wt.High_S16 Wt.Best_S17 Wt.Low_S17
    ## 1    74.47200    66.05700   47.67800    75.11700    68.73200   51.14800
    ## 2    71.62400    63.92200   46.51900    73.56800    66.15800   47.84800
    ## 3    76.61500    66.67625   52.23000    77.36750    68.22000   53.13000
    ## 4    72.82228    65.47962   49.88405    75.24544    67.45190   51.93418
    ## 5    74.66667    64.99222   52.22778    74.89667    67.05556   54.46667
    ## 6    72.20000    64.58571   54.02857    74.58571    66.31429   55.35714
    ##   Wt.High_S17
    ## 1    77.59200
    ## 2    75.07400
    ## 3    78.74250
    ## 4    76.79570
    ## 5    77.25556
    ## 6    76.31429

``` r
# Create expected performance matrices for complementarity analysis (optimization) and uncertainty analysis
perf.transposed <- exp.perf[,-1] %>%
  t() %>%
  data.frame() %>%
  setNames(exp.perf[,1]) %>%
  mutate(Est.type = rownames(.)) %>%
  separate(Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = TRUE) %>%
  relocate(Estimate, Strategy)

best <- perf.transposed %>%
  filter(grepl("Best", Estimate)) %>%
  mutate(Estimate = NULL)

low <- perf.transposed %>%
  filter(grepl("Low", Estimate)) %>%
  mutate(Estimate = NULL)

high <- perf.transposed %>%
  filter(grepl("High", Estimate)) %>%
  mutate(Estimate = NULL)

write.csv(best, paste0(results, "/ExpPerform_best.csv"), row.names = FALSE) # use this table for the complementarity analysis
write.csv(low, paste0(results, "/ExpPerform_low.csv"), row.names = FALSE) # for uncertainty analysis under most pessimistic scenario
write.csv(high, paste0(results, "/ExpPerform_high.csv"), row.names = FALSE) # for uncertainty analysis under most optimistic scenario

head(best)
```

    ##                  Strategy Grassland species Burrow and den species
    ## Wt.Best_Baseline Baseline            45.500                 45.500
    ## Wt.Best_S1             S1            57.660                 54.588
    ## Wt.Best_S2             S2            49.682                 46.775
    ## Wt.Best_S3             S3            51.938                 47.128
    ## Wt.Best_S4             S4            47.180                 45.332
    ## Wt.Best_S5             S5            58.376                 57.100
    ##                  Sand dune species Wetland and shorebird species Amphibians
    ## Wt.Best_Baseline          52.87500                      51.20253   50.55556
    ## Wt.Best_S1                61.03500                      57.64304   58.73333
    ## Wt.Best_S2                53.95875                      52.46139   53.95556
    ## Wt.Best_S3                57.68500                      52.70127   51.37778
    ## Wt.Best_S4                52.87500                      56.66785   59.40356
    ## Wt.Best_S5                60.63250                      58.58835   61.51111
    ##                  Fish species Healthy prairie landscape
    ## Wt.Best_Baseline     55.00000                  40.00000
    ## Wt.Best_S1           59.11429                  54.22222
    ## Wt.Best_S2           55.00000                  43.51333
    ## Wt.Best_S3           55.00000                  46.57778
    ## Wt.Best_S4           61.00000                  45.60000
    ## Wt.Best_S5           60.38571                  56.11111

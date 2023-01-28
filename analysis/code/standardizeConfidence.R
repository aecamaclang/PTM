#' ---
#' title: "Standardize Benefit Estimates"
#' author: "Adapted for the SK PTM by Griffy J. Vigneron"
#' date: "6 October 2022"
#' output: github_document
#' ---

#' This script organizes and provides data about benefit estimates and saves results tables as .csv files:  
#' 1) **Estimates_tidy.csv** - Tidy (long) version of the original **Estimates_combined.csv** file  
#' 2) **Estimates_count_group.csv** - Number of expert estimates for each ecological group  
#' 3) **Estimates_count_strategy.csv** - Number of expert estimates there are for each strategy x group  
#' 4) **Estimates_std_wide.csv** - Reformat into a wider tidy version of Estimates_combined.csv  
#' 5) **Estimates_std_long.csv** - Tidy version of Standardized estimates - for use in plotting  
#' 
#'
#' It requires output from *combineTables.R*, which organizes the estimates into a
#' single table and saves it as **Estimates_combined.csv** in the data folder.
#'
#' Load packages
#+ warning = FALSE, message = FALSE
library(tidyverse)
library(stringr)
library(here)

#' ## Read in and tidy data
#+ warning = FALSE, message = FALSE
subfolder <- here("results")
estimates <- read.csv("./results/Estimates_combined.csv")
# head(estimates)

#' Use tidyr package to transform data to tidy version, with single columns for Estimate (e.g., best guess, lower, upper) and Value (value of estimate, 0-100)
rlong <-
  gather(estimates,
         key = Estimate,
         value = Value,
         Best.guess:Upper_17
  )
head(rlong)

rlong <- na.omit(rlong)
write_csv(rlong, "./results/Estimates_tidy.csv")

rlong$Value <- as.numeric((rlong$Value))
# str(rlong) # Check data type


#' ## Summarize the number of expert estimates
#' Tabulate how many expert estimates there are for each ecological group
table.data <-spread(rlong, Estimate, Value) 
table.subset <- table.data[, c(2, 3)] # Subset table.data to only include the columns "Expert" and "Ecological.Group"
exp.table <- table(table.subset$Ecological.Group)
write.csv(exp.table, "./results/Estimates_count_group.csv", row.names=FALSE)
exp.table

#' Create new columns to specify Estimate Type and Strategy separately
# Find the "_" character and uses the digits that follow it as the Strategy name. 
rlong$Strategy <- "BLANK"
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)] <- 
  paste0("S",str_extract(rlong$Estimate[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==1)], "(?<=_)[:digit:]+"))
rlong$Strategy[which(str_detect(rlong$Estimate, "(?<=_)[:digit:]+")==0)] <- 
  paste0("Baseline") # Rows without the "_" are Baseline estimates

# Create a new column for type of estimate
rlong$Est.Type <- "BLANK" # creates a new column in the table
rlong$Est.Type[grep("Best.guess", rlong$Estimate)] <- "Best.Guess"
rlong$Est.Type[grep("Lower", rlong$Estimate)] <- "Lower"
rlong$Est.Type[grep("Upper", rlong$Estimate)] <- "Upper"

#' Tabulate how many estimates there are for each strategy
table.subset2 <- subset(rlong, Est.Type=="Best.Guess") # Subset to count how many experts provided estimates for each group and strategy
strat.levels <- unique(table.subset2$Strategy)
table.subset2$Strategy <- factor(table.subset2$Strategy,levels = strat.levels)
st.table <- table(table.subset2$Ecological.Group, table.subset2$Strategy)
write.csv(st.table, "./results/Estimates_count_strategy.csv") #neeed this
st.table

#' Subset dataframe by estimate type 
bg <- subset(rlong, Est.Type == "Best.Guess")
low <- subset(rlong, Est.Type == "Lower")
up <- subset(rlong, Est.Type == "Upper")

# Create new table in wide format 
rlong.sub <- rlong[,c(1,2,3,4,5)] 
rlong.wide <- spread(rlong.sub, Estimate, Value)

# Make sure Strategies are in correct order
est.levels <- unique(rlong$Estimate)
rlong.wide <- rlong.wide[, c("Expert", "Ecological.Group", est.levels)] 

# Make sure Ecological groups are in the same order as in original tables
grp.levels <- unique(rlong$Ecological.Group)
rlong.wide$Ecological.Group<-factor(rlong.wide$Ecological.Group, levels=grp.levels) 
rlong.wide <- with(rlong.wide, rlong.wide[order(Expert, Ecological.Group),]) 

# Output results
write_csv(rlong.wide, "./results/Estimates_std_wide.csv")
write_csv(rlong, "./results/Estimates_std_long2.csv") 

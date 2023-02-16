#' ---
#' title: "Optimize Strategies"
#' author: "Abbey Camaclang"  
#' date: "15 Feb 2023"
#' output: github_document
#' ---

#' This code performs the complementarity analysis using the consOpt R package
#' developed by Nicolai Cryer <nkcryer@gmail.com> and updated by Abbey Camaclang
#' with a number of bug fixes.  
#'   
#' Requires **ExpPerform_best.csv** from *expPerformance.R*, and the
#' **CostFeas.csv** table of cost and feasibility for each strategy. Also
#' requires a table **Combinations.csv** specifying which individual strategies
#' are in the combination strategies.  
#'   
#' If using for the first time, need to install and load packages. Two ways to  build/install the consOpt package:  
#'   
#' Method A  
#+ eval = FALSE
install.packages("remotes")
library("remotes")
install_github("ConservationDecisionsLab/consOpt")

#' OR Method B  
#' 1) download/clone the github repository https://github.com/ConservationDecisionsLab/consOpt  
#' 2) open up RStudio and open the RProj file in the downloaded directory  
#' 3) press "build and install" in the top right corner. This should restart the R session and load the package.  
#'   
#' May need to install/update Rtools, R/RStudio, or some of the required
#' packages - see DESCRIPTION file in the consOpt package for a list of other required packages.  
#'   
#+ warning = FALSE, message = FALSE
# Load packages
library(consOpt)
library(tidyverse)
library(cowplot)
library(here)

# Specify paths to subfolders within current working directory
input <- here("analysis", "data", "raw") # where raw data files are located
derived <- here("analysis", "data") # where compiled or summary data tables are located
results <- here("analysis", "results") # where results of analysis should be saved
figures <- here("analysis", "figures") # where plots should be saved

# Read in and prep data
combos <- read.csv(paste0(input, "/Combinations.csv"), header = TRUE) # list of individual strategies that make up each strategy (in columns). Should have a column for baseline and all strategies
benefits.best <- read.csv(paste0(results, "/ExpPerform_best.csv"), row.names = 1) # expected probability of persistence for each Strategy (rows) and Species Group (columns), including baseline
costfeas <- read.csv(paste0(derived, "/CostFeas.csv")) # estimated Cost and Feasibility for each Strategy (col 1), including Baseline
costs <- costfeas$Best.Cost
names(costs) <- costfeas$Strategy

#' ## Find optimal strategies
#' Run the optimization routine across the default budgets and thresholds. For some sparse documentation, type ?Optimize
#+ warning = FALSE, message = FALSE
results.best <- Optimize(benefits.matrix = benefits.best, 
                    cost.vector = costs, 
                    combo.strategies = combos
                    , thresholds = c(50.01, 60.01)
                    )
write.csv(results.best, paste0(results, "/Complementarity_best.csv"), row.names = FALSE)

#' Plot using plotting function included in the consOpt package
optcurve.best <- PlotResults(results.best)

#' OR create custom plot function (based on plotting function in the package):
#+ warning = FALSE, message = FALSE
library(viridis)

PlotOptCurve <- function(summary.results, benefits.matrix, draw.labels=TRUE){
  
  tmp <- summary.results
  
  tmp$total_cost <- (tmp$total_cost / 10^6) # rescale x-axis to millions
  tmp$threshold <- round(tmp$threshold) # remove decimal points
  
  # Create plot object
  this.plot <- ggplot(tmp, aes(
    x = total_cost, 
    y = number_of_species, 
    group = threshold, 
    linetype = factor(threshold),
    shape = factor(threshold), 
    label = ifelse(strategies=="Baseline"," ",strategies)
  )
  ) +
    geom_step(
      # aes(color = factor(threshold)), 
      # size = 0.8,
      # alpha = 0.6
    ) +
    geom_point(
      # aes(color = factor(threshold)),
      size = 2
      # ,show.legend = FALSE
    ) +
    theme_cowplot() +
    theme(legend.justification = c(1,0),
          legend.position = c(0.95, 0.05),
          legend.key.height = unit(0.6,"cm"),
          legend.key.width = unit(1, "cm"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12),
          plot.margin = margin(0.5, 1, 0.5, 0.5, "cm")
          # legend.title.align=0.5
    ) +
    # scale_color_viridis(discrete=TRUE) +
    scale_y_continuous(
      # labels = function (x) floor(x), 
      breaks = min(tmp$number_of_species):length(benefits.matrix),
      limits = c(min(tmp$number_of_species), length(benefits.matrix))
    ) +
    labs(x = "Total cost (millions)", 
         y = "Number of groups secured"
         , linetype = "Persistence\nthreshold (%)"
         , shape = "Persistence\nthreshold (%)"
    )
  
  if(draw.labels){
    this.plot <- this.plot + 
      geom_text_repel(size = 4, 
                      hjust = "left", 
                      nudge_x = 0.5,
                      nudge_y = -0.15,
                      xlim = c(0, max(tmp$total_cost)+5), 
                      ylim = c(-0.5, max(tmp$number_of_species)+0.5), 
                      show.legend = FALSE
                      # , direction = "both"
      )
  }
  
  plot(this.plot)
  this.plot
}

optcurve.best <- PlotOptCurve(results.best, benefits.best, draw.labels = TRUE)

ggsave(paste0(figures, "/Complementarity_best.pdf"), optcurve.best, width = 180, height = 120, units = "mm")
# ggsave(paste0(figures, "/Complementarity_best.tiff"), optcurve.best, width = 120, height = 115, units = "mm", dpi = 600)

#' ## Uncertainty analysis
#' Run the optimization using lowest (pessimistic) estimates and highest (optimistic) estimates
#+ warning = FALSE, message = FALSE
benefits.low <- read.csv(paste0(results, "/ExpPerform_low.csv"), row.names = 1)
benefits.high <- read.csv(paste0(results, "/ExpPerform_high.csv"), row.names = 1)

results.low <- Optimize(benefits.matrix = benefits.low,
                  cost.vector = costs,
                  combo.strategies = combos
                  , thresholds = c(40.01, 50.01)
                  )

results.high <- Optimize(benefits.matrix = benefits.high,
                  cost.vector = costs,
                  combo.strategies = combos
                  , thresholds = c(60.01, 70.01)
                  ) 

write.csv(results.low, paste0(results, "/Complementarity_low.csv"), row.names = FALSE)
write.csv(results.high, paste0(results, "/Complementarity_high.csv"), row.names = FALSE)

#' Plot the benefit curves for lowest and highest estimates
#+ warning = FALSE, message = FALSE
# Using plotting function included in the consOpt package:
optcurve.low <- PlotResults(results.low)
optcurve.high <- PlotResults(results.high)

# OR using the custom plot function above:
optcurve.low <- PlotOptCurve(results.low, benefits.best, draw.labels = TRUE)
optcurve.high <- PlotOptCurve(results.high, benefits.best, draw.labels = TRUE)

# Save plots as pdf or tiff files
ggsave(paste0(figures, "/Complementarity_low.pdf"), optcurve.low, width = 180, height = 120, units = "mm")
ggsave(paste0(figures, "/Complementarity_high.pdf"), optcurve.high, width = 180, height = 120, units = "mm")
# ggsave(paste0(figures, "/Complementarity_low.tiff"), optcurve.low, width = 120, height = 115, units = "mm", dpi = 600)
# ggsave(paste0(figures, "/Complementarity_high.tiff"), optcurve.high, width = 120, height = 115, units = "mm", dpi = 600)

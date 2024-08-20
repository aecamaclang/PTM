#' Calculate benefit
#' 
#' Calculates benefit of each action from individual expert estimates.
#' 
#' @param dataframe individual expert estimates for each species/group (rows) 
#' under the Baseline and each Action/Strategy (columns).
#' 
#' @return dataframe of the benefits of each Action/Strategy. 
#'
#' @examples 
#' calculate_benefit()
#' 
#' @export
calculate_benefit <- function(dataframe) {
  
  base <- dataframe[which(grepl("Baseline", names(dataframe)))]
  strats <- dataframe[which(grepl("S\\d", names(dataframe)))]  
  benefit <- strats - as.matrix(base)
  
  benefit <- cbind(dataframe[which(!grepl('Baseline|S\\d', names(dataframe)))],
                   benefit)
  # base <- cbind(dataframe[,1:2], base)
}

#' Number of species
#' 
#' Calculates the number of species in each group.
#' 
#' @param t table with the names of all species in each ecological group. 
#' Ecological group names are used as column headers, with species names
#' listed below.
#' 
#' @return dataframe with number of species in each group.
#' 
#' @examples
#' 
#' @export
sum_spp <- function(t) {
  numspp <- apply(t, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]))
  grpwts <- data.frame(Ecological.Group=names(numspp), numspp)
  # grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = unique(names(numspp)))
  grpwts
}

#' Weight individual expert estimates
#' 
#' Calculates the weights to assign to each expert estimate based on the number
#' of species in each group that the estimate is based on.
#' 
#' @param estimate table of benefits calculated from individual expert estimates.
#' @param nspp table with number of species in each group.
#' @param nscore table with the number of species scored by each expert for each action.
#'
#' @return
#' 
#' @example
#' 
#' @export
wt_species <- function(estimate, nspp, nscore = NULL) {
  if (is.null(nscore)) { #if all estimates based on entire group, nscore = NULL (default)
    out <- estimate
    out['NumSppScored'] <- NA
  } else {
    out <- left_join(estimate, nscore, # new column for number of species that estimate is based on
                     by=c("Expert", "Ecological.Group", "Strategy"))
  }
  out <- out %>%
    left_join(nspp, by = "Ecological.Group") # total number of species in ecol. group
  
  idx <- which(is.na(out$NumSppScored)) # NOTE: NAs indicate estimates that are based on all species in the ecol group (i.e., full weights)
  out$NumSppScored[idx] <- out$numspp[idx] # fills in the column with the total number of species
  
  wts <- aggregate(out$NumSppScored, # sums the number of spp scored across all experts
                   by = list(Ecological.Group = out$Ecological.Group, 
                             Strategy = out$Strategy, Estimate = out$Estimate), 
                   FUN = sum, na.rm = TRUE)
  
  out <- out %>%
    left_join(wts, by = c("Ecological.Group", "Strategy", "Estimate")) %>% # adds a column with values (sums) from the aggregate() function above
    mutate(Wts = NumSppScored/x) %>% # calculate the weights to assign (NOTE: x is the sum from above)
    mutate(Wt.Avg = Value *Wts)
}
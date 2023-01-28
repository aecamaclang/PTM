#' ---
#' title: "combineTables.R"
#' author: "Abbey Camaclang"
#' date: "17 June 2023"
#' output: github_document
#' ---

#' This script reads individual expert estimates from multiple .csv files
#' and compiles them into a single **Estimates_all.csv** file.
#' It requires that each expert table is saved as a .csv file in a *benefits* subfolder in the */analysis/data/raw* folder, 
#' contain the same number of rows and columns, and no other .csv files are in the same folder.

#+ warning = FALSE, message = FALSE
#+ 

library(naniar)
library(stringi)
#library(dplyr)
library(plyr)
library(tidyverse)
library(data.table)
library(here)

#' Read in the individual tables of expert estimates and combine. NOTE to maintain confidentiality, only sample tables are provided
#+ warning = FALSE, message = FALSE

nstrat <- 17 # number of management strategies (including combinations, but excluding baseline)
ngroups <- 7 # number of ecological groups (rows)
numexp <- 10 # number of experts

numcols <- (nstrat+1+2) # total number of columns to read in (one for each strategy + 1 column for Baseline + 2 for row header names)
numrows <- (ngroups+1)*3 # total number of rows to read in (3 rows for each Ecol group and the example [Best guess, Lower, Upper])

skiplines <- 18# number of header rows to skip (a bit finicky - needs trial & error)

# Combine .csv files into single data frame
subfolder <- here("analysis", "data", "raw", "benefits") # specifies path to the subfolder where .csv files are saved
files <- list.files(path = paste(subfolder, "/", sep=""), # Name of the subfolder in working directory that contains the files
                    pattern = "*.csv", 
                    full.names = T)
listcsv <- lapply(files, 
                  function(x) read.csv(x, 
                                       skip = 20, 
                                       header = F, 
                                       nrows = numrows, 
                                       as.is = T))
rawdata <- do.call("rbind", listcsv)
rawdata <- rawdata[,1:numcols] # exclude 'Notes' column
#stratnames <- paste(rep("S", times = nstrat), 1:nstrat, sep = "") # vector of simplified strategy names
names(rawdata) <- c("Ecological.Group", 
                 "Estimate", 
                 "Baseline", 
                 paste(rep("S", times = nstrat), 
                       1:nstrat, 
                       sep = "")) 

# Add Expert column
tempvec <- c()
for (i in 1:numexp) {tempvec <- c(tempvec, rep(i, times = (ngroups+1)*3))}

rawdata <- rawdata %>% 
  add_column(Expert = tempvec, .before = "Ecological.Group")


##### Format table
data <- rawdata %>% 
  mutate(Ecological.Group = ifelse(Ecological.Group == "", NA, Ecological.Group)) %>% # changes blank rows to NA
  fill(Ecological.Group) %>% # fills NA rows with Ecological Group name
  filter(!grepl("example", Ecological.Group)) # remove 'example' rows 
  
# Change Ecol Group names to short forms
data$Ecological.Group[which(str_detect(data$Ecological.Group, "GRASSLAND") == 1)] <- "Grassland species"
data$Ecological.Group[which(str_detect(data$Ecological.Group, "BURROW/DEN") == 1)] <- "Burrow/den species"
data$Ecological.Group[which(str_detect(data$Ecological.Group, "SAND DUNE") == 1)] <- "Sand dune species"
data$Ecological.Group[which(str_detect(data$Ecological.Group, "WETLAND") == 1)] <- "Wetland species"
data$Ecological.Group[which(str_detect(data$Ecological.Group, "AMPHIBIANS") == 1)] <- "Amphibians"
data$Ecological.Group[which(str_detect(data$Ecological.Group, "FISH SPECIES") == 1)] <- "Fish species"
data$Ecological.Group[which(str_detect(data$Ecological.Group, "HEALTHY") == 1)] <- "Healthy prairie landscape"

data$Estimate[which(str_detect(data$Estimate, "High") == 1)] <- "High"
data$Estimate[which(str_detect(data$Estimate, "Low") == 1)] <- "Low"

##### Clean data  
# Replace X's  and blanks with NA
na_strings <- c("X", "X ", "x", "x ")
clean <- data %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  replace_with_na_all(condition = ~.x == "")
 
# Replace B's with baseline estimates from same row 
for (i in 1:(numexp*ngroups*3)) {
  temp <- which(data[i,] == "B" | data[i,] == "b")
  clean[i,temp] <- data[i,4]
}

# Save to csv
write.csv(clean, file="./analysis/data/Estimates_all.csv", row.names=FALSE) 

#' Convert to tidy (long) table
long <-
  gather(clean,
         key = Strategy,
         value = Value,
         Baseline:S17
  )

write_csv(long, "./analysis/data/derived/Estimates_tidy.csv")

long <- na.omit(long)
long$Value <- as.numeric(long$Value)
tempvec <- paste(long$Strategy, long$Estimate, sep = ".")
long <- add_column(long, Strat.Est = tempvec, .before = "Value")

# Create new table in wide format 
long.sub <- long[,c(1,2,5,6)]
wide <- spread(long.sub, Strat.Est, Value)

# Reorder 
est.levels <- unique(long$Strat.Est)
wide <- wide[, c("Expert", "Ecological.Group", est.levels)] 
# strat.levels <- unique(long$Strategy)
# wide$Strategy <- factor(wide$Strategy, levels = strat.levels)
# wide <- wide[, c("Expert", "Ecological.Group", est.levels)] 

grp.levels <- unique(long$Ecological.Group)
wide$Ecological.Group <- factor(wide$Ecological.Group, levels = grp.levels) 
wide <- with(wide, wide[order(Expert, Ecological.Group),]) 

# Output results
write_csv(wide, "./analysis/data/Estimates_wide.csv")
#write_csv(long, "./analysis/data/derived/Estimates_long.csv") 


#' ## Summarize the number of expert estimates
#' Tabulate how many expert estimates there are for each ecological group
temp <- wide[, c(1, 2)] # Subset table.data to only include the columns "Expert" and "Ecological.Group"
spp.count <- table(temp$Ecological.Group)
write.csv(spp.count, "./analysis/results/Count_by_group.csv", row.names=FALSE)

#' Tabulate how many estimates there are for each strategy
temp2 <- subset(long, Estimate == "Best") # Subset to count how many experts provided estimates for each group and strategy
strat.levels <- unique(temp2$Strategy)
temp2$Strategy <- factor(temp2$Strategy,levels = strat.levels)
stgy.count <- table(temp2$Ecological.Group, temp2$Strategy)
write.csv(stgy.count, "./analysis/results/Count_by_strategy.csv") #need this


#####

#read the .csv files
temp <- read.csv(files[1], skip = skiplines, nrows = numrows)
temp <- temp %>% 
    select(all_of(1:numcols)) #%>%
    filter(!row_number() %in% c(1)) #remove empty rows
names(temp) <- lapply(temp[1, ], as.character) #Make first row into header
names(temp)[1] <- "Ecol.Groups"
names(temp)[2] <- "Estimate"
temp <- temp[-1,] 
temp <- data.frame(temp, fix.empty.names = TRUE) #rename empty colnames

#ecological groups
ecodata <- pull(temp, Ecol.Groups) #create a group of ecological names
ecodata <- data_frame(as.data.frame(ecodata)) #make them a data frame

#remove blank rows or NA from ecological groups
ecodata <- ecodata[!apply(ecodata == "", 1, all), ] #remove blank rows
#ecodata <- ecodata %>% drop_na() #remove NA

#create empty dataframe for output
byexpert <- data.frame() 
Expert <- list()
e = 0
df<-ecodata

for (i in 1:length(experts)){ # else use length(files) if all estimates are available
  temp2 <- read_csv(files[i], skip = skiplines)
  temp2 <- temp2 %>% 
    select(0:numcols) #%>%
    #filter(!row_number() %in% c(1))#shouldn't read a file in a loop? Do I need to to read each expert estimate?
  #names(temp) <- lapply(temp[1, ], as.character) #Make first row into header
  #temp <- temp[-1,] 
  temp2 <- data.frame(temp2, fix.empty.names = TRUE) #rename empty colnames
  colnames(temp2)[2]<- "Col" #gives column 2 a name
  temp2$Col <- stri_replace_all_regex(temp2$Col,
                                     pattern=c('Best', 'Highest', '\\<High\\>', 'Lowest', '\\<Low\\>'),
                                     replacement=c('Best guess', 'Upper', 'Upper', 'Lower', 'Lower'),
                                     vectorize=FALSE)
  temp2 <- temp2[!grepl("CHECK", temp2$Col),]
  temp2 <- temp2[!grepl("CONFIDENCE", temp2$Col),]
  e = e+1 #counter for number of experts/sheets
  
  #create data frame for loop output with row names for Ecological Groups
  #df<-ecodata
  
  #reorganize estimates
  for (h in 1:nrow(ecodata)) { #for all ecological groups (including example)
    n = 2 #starts from two, so eco groups are first column
    Expert <- append(Expert, e) #fills a list of expert numbers
    for (j in 3:ncol(temp2)) { #for all strategies (starts from 3rd col)
      for (k in 1:3) { #do this 3 times: best, lower, upper -> this should be changed if including confidence/CHECK
        df[h,n]= temp2[(h-1)*3+k,j] #number of rows - 1, 5 times + 3
        n = n+1
      } 
    }
  }
  
  byexpert <-rbind(byexpert, df)
}

#Change X's to NA
na_strings <- c("X", "X ", "x", "x ")
byexpert <- byexpert %>% 
  replace_with_na_all(condition=~.x %in% na_strings) 

#Add expert column
Expert <- as.character(Expert)
byexpert %>% add_column(newColname = "Expert")
byexpert$Expert <- Expert
byexpert <- byexpert %>% 
  select(Expert, everything()) #place expert column in 1st position

#Add header names: Best, Lowest, Highest 
#byexpert2 <- byexpert
n = 3
s = 1 #counter for strategy number
for(j in 3) {
  for(k in 1:3){ #best, lowest, highest
    names(byexpert)[n] <- paste(toString(temp2[k,2]))
    n = n+1
  } 
}
clist <- c("Best guess", "Lower", "Upper")
for(j in 1:nstrat){ #for each strategy; 
  for(k in clist){ #best_1, lowest_1, highest_1 etc - rows
    names(byexpert)[n] <- paste(k, "_", s)
    n = n+1
  } 
  s = s+1  
}

byexpert <- byexpert %>%
  dplyr::rename(`Ecological Group` = `ecodata`) 

bestguess_base <- byexpert[,grep("Best guess$", colnames(byexpert))]
lower_base <- byexpert[,grep("Lower$", colnames(byexpert))]
upper_base <- byexpert[,grep("Upper$", colnames(byexpert))]

# Find strategy column indices
bestguess <- grep("Best guess",colnames(byexpert))
lowest <- grep("Lower", colnames(byexpert))
highest <- grep("Upper", colnames(byexpert))

# For each relevant column, replace "b" with baseline values from the same row
# This needs to be fixed later - this was copied
for (i in 1:length(bestguess)) {
  bg_temp <- which(byexpert[,bestguess[i]]=="B" | byexpert[,bestguess[i]]=="b")
  byexpert[bg_temp,bestguess[i]] <- bestguess_base[bg_temp,]
  
  l_temp <- which(byexpert[,lowest[i]]=="B" | byexpert[,lowest[i]]=="b")
  byexpert[l_temp,lowest[i]] <- lower_base[l_temp,]
  
  u_temp <- which(byexpert[,highest[i]]=="B" | byexpert[,highest[i]]=="b")
  byexpert[u_temp,highest[i]] <- upper_base[u_temp,]
}

#' Standardize group labels if needed (this will be project specific)
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "example only, values are random")==1)] <- "EXAMPLE"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "GRASSLAND SPECIES")==1)] <- "Grassland Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "BURROW")==1)] <- "Burrow and Den Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "SAND DUNE SPECIES")==1)] <- "Sand Dune Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "WETLAND")==1)] <- "Wetland and Shorebird Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "AMPHIBIANS")==1)] <- "Amphibians"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "FISH SPECIES")==1)] <- "Fish Species"
byexpert$`Ecological Group`[which(str_detect(byexpert$`Ecological Group`, "HEALTHY")==1)] <- "Healthy Prairie Landscape"

byexpert <- byexpert[!grepl('EXAMPLE', byexpert$`Ecological Group`),]

names(byexpert) <- gsub(" _ ", "_", names(byexpert))
names(byexpert) <- gsub(" ", ".", names(byexpert))






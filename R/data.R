##########
# What: Data for forecasting interstate war battle deaths
# Date: August 2012
#
# Copyright 2012 Andreas Beger
# GNU GPL 3.0 <http://www.gnu.org/licenses/>
# Please reference Beger, Andreas, "R: Bayesian time-series count models", 
# <http://andybeger.wordpress.com>
###########

setwd("/Users/adbeger/Dropbox/Research/WarFatalities_IO/Rev2")

#
# Load data
#

library(foreign)
library(plyr)

# War participants list
war.part <- read.table("inputData/InterStateWarData_v4.0.csv", header = TRUE, 
  sep = ",")

# Create unique identifier
war.part$partID <- paste(sprintf("%03d", war.part$WarNum), 
  sprintf("%03d", war.part$ccode), sep="")

# Replace -8 with NA
war.part$StartYear2[war.part$StartYear2 == -8] <- NA
war.part$EndYear2[war.part$EndYear2 == -8] <- NA

# Create start and end year for each war
# There is one war, France in WW2, where there is a gap between start/end year 1/2
war.year <- war.part[, c("WarNum", "StartYear1", "StartYear2", "EndYear1", "EndYear2")]
war.daterange <- ddply(war.year, ~WarNum, summarize, start=min(StartYear1), end=max(EndYear1), end2=max(EndYear2))
war.daterange$end <- ifelse(!is.na(war.daterange$end2), war.daterange$end2, war.daterange$end)
war.daterange <- subset(war.daterange, select=-c(end2))

war.part <- merge(war.part, war.daterange, by="WarNum")
war.part <- subset(war.part, select=c(partID, WarNum, WarName, start, end, ccode, StateName, Side, Initiator, BatDeath))
rm(war.year)

# Expanded participant-year set
min.year <- min(war.part$start) - 1
max.year <- max(war.part$end) 
part.year <- expand.grid(partID=war.part$partID, year=min.year:max.year)
rm(max.year, min.year)

part.year <- merge()

#
# Functions
#

# Lookup NMC data
nmc.lookup <- function(ccode, year) {
  cond <- nmc$ccode==ccode & nmc$year==year
  if (any(cond)) {
    return(nmc[which(cond), 3:6])
  }
  else {
    return(c(NA, NA, NA, NA))
  }
}

End goal variables that need to be added:
  democracy
  capabilities
  power ratio
  issue salience
  number of states
  contiguity
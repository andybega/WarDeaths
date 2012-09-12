##########
# What: Data for forecasting interstate war battle deaths
# Date: September 2012
#
# Copyright 2012 Andreas Beger
# GNU GPL 3.0 <http://www.gnu.org/licenses/>
# Please reference Beger, Andreas, "R: Bayesian time-series count models", 
# <http://andybeger.wordpress.com>
###########

setwd("/Users/adbeger/Research/WarDeaths")

library(foreign)
library(plyr)
library(gdata)

##########
# War-participant list
#
##########

# War participants list
war.part <- read.table("inputData/InterStateWarData_v4.0.csv", header = TRUE, 
  sep = ",")

# Create unique identifier
war.part$partID <- paste(sprintf("%03d", war.part$WarNum), 
  sprintf("%03d", war.part$ccode), sprintf("%04d", war.part$StartYear1), sep="")

# Fix minor issues. If war-participation has two periods, set end to second end.
war.part <- replace(war.part, war.part==-8, NA)
war.part$part.start <- war.part$StartYear1
war.part$part.end <- ifelse(is.na(war.part$EndYear2), war.part$EndYear1, war.part$EndYear2)

# Create start and end year for each war
# There is one war, France in WW2, where there is a gap between start/end year 1/2
war.year <- war.part[, c("WarNum", "StartYear1", "StartYear2", "EndYear1", "EndYear2")]
war.daterange <- ddply(war.year, ~WarNum, summarize, start=min(StartYear1), end=max(EndYear1), end2=max(EndYear2))
war.daterange$end <- ifelse(!is.na(war.daterange$end2), war.daterange$end2, war.daterange$end)
war.daterange <- subset(war.daterange, select=-c(end2))
war.part <- merge(war.part, war.daterange, by="WarNum")
rm(war.year, war.daterange)
war.part <- subset(war.part, select=c(partID, WarNum, WarName, start, end, ccode, StateName, part.start, part.end, Side, Initiator, BatDeath))

##########
# War-participant-year list
# Includes start year - 1 to end year of war, regardless of participation
# start/end year, which may be different.
#
##########

# Expanded participant-year set
min.year <- min(war.part$start) - 1
max.year <- max(war.part$end) 
part.year <- expand.grid(partID=war.part$partID, year=min.year:max.year)
rm(max.year, min.year)
part.year <- merge(part.year, war.part, by="partID")

# Drop participan years that are not in period from start year - 1 to end year.
part.year <- subset(part.year, (part.year$year >= part.year$start - 1) & 
  (part.year$year <= part.year$end))

# Missing values


##########
# NMC data
#
##########

nmc <- read.table("inputData/NMC_v4_0.csv", header = TRUE, sep = ",")
nmc <- subset(nmc, select=c(ccode, year, irst, milex, milper, pec, tpop, upop, cinc))
nmc[nmc==-9] <- NA

part.year <- join(part.year, nmc, by=c("ccode", "year"), type="left")
rm(nmc)

##########
# Polity data
#
##########

p4 <- read.xls('inputData/p4v2010.xls')
p4 <- subset(p4, select=c(ccode, year, polity))

part.year <- join(part.year, p4, by=c('ccode', 'year'), type='left')

match.polity <- function(cow, pol) {
  print(part.year$polity[part.year$ccode==cow[1] & part.year$year==cow[2]])
  print(p4$polity[p4$ccode==pol[1] & p4$year==pol[2]])
  part.year$polity[part.year$ccode==cow[1] & part.year$year==cow[2]] <- p4$polity[p4$ccode==pol[1] & p4$year==pol[2]]
}

# Austria in 1847
part.year$polity[part.year$ccode==300 & part.year$year==1847] <- p4$polity[p4$ccode==305 & p4$year==1847]




# reorder data
part.year <- part.year[order(part.year$WarNum, part.year$Side, part.year$ccode, part.year$year), ]

##########
# Aggregate to participant level (no time dimension)
#
##########



# Function to lag by unique ID
PanelLag <- function(x, lag) {
  for (i in levels(partID)) {
    x <- x[]
    x.out <- c(NA, x[])
  }
}

need function that can lag panel variables.

need to look at imputation
test <- amelia(nmc, m=1, ts='year', cs='ccode', polytime=2, intercs=TRUE)

# temporary
milper <- part.year$milper[part.year$year==part.year$start-1 & ]


End goal variables that need to be added:
  democracy
  capabilities
  power ratio
  issue salience
  number of states
  contiguity
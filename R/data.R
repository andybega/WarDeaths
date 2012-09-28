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
library(zoo)

# List of state names for reference
state.names <- read.xls('data/COW State list.xls')
state.names <- state.names[!duplicated(state.names), ]
state.names$StateNme <- trim(state.names$StateNme)

##########
# War-participant list
#
##########

# War participants list
war.part <- read.table("data/InterStateWarData_v4.0.csv", header = TRUE, 
  sep = ",")

# Missing values
war.part$BatDeath[war.part$BatDeath==-9] <- NA

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
# Includes part start year - 1 to part end year
#
##########

# Expanded participant-year set
min.year <- min(war.part$part.start) - 1
max.year <- max(war.part$part.end) 
part.year <- expand.grid(partID=war.part$partID, year=min.year:max.year)
rm(max.year, min.year)
part.year <- merge(part.year, war.part, by="partID")

# Drop participan years that are not in period from start year - 1 to end year.
part.year <- subset(part.year, (part.year$year >= part.year$part.start - 1) & 
  (part.year$year <= part.year$part.end))

##########
# NMC data
#
##########

nmc <- read.table("data/NMC_v4_0.csv", header = TRUE, sep = ",")
nmc <- subset(nmc, select=c(ccode, year, irst, milex, milper, pec, tpop, upop, cinc))
nmc[nmc==-9] <- NA

part.year <- join(part.year, nmc, by=c("ccode", "year"), type="left")

##########
# Polity data
#
##########

p4 <- read.xls('data/p4v2010.xls')
p4 <- subset(p4, select=c(ccode, year, polity))

# # Carry forward last regime value for transition period (-88)
p4$polity[p4$polity==-88] <- NA
p4$polity <- unlist(by(p4$polity, p4$ccode, na.locf, na.rm=F))

# Set -66 and -77 to -10 polity score
p4$polity[p4$polity==-66] <- -10
p4$polity[p4$polity==-77] <- -10

# Recode some ccodes for consistency with COW state system membership; 
# for good merge
p4$ccode[p4$ccode==305 & p4$year %in% c(1816:1918)] <- 300  # Austria-Hungary/Austria
p4$ccode[p4$ccode==324 & p4$year %in% c(1816:1860)] <- 325  # Italy/Piedmont-Sardinia
p4$ccode[p4$ccode==342 & p4$year %in% c(1816:1920)] <- 345  # Yugoslavia/Serbia
p4$ccode[p4$ccode==347 & p4$year %in% c(1992:2005)] <- 345  # Yugoslavia/Yugoslavia
p4$ccode[p4$ccode==364 & p4$year %in% c(1923:1991)] <- 365  # Russia/USSR
p4$ccode[p4$ccode==529 & p4$year %in% c(1994:2010)] <- 530  # Ethiopia
p4$ccode[p4$ccode==769 & p4$year %in% c(1947:1971)] <- 770  # Pakistan
p4$ccode[p4$ccode==818 & p4$year %in% c(1977:2010)] <- 816  # North Vietnam/Vietnam
  
# Merge in polity
part.year <- join(part.year, p4, by=c('ccode', 'year'), type='left')

# Fix merge for state-years outside polity (take closes year polity score)
part.year$polity[part.year$ccode==329 & part.year$year==1861] <- p4$polity[p4$ccode==329 & p4$year==1860] # Two Sicilies
part.year$polity[part.year$ccode==367 & part.year$year %in% c(1917:1919)] <- p4$polity[p4$ccode==367 & p4$year==1920] # Latvia
part.year$polity[part.year$ccode==540 & part.year$year==1974] <- p4$polity[p4$ccode==540 & p4$year==1975] # Angola
part.year$polity[part.year$ccode==750 & part.year$year %in% c(1946:1949)] <- p4$polity[p4$ccode==750 & p4$year==1950] # India
part.year$polity[part.year$ccode==770 & part.year$year==1946] <- p4$polity[p4$ccode==770 & p4$year==1947] # Pakistan
part.year$polity[part.year$ccode==666 & part.year$year==1947] <- p4$polity[p4$ccode==666 & p4$year==1948] # Israel
part.year$polity[part.year$ccode==651 & part.year$year %in% c(1881:1882)] <- p4$polity[p4$ccode==651 & p4$year==1922] # Egypt
part.year$polity[part.year$ccode %in% c(240, 273, 275, 280) & part.year$year %in% c(1865:1866)] <- -7 # Minor German states during unification wars
part.year$polity[part.year$ccode==346 & part.year$year==1991] <- p4$polity[p4$ccode==346 & p4$year==1992] # Bosnia

# reorder data
part.year <- part.year[order(part.year$WarNum, part.year$Side, part.year$ccode, part.year$year), ]

##########
# Create lagged variables
# drop lead part years after this
##########

# Function to lag by unique ID
panelLag <- function(var, id, data) {
  x.out <- unlist(by(data[, var], data[, id], function(x) c(NA, x[1:(length(x)-1)])))
  return(x.out)
}

part.year <- part.year[order(part.year$partID, part.year$year), ]
part.year$polity.l1 <- panelLag('polity', 'partID', part.year)
part.year$milper.l1 <- panelLag('milper', 'partID', part.year)
part.year$milex.l1 <- panelLag('milex', 'partID', part.year)
part.year$tpop.l1 <- panelLag('tpop', 'partID', part.year)

# Drop participan years that are not in period from start year to end year.
part.year <- subset(part.year, (part.year$year >= part.year$part.start) & 
  (part.year$year <= part.year$part.end))

##########
# Capability variables
# make sure lead years are dropped
##########

## Number of state on each side
part.year <- ddply(part.year, .(WarNum, year, Side), transform, states.side=length(WarNum))

## Total states in war at any given time
part.year <- ddply(part.year, .(WarNum, year), transform, states.total=length(WarNum))

## Total capabilities per side
part.year <- ddply(part.year, .(WarNum, year, Side), transform, milper.l1.side=sum(milper.l1))
part.year <- ddply(part.year, .(WarNum, year, Side), transform, milex.l1.side=sum(milex.l1))
part.year <- ddply(part.year, .(WarNum, year, Side), transform, tpop.l1.side=sum(tpop.l1))

## Ratios


# Test case Crimean War
# test <- subset(part.year, WarNum==22)

##########
# Participant level (no time dimension)
#
##########

# Subset part.year and merge
merge.vars <- c('partID', 'year', "irst", "milex", "milper", "pec", "tpop", 
                "upop", "cinc", "polity.l1", 'milper.l1', 'milex.l1', 'tpop.l1',
                'states.side', 'states.total', 'milper.l1.side', 
                'milex.l1.side', 'tpop.l1.side'
                )
merge.data <- subset(part.year, select=merge.vars)
war.part$year <- war.part$part.start
war.part <- merge(war.part, merge.data, by=c('partID', 'year'), type='left')
war.part <- subset(war.part, select=-year)
war.part <- war.part[with(war.part, order(WarNum, Side, part.start, ccode)), ]

save(war.part, file='data/war_part.RData')
save.image(file='data/workspace.RData')
## end

# need to look at imputation
# test <- amelia(nmc, m=1, ts='year', cs='ccode', polytime=2, intercs=TRUE)

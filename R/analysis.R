##########
# What: Analysis for forecasting interstate war battle deaths
# Date: August 2012
#
# Copyright 2012 Andreas Beger
# GNU GPL 3.0 <http://www.gnu.org/licenses/>
# Please reference Beger, Andreas, "R: Bayesian time-series count models", 
# <http://andybeger.wordpress.com>
###########


part.year$duration <- part.year$end - part.year$start + 1
part.year$dead.per.year <- ifelse(part.year$year < part.year$start, 0, 
  part.year$BatDeath/part.year$duration)
dead.per.year <- aggregate(part.year$dead.per.year, by=list(part.year$year), sum)
colnames(dead.per.year)[1] <- "year"
year <- data.frame(year=1816:2007)
dead.per.year <- join(year, dead.per.year, by="year", type="left")
rm(year)
dead.per.year[is.na(dead.per.year$x), "x"] <- 0
dead.per.year <- ts(dead.per.year, start=1816)
plot(dead.per.year[, "x"])
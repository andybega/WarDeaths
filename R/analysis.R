##########
# What: Analysis for forecasting interstate war battle deaths
# Date: August 2012
#
# Copyright 2012 Andreas Beger
# GNU GPL 3.0 <http://www.gnu.org/licenses/>
# Please reference Beger, Andreas, "R: Bayesian time-series count models", 
# <http://andybeger.wordpress.com>
###########

library(ggplot2)
library(MASS)

if (!exists('war.part')) load('data/war_part.RData')

############
# Rough plot of dead per year
#
############

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

############
# DV characteristics
#
############

# Fit normal to log10 y using MLE
fitdistr(as.numeric(na.omit(log10(war.part$BatDeath[war.part$BatDeath>0]))), 'normal')

# Plot histogram with fitted normal
p <- ggplot(data=war.part, aes(BatDeath)) 
p <- p + geom_histogram(aes(y=..density..), binwidth=0.2, color='black', fill='white') + 
  stat_function(fun=dnorm, color='black', arg=list(mean=3.48, sd=1.16)) +
  scale_x_log10() 
p

############
# Models
#
############

war.part <- subset(war.part, complete.cases(war.part))

log.lm <- lm(log10(BatDeath + 1) ~ polity.l1 + log(milper.l1) + log(milex.l1) + log(tpop.l1) + log(milper.l1.side) + log(tpop.l1.side), data=war.part)

# Fitted values
pred.lm <- predict(log.lm, level=0.8, interval='confidence')
pred.lm <- round(10^pred.lm)

# combine
war.part <- cbind(war.part, pred.lm)

# Label and ordering variable
war.part$label <- with(war.part, factor(partID, levels=partID[order(BatDeath)]))

# Plot fitted range vs. observed values.
p <- ggplot(war.part) +
  geom_pointrange(aes(x=label, y=fit, ymin=lwr, ymax=upr)) +
  geom_point(aes(x=label, y=BatDeath), color='blue') +
  scale_y_log10(name='log10 fitted values') + scale_x_discrete(name='') +
  theme(axis.text.x=element_text(angle=90))
  
p




glm.nb(BatDeath ~ 1, data=war.part)
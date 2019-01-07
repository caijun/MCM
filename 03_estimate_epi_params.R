rm(list = ls())

source("R/threshold.R")
source("R/segment.R")
source("R/curvature.R")
source("R/helper.R")

load("output/Japan_Flu_Sentinel.rda")

# regard Japan as a prefecture
pref.flu1 <- rbind(nation.flu1, pref.flu1)

library(tidyverse)

# Shoji.etal-TohokuJExpMed-2011
# An influenza epidemic at each prefecture was defined as occurring when >=1.0 
# influenza cases per sentinel were reported for three consecutive weeks. The 
# first week of the three consecutive weeks is defined as the week of epidemic 
# onset. Epidemic parameters estimated by this method are used as the gold 
# standard.

# first, need to label influenza seasons, running from week 35 to the 
# upcoming week 34
library(ISOweek)
year.range <- range(pref.flu1$year)
year <- seq(year.range[1] - 1, year.range[2] + 1, 1)
breaks <- ISOweek2date(paste0(year, "-W35-7"))
labels <- paste(year[-length(year)], year[-1], sep = "/")
pref.flu1$season <- cut(pref.flu1$weekending, breaks, labels = labels)
pref.flu1 <- pref.flu1 %>% 
  mutate(season = as.character(season)) %>%
  group_by(Prefecture, season) %>% 
  dplyr::mutate(weeknum_of_season = 1:n()) %>% 
  ungroup()

# linear interpolate epidemic start and end dates by prefecture and season
# x <- subset(pref.flu1, season == "2012/2013" & Prefecture == "Okinawa")
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Tokyo")
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Kyoto")
# x <- subset(pref.flu1, season == "2012/2013" & Prefecture == "Oita")
# x <- subset(pref.flu1, season == "2013/2014" & Prefecture == "Mie")
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Hokkaido")
# x <- subset(pref.flu1, season == "2012/2013" & Prefecture == "Hiroshima")
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Aichi")
x <- subset(pref.flu1, season == "2013/2014" & Prefecture == "Akita")
# epidemic onset number esitmated by using MCM are very large
# without setting the upper threshold of 5.0 for certain prefectures
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Hokkaido")
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Iwate")
# x <- subset(pref.flu1, season == "2016/2017" & Prefecture == "Okinawa")
ec <- data.frame(t = x$weeknum_of_season, y = x$flu.sentinel)
interp.ec(ec)
segment.ec(ec)
# better results without smoothing
curvature.ec(ec, smoothing = FALSE)
curvature.ec(ec, smoothing = TRUE)

# following analyses are on week unit
# use empirical threhsold method (ETM)
library(plyr)
epi.params.etm <- ddply(pref.flu1, .(Prefecture, season), function(df) {
  interp.ec(data.frame(t = df$weeknum_of_season, y = df$flu.sentinel))
})

epi.params.etm <- epi.params.etm %>% 
  mutate(pre.season.ending = pre.season.ending(season)) %>% 
  mutate(epi.start.date = pre.season.ending + epi.start * 7, 
         epi.end.date = pre.season.ending + epi.end * 7, 
         epi.peak.date = pre.season.ending + epi.peak * 7)

# use segmented regression method (SRM)
epi.params.srm <- ddply(pref.flu1, .(Prefecture, season), function(df) {
  segment.ec(data.frame(t = df$weeknum_of_season, y = df$flu.sentinel))
})

epi.params.srm <- epi.params.srm %>% 
  mutate(pre.season.ending = pre.season.ending(season)) %>% 
  mutate(epi.start.date = pre.season.ending + epi.start * 7, 
         epi.end.date = pre.season.ending + epi.end * 7, 
         epi.peak.date = pre.season.ending + epi.peak * 7)

# use maximum curvature method (MCM)
epi.params.mcm <- ddply(pref.flu1, .(Prefecture, season), function(df) {
  curvature.ec(data.frame(t = df$weeknum_of_season, y = df$flu.sentinel), 
               smoothing = FALSE)
})

epi.params.mcm <- epi.params.mcm %>% 
  mutate(pre.season.ending = pre.season.ending(season)) %>% 
  mutate(epi.start.date = pre.season.ending + epi.start * 7, 
         epi.end.date = pre.season.ending + epi.end * 7, 
         epi.peak.date = pre.season.ending + epi.peak * 7)

save(epi.params.etm, epi.params.srm, epi.params.mcm, 
     pref.flu1, file = "output/Japan_Pref_Epi_Params.rda")

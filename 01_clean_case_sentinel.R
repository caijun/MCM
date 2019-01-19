rm(list = ls())

# Japan Influenza
files <- list.files("data/influenza", pattern = ".*teiten[[:digit:]]{2}.csv$", 
                    recursive = TRUE, include.dirs = TRUE, full.names = TRUE)
# the reported diseases have been changed since 2018-W01
years <- substr(files, 22, 25)
files1 <- files[years < 2018]
files2 <- files[years >= 2018]

teiten1 <- do.call(rbind, lapply(files1, function(file, flu.sentinel = TRUE) {
  csv <- read.csv(file, stringsAsFactors = FALSE, fileEncoding = "cp936")
  week <- csv[1, 1]
  date <- csv[1, 7]
  disease <- unname(unlist(csv[3, seq(2, 37, by = 2)]))
  header <- c(csv[3, 1], disease)
  cases <- csv[-(1:4), c(1, seq(2, 37, by = 2))]
  names(cases) <- header
  cases <- cbind(cases, date, week)
  sentinel <- csv[-(1:4), c(1, seq(3, 37, by = 2))]
  names(sentinel) <- header
  sentinel <- cbind(sentinel, date, week)
  if (flu.sentinel) {
    return(sentinel)
  } else{
    return(cases)
  }
}))

teiten2 <- do.call(rbind, lapply(files2, function(file, flu.sentinel = TRUE) {
  csv <- read.csv(file, stringsAsFactors = FALSE, fileEncoding = "cp936")
  week <- csv[1, 1]
  date <- csv[1, 7]
  disease <- unname(unlist(csv[3, seq(2, 37, by = 2)]))
  header <- c(csv[3, 1], disease)
  cases <- csv[-(1:4), c(1, seq(2, 37, by = 2))]
  names(cases) <- header
  cases <- cbind(cases, date, week)
  sentinel <- csv[-(1:4), c(1, seq(3, 37, by = 2))]
  names(sentinel) <- header
  sentinel <- cbind(sentinel, date, week)
  if (flu.sentinel) {
    return(sentinel)
  } else{
    return(cases)
  }
}))
setdiff(names(teiten1), names(teiten2))
setdiff(names(teiten2), names(teiten1))

teiten <- rbind(teiten1[c(1, 2, 20, 21)], teiten2[c(1, 2, 20, 21)])
names(teiten)[2] <- "flu.sentinel"

library(tidyverse)
library(lubridate)
library(stringr)
library(ISOweek)
flu <- teiten %>% 
  dplyr::select(Prefecture, flu.sentinel, date, week) %>% 
  dplyr::mutate(flu.sentinel = as.numeric(ifelse(flu.sentinel == "-", NA, flu.sentinel)), 
         date = gsub("Data collected as of", "", date), 
         date = trimws(date), 
         date_of_report = as.Date(date, format = "%B %d, %Y"), 
         year = as.integer(str_sub(week, -4)), 
         weeknum = as.integer(str_sub(week, end = -14)), 
         weeknum1 = str_pad(weeknum, 2, side = "left", pad = "0"), 
         weekending = ISOweek2date(paste0(year, "-W", weeknum1, "-7")), 
         isoweek = ISOweek(weekending)) %>%
  select(-date, -week)

nation.flu <- flu %>%
  dplyr::filter(Prefecture == "Total No.") %>%
  mutate(Prefecture = "Japan")
subset(nation.flu, is.na(date_of_report))

p <- ggplot(nation.flu, aes(weekending, flu.sentinel))
p + geom_line() + 
  geom_hline(yintercept = 1.0, color = "red", linetype = "dashed") + 
  labs(title = "Japan", x = "Weekending date", 
       y = "Number of influenza cases per sentinel")

pref.flu <- flu %>% 
  dplyr::filter(Prefecture != "Total No.")
subset(pref.flu, is.na(date_of_report))

# data at the national level is not simply the average of the data at the 
# prefectural level.
# # cases per week per sentinel = sum(# cases per week)/(# sentinels) at both 
# the national and prefectural levels.
x <- pref.flu %>% 
  mutate(flu.sentinel = ifelse(is.na(flu.sentinel), 0, flu.sentinel)) %>% 
  group_by(isoweek) %>% 
  dplyr::summarise(flu.sentinel = mean(flu.sentinel))

# study period 2012 week 35 to 2018 week 34, namely 2012-09-02 to 2018-08-26 in 
# terms of weekedning date
nation.flu1 <- nation.flu %>% 
  dplyr::filter(weekending >= "2012-09-01" & weekending <= "2018-09-01") %>% 
  select(-date_of_report, -weeknum1) %>% 
  mutate(month = format(weekending, format = "%m")) %>%
  mutate(month = as.integer(month)) %>% 
  mutate(flu.sentinel = ifelse(is.na(flu.sentinel), 0, flu.sentinel)) %>%
  select(Prefecture, weekending, year, month, weeknum, isoweek, flu.sentinel)

pref.flu1 <- pref.flu %>% 
  dplyr::filter(weekending >= "2012-09-01" & weekending <= "2018-09-01") %>% 
  select(-date_of_report, -weeknum1) %>% 
  mutate(month = format(weekending, format = "%m")) %>%
  mutate(month = as.integer(month)) %>% 
  mutate(flu.sentinel = ifelse(is.na(flu.sentinel), 0, flu.sentinel)) %>%
  select(Prefecture, weekending, year, month, weeknum, isoweek, flu.sentinel)

save(nation.flu1, pref.flu1, file = "output/Japan_Flu_Sentinel.rda")

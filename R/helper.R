# get the ending date of the previous season
pre.season.ending <- function(season, ending.week = 34) {
  year <- substr(season, 1, 4)
  ending.date <- ISOweek2date(paste0(year, "-W", ending.week, "-7"))
  return(ending.date)
}

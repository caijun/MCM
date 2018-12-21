# linearly interpolate epidemic start (or end) time for an increase (decrease) 
# half epidemic curve
#' @param half the half epidemic curve, a data.frame with columns `t` and `y`.
#' @param th threshold for epidemic.
#' @param n consecutive observations
interp.half.ec <- function(half, th = 1.0, n = 3, trend = c("increase", "decrease")) {
  if (trend == "decrease") half <- half[nrow(half):1, ]
  
  require(zoo)
  rollmin <- rollapply(half$y, width = n, min, align = "right")
  idx <- which(rollmin > th)[1]
  # minimum of flu.sentinel is greater than 1.0, for example Okinawa
  if (is.na(idx)) return(NA)
  if (idx == 1) return(NA)
  idx <- c(idx - 1, idx)
  require(Hmisc)
  res <- approxExtrap(x = half$y[idx], y = half$t[idx], xout = th)
  onset <- res$y
  return(onset)
}

# linear interpolate epidemic start and end weeks for a whole epidemic curve
#' @param ec the whole epidemic curve, , a data.frame with columns `t` and `y`.
interp.ec <- function(ec, th = 1.0, n = 3) {
  pk.idx <- which.max(ec$y)
  epi.peak <- ec$t[pk.idx]
  epi.peak.num <- ec$y[pk.idx]
  half1 <- ec[1:pk.idx, ]
  half2 <- ec[pk.idx:nrow(ec), ]
  epi.start <- interp.half.ec(half1, trend = "increase", th = th, n = n)
  epi.start.num <- ifelse(is.na(epi.start), NA, th)
  epi.end <- interp.half.ec(half2, trend = "decrease", th = th, n = n)
  epi.end.num <- ifelse(is.na(epi.end), NA, th)
  epi.duration <- epi.end - epi.start + 1
  dur.idx <- ec$t >= epi.start & ec$t <= epi.end
  # mean intensity during epidemic duration
  epi.duration.num <- sum(ec$y[dur.idx]) / sum(dur.idx)
  return(data.frame(epi.start, epi.start.num, 
                    epi.end, epi.end.num, 
                    epi.peak, epi.peak.num, 
                    epi.duration, epi.duration.num))
}

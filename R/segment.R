# estimate epidemic start (or end) time for an increase (decrease) 
# half epidemic curve using broken-line model
#' @param half the half epidemic curve, expressed as time series of observations
segment.half.ec <- function(half) {
  require(segmented)
  fit.lm <- lm(y ~ t, data = half)
  fit.seg <- segmented(fit.lm, seg.Z = ~t, psi = half$t[2])
  idx1 <- which(half$t == floor(fit.seg$psi[2]))
  idx2 <- which(half$t == ceiling(fit.seg$psi[2]))
  # linearly interpolate to obtain the y value at epi.start
  require(Hmisc)
  res <- approxExtrap(x = half$t[c(idx1, idx2)], y = half$y[c(idx1, idx2)], 
                      xout = fit.seg$psi[2])
  yval <- res$y
  return(c(fit.seg$psi, yval))
}

# estimate epidemic parameters for a whole epidemic curve using broken-line model
#' @param ec the whole epidemic curve, expressed as time series of observations
segment.ec <- function(ec) {
  pk.idx <- which.max(ec$y)
  epi.peak <- ec$t[pk.idx]
  epi.peak.num <- ec$y[pk.idx]
  half1 <- ec[1:pk.idx, ]
  half2 <- ec[pk.idx:nrow(ec), ]
  psi <- segment.half.ec(half1)
  epi.start <- psi[2]
  epi.start.sd <- psi[3]
  epi.start.num <- psi[4]
  psi <- segment.half.ec(half2)
  epi.end <- psi[2]
  epi.end.sd <- psi[3]
  epi.end.num <- psi[4]
  epi.duration <- epi.end - epi.start + 1
  dur.idx <- ec$t >= epi.start & ec$t <= epi.end
  # mean intensity during epidemic duration
  epi.duration.num <- sum(ec$y[dur.idx]) / sum(dur.idx)
  return(data.frame(epi.start, epi.start.num, 
                    epi.end, epi.end.num, 
                    epi.peak, epi.peak.num, 
                    epi.duration, epi.duration.num, 
                    epi.start.sd, epi.end.sd))
}

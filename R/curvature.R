source("R/curvature_np.R")

# linearly extrapolate to pad epidemic curve at the front and rear
pad.ec <- function(ec, pad = 2) {
  require(Hmisc)
  front <- approxExtrap(x = ec$t[1:2], y = ec$y[1:2], xout = ec$t[1] - pad:1)
  rear <- approxExtrap(x = ec$t[(nrow(ec) - 1):nrow(ec)], 
                       y = ec$y[(nrow(ec) - 1):nrow(ec)], 
                       xout = ec$t[nrow(ec)] + 1:pad)
  ec1 <- rbind(data.frame(t = front$x, y = front$y), 
               ec, data.frame(t = rear$x, y = rear$y))
  return(ec1)
}

# calculate curvature at each observation point
calc.curvature <- function(ec, n = 5, upper = 5.0) {
  pad <- (n - 1) / 2
  # linearly extrapolate to pad epidemic curve at the front and rear
  ec1 <- pad.ec(ec, pad)
  # fit circle at each observation point
  res <- NULL
  for (i in 1:nrow(ec)) {
    j <- i + pad
    x <- ec1$t[(j - pad):(j + pad)]
    y <- ec1$y[(j - pad):(j + pad)]
    res0 <- fit.circle(x, y)
    res <- rbind(res, res0)
  }
  # calculate curvature at each observation point
  res$curvature <- 1 / res$r
  
  # filter out those undesirable curvatures
  pk.idx <- which.max(ec$y)
  half1 <- res$theta[1:pk.idx]
  half2 <- res$theta[(pk.idx + 1):length(res$theta)]
  indicator <- c(I(half1 >= 0 & half1 <= 90), I(half2 >= 270 & half2 <= 360))
  res$new.curvature <- res$curvature * indicator * (ec$y <= upper)
  
  return(res)
}

# estimate epidemic parameters for a whole epidemic curve 
# using curvature-based method
#' @param ec the whole epidemic curve, expressed as time series of observations
#' @param n number of points used for fittign circle
#' @param upper we are pretty sure that exceeding the upper threshold, an epidemic has already been onset.
#' @param smoothing logical indicator controling whether smoothing epidemic curve using Savitzky#' -Golay smoothing filters
curvature.ec <- function(ec, n = 5, upper = 5.0, smoothing = FALSE) {
  if (smoothing) {
    require(signal)
    sg <- sgolay(p = 1, n = 3, m = 0)
    ec$y <- filter(sg, ec$y)
  }
  res <- calc.curvature(ec, n = n, upper = upper)
  
  pk.idx <- which.max(ec$y)
  epi.peak <- ec$t[pk.idx]
  epi.peak.num <- ec$y[pk.idx]
  start.idx <- which.max(res$new.curvature[1:pk.idx])
  epi.start <- res$tp_x[start.idx]
  epi.start.num <- res$tp_y[start.idx]
  end.idx <- pk.idx + which.max(res$new.curvature[(pk.idx + 1):length(res$new.curvature)])
  epi.end <- res$tp_x[end.idx]
  epi.end.num <- res$tp_y[end.idx]
  epi.duration <- epi.end - epi.start + 1
  dur.idx <- ec$t >= epi.start & ec$t <= epi.end
  # mean intensity during epidemic duration
  epi.duration.num <- sum(ec$y[dur.idx]) / sum(dur.idx)
  return(data.frame(epi.start, epi.start.num, 
                    epi.end, epi.end.num, 
                    epi.peak, epi.peak.num, 
                    epi.duration, epi.duration.num))
}
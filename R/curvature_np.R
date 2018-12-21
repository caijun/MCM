# given a load of points with x, y coordinates, we can estimate the radius
# of curvature by fitting a circle to them using Pratt algorithm.
fit.circle <- function(x, y) {
  n <- length(x) # number of data points
  # the centriod of the data points
  mx <- mean(x)
  my <- mean(y)
  centroid <- c(mx, my)
  X <- x - mx
  Y <- y - my
  Z <- X^2 + Y^2
  
  Mxx <- mean(X^2)
  Myy <- mean(Y^2)
  Mxy <- mean(X*Y)
  Mxz <- mean(X*Z)
  Myz <- mean(Y*Z)
  Mzz <- mean(Z*Z)
  
  # compute the coefficients of the characteristic polynomial
  Mz <- Mxx + Myy
  Cov_xy <- Mxx * Myy - Mxy * Mxy
  Mxz2 <- Mxz^2
  Myz2 <- Myz^2
  
  A2 <- 4 * Cov_xy - 3 * Mz^2 - Mzz
  A1 <- Mzz * Mz + 4 * Cov_xy * Mz - Mxz2 - Myz2 - Mz^3
  A0 <- Mxz2 * Myy + Myz2 * Mxx - Mzz * Cov_xy - 2 * Mxz * Myz * Mxy + Mz^2 * Cov_xy
  A22 <- A2 + A2
  
  epsilon <- 1e-12
  IterMax <- 20
  xnew <- 0
  ynew <- 1e+20
  # Newton's method starting at x=0
  for (i in 1:IterMax) {
    yold <- ynew
    ynew <- A0 + xnew * (A1 + xnew * (A2 + 4 * xnew^2))
    if (abs(ynew) > abs(yold)) {
      cat("Newton-Pratt goes wrong direction: |ynew| > |yold|")
      xnew = 0
      break
    }
    Dy <- A1 + xnew * (A22 + 16 * xnew^2)
    xold <- xnew
    xnew <- xold - ynew / Dy
    if (xnew != 0 & abs((xnew - xold) / xnew) < epsilon) break
    if (i > IterMax) {
      cat("Newton-Pratt will not converge")
      xnew = 0
    }
    if (xnew < 0) {
      sprintf('Newton-Pratt negative root: x = %f\n', xnew)
      xnew <- 0
    }
  }
  
  # compute the circle parameters
  DET <- xnew^2 - xnew * Mz + Cov_xy
  cx <- Mxz * (Myy - xnew) - Myz * Mxy
  cy <- Myz * (Mxx - xnew) - Mxz * Mxy
  if (DET != 0) {
    c <- c(cx, cy) / DET / 2
    center <- c + centroid
    r <- sqrt(t(c) %*% c + Mz + 2 * xnew)
    # normal vector from the tangent point (tp_x, tp_y) to the circle center (x0, y0)
    # tp_x is the same as the middle of x
    x0 <- center[1]
    y0 <- center[2]
    # current point (xk, yk)
    xk <- x[(n + 1) / 2]
    yk <- y[(n + 1) / 2]
    if (xk != x0) {
      # slope of the line from (xk, yk) to (x0, y0)
      a <- (yk - y0) / (xk - x0)
      if (x0 > xk) {
        tp_x <- x0 - sqrt(r^2 / (1 + a^2))
      } else {
        tp_x <- x0 + sqrt(r^2 / (1 + a^2))
      }
    } else {
      tp_x <- xk
    }
    
    if (y0 > yk) {
      tp_y <- y0 - sqrt(r^2 - (tp_x - x0)^2)
    } else {
      tp_y <- y0 + sqrt(r^2 - (tp_x - x0)^2)
    }
    tp <- c(tp_x, tp_y)
    normal <- center - tp
    normal_x <- normal[1]
    normal_y <- normal[2]
    # the counterclockwise angle (a value in degrees between 0 and 360) between 
    # the x-axis and normal vector, namely the vector from the tangent point to 
    # to the circle center
    theta <- atan2(normal_y, normal_x) / pi * 180
    if (normal_y < 0) theta <- theta + 360
    # return the directional angle of tangent vector
    theta <- (theta - 90) %% 360
    return(data.frame(x0, y0, r, tp_x, tp_y, normal_x, normal_y, theta))
  } else {
    # straight line
    tp_x <- x[(n + 1) / 2]
    tp_y <- y[(n + 1) / 2]
    normal_x <- 0
    normal_y <- 1
    theta <- 0
    return(data.frame(x0 = tp_x, y0 = Inf, r = Inf, tp_x, tp_y, normal_x, normal_y, 
                      theta))
  }
}

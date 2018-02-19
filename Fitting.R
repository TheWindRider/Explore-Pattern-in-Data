library(fitdistrplus)

fit_weibull_normal <- function(v) {
  offset <- floor(min(v))
  
  dist_weibull <- fitdist(v - offset, "weibull")
  dist_normal <- fitdist(v, "norm")
  
  if (dist_weibull$aic < dist_normal$aic) {
    cat(sprintf("Weibull: shape=%.1f, scale=%.1f, offset=%d \n", 
                dist_weibull$estimate["shape"], dist_weibull$estimate["scale"], offset))
  }
  else {
    cat(sprintf("Normal: mean=%.1f, sd=%.1f \n", 
                dist_normal$estimate["mean"], dist_normal$estimate["sd"]))
  }
}
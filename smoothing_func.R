
# butterworth filter for smoothing traces
butterworth_filt <- function(x, n, W) {
  bf <- signal::butter(n, W)
  gd <- suppressWarnings(round(signal::grpdelay(bf, Fs = 100)$gd[1]))
  if (!gd >= 0) 
    stop("gd must be >= 0.")
  x_b <- signal::filter(bf, x)
  x_b_gd <- c(x_b[(gd + 1):length(x_b)], rep(x_b[length(x_b)], gd))
  x_b_gd
}
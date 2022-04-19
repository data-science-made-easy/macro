plot_thermometer <- function(sum_abs_impulses) {
  shock_threshold <- .8 # where red begins
  shock_orange    <- .6 # where orange begins
  fraction <- sum_abs_impulses / max_impulses * shock_threshold
  # fraction = 1
  fraction_min <- 0.08
  fraction <- max(fraction_min, min(.9, fraction)) # limit so arrow is inside plot
  par(mai = rep(0,4))
  plot(NA, xlim = c(0, 1.05), ylim = 0:1, xlab = "", ylab = "", axes = F)

  # bliksem <- png::readPNG("bliksem.png", T)
  # eps = .01
  # rasterImage(image = bliksem, xleft = 0.8 - eps, xright = 1.1 - eps, ybottom = 0.45, ytop = 1, interpolate = T)

  polygon(x = c(0,shock_orange,shock_orange,0), y = c(0, 0, shock_orange/2, 0), col = col_apple, border = NA)
  polygon(x = c(shock_orange,shock_threshold,shock_threshold,shock_orange), y = c(0, 0, shock_threshold/2, shock_orange/2), col = col_sun, border = NA)
  polygon(x = c(shock_threshold,1,1,shock_threshold, shock_threshold), y = c(0, 0, .5, shock_threshold/2, 0), col = col_rose, border = NA)
  this_col <- if (shock_threshold < fraction) col_rose else col_apple
  
  
  x  <- fraction
  y  <- fraction / 2
  # dx <- 0.075
  # dy <- .275
  #
  # x. <- c(x, x + dx, x, x + dx * 1.5, x - 0 * dx, x - dx - dx / 2, x, x)
  # y. <- c(y, y + dy, y + dy * 1.3, y + 2 * dy, y + 2 * dy, y + 1.3 * dy, y + 0.8 * dy, y)

  dx <- 0.18
  dy <- .35
  
  x. <- c(x, x + dx, x + .3*dx, x + .5*dx, x - .35 * dx, x - 5 * dx / 10, x + .15 * dx, x)
  y. <- c(y, y + dy, y + dy, y + 1.45 * dy, y + 1.45 * dy, y + 0.7 * dy, y + 0.7 * dy, y)
  
  # this_col <- if (fraction_min == fraction) col_apple else if (fraction < thresh)
  # if (fraction_min == fraction) {
  #   shock_col <- col_sun
  # } else
  
  valid_impulses <- fraction <= shock_threshold
  
  shock_col <- if (valid_impulses) col_sun else col_rose
  polygon(x = x., y = y., col = NA, border = "black", lwd = 2)
  # text(x = x., y = y., 1:length(x.))
    
  # text(fraction, fraction / 2, pos = 3, cex = 2, labels = utf8::as_utf8("↓"), col = this_col)
  
  # print(valid_impulses)
}

# plot_thermometer(100)
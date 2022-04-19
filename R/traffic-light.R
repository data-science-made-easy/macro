# circle
plot_circle <- function (x = 0, y = 0, radius, n_points = 100, force_circle = FALSE, border = NULL, col = NA, lty = 0, density = NULL, angle = 45, lwd = 1) { # code reused from package plotrix
    ymult <- 1

    if (force_circle & 1 < dev.cur()) { # radius matches values on x-axis; y-scaling such that a true circle results
      xyasp <- par("pin")
      xycr  <- diff(par("usr"))[c(1, 3)]
      ymult <- xyasp[1]/xyasp[2] * xycr[2]/xycr[1]
    }
    
    angle_step   <- 2 * pi / n_points
    angle_series <- seq(0, 2 * pi - angle_step, by = angle_step)
    if (length(col) < length(radius)) 
        col <- rep(col, length.out = length(radius))
    for (circle in 1:length(radius)) {
        xv <- cos(angle_series) * radius[circle] + x
        yv <- sin(angle_series) * radius[circle] * ymult + y
        polygon(xv, yv, border = border, col = col[circle], lty = lty, density = density, angle = angle, lwd = lwd)
    }
    
    invisible(list(x = xv, y = yv))
}

plot_traffic_light <- function(violation = FALSE) {
  par(mai = rep(0,4))
  plot(NA, xlim = c(0, 1), ylim = c(0, 2), xlab = "", ylab = "", axes = F)

  rect(0, 0, 1, 2, col = "#EEEEEE", lwd = 2)
  plot_circle(.5,  .5, .35, col = if (!violation) col_apple else "gray", force_circle = T)
  plot_circle(.5, 1.5, .35, col = if (violation)  col_rose  else "gray", force_circle = T)
}
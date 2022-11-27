.compute_plot <- function(x, bar_width = 1L, hpad = 0) {

  # this function calculates a user-adjusted bar width, adds white space between lines
  # and bars (if hpad > 0), and imputes NAs where the lines should not be plotted for both y_prop and y_freq

  x$bar_width <- x$bar_width * bar_width
  horizontal_ws <- (length(x$curve) -  x$bar_width) * hpad
  bar_adjust <-  x$bar_width + horizontal_ws

  x1 <- c(0, x$x_pos[-1] - floor(bar_adjust / 2))
  x2 <- c(x$x_pos[-length(x$x_pos)] + floor(bar_adjust / 2), x$x_pos[length(x$x_pos)])

  # dropping values behind bars
  if (x$type == "trace") {
    for (i in seq_along(x1)) {
      x$traces$y_prop <- ifelse(x$traces$x_axis > x1[i] & x$traces$x_axis < x2[i], NA, x$traces$y_prop)
      x$traces$y_count <- ifelse(x$traces$x_axis > x1[i] & x$traces$x_axis < x2[i], NA, x$traces$y_count)
    }
  }

  if (!is.null(x$flows)) {
    for (i in seq_along(x1)) {
      x$flows$ymin <- ifelse(x$flows$x_axis > x1[i] & x$flows$x_axis < x2[i], NA, x$flows$ymin)
    }
  }

  return(x)

}

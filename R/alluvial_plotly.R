alluvial_plotly <- function(x, highlight_col = "black", highlight_on = "plotly_hover", opacityDim = 0.2, 
                            dynamic = FALSE, tooltip = x$ID, bar_width = 1L, hpad = 0, col = "y_from", 
                            xlabs = NULL, labels = NULL, ggtitle = NULL, y_scale = "prop", bar_clrs = NULL, 
                            flow_clrs = NULL, bar_alpha = 1L, flow_alpha = 0.3, show.legend = TRUE, 
                            auto_theme = FALSE, remove_y_axis = FALSE, ...) {
  
  if (class(x) != "alluvial_model") {
    stop("x must be an alluvial model object. First use `alluvial_prep()`, then pass the result to x")
  }
  
  if (x$type == "flow") {
    stop("Plotting flows with plotly is not currently supported")
  }
  
  # all manipulations to model must happen prior to using highlight_key()
  x <- .compute_plot(x, bar_width, hpad)
  
  x$traces <- highlight_key(x$traces, ~.data[[x$ID]])
  
  p <- .alluvial_base(
    x,
    col = col,
    xlabs = xlabs,
    labels = labels,
    ggtitle = ggtitle,
    y_scale = y_scale,
    bar_clrs = bar_clrs,
    flow_clrs = flow_clrs,
    bar_alpha = bar_alpha,
    flow_alpha = flow_alpha,
    show.legend = show.legend,
    auto_theme = auto_theme,
    remove_y_axis = remove_y_axis
  )
  
  gg <- ggplotly(p, tooltip = tooltip)
  gg <- layout(gg, ...)
  
  highlight(gg, on = highlight_on, color = highlight_col, opacityDim = opacityDim, dynamic = dynamic)
  
}

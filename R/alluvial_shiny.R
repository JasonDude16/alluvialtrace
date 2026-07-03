#' Tune and export an alluvial plot in Shiny
#'
#' @param x An `alluvial_model` object returned by `alluvial_prep_trace()` or `alluvial_prep_flow()`.
#' @param run Logical; if `TRUE`, run the app with `shiny::runApp()`. If `FALSE`, return the Shiny app object.
#' @param ... Additional arguments passed to `shiny::runApp()` when `run = TRUE`.
#'
#' @return A Shiny app object when `run = FALSE`; otherwise the result of `shiny::runApp()`.
#' @export
alluvial_shiny <- function(x, run = TRUE, ...) {
  if (!inherits(x, "alluvial_model")) {
    stop("x must be an alluvial model object. First use `alluvial_prep_trace()` or `alluvial_prep_flow()`.", call. = FALSE)
  }

  app <- shiny::shinyApp(
    ui = .alluvial_shiny_ui(x),
    server = .alluvial_shiny_server(x)
  )

  if (run) {
    return(shiny::runApp(app, ...))
  }

  app
}


.alluvial_shiny_ui <- function(x) {
  col_choices <- .alluvial_plot_col_choices(x)
  step_choices <- stats::setNames(x$steps, x$steps)
  value_choices <- stats::setNames(sort(unique(as.character(x$bars$y_value))), sort(unique(as.character(x$bars$y_value))))
  trace_only <- x$type == "trace"
  has_flows <- !is.null(x$flows)
  output_choices <- if (trace_only) {
    c("Static ggplot" = "static", "Interactive plotly" = "plotly")
  } else {
    c("Static ggplot" = "static")
  }

  shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
        body {
          background: #f4f4f2;
          color: #25231f;
          font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
        }
        .container-fluid { max-width: 1500px; }
        h2 {
          font-weight: 650;
          letter-spacing: 0;
          margin: 18px 0 12px;
        }
        .alluvial-header {
          position: relative;
          background: #2b2925;
          color: #f8f7f2;
          border-radius: 8px;
          padding: 20px 24px 20px 28px;
          margin-top: 18px;
          border: 1px solid #22201d;
          border-left: 8px solid #b8864f;
          box-shadow: 0 8px 22px rgba(43, 41, 37, 0.16);
          overflow: hidden;
        }
        .alluvial-header::after {
          content: '';
          position: absolute;
          right: 0;
          top: 0;
          bottom: 0;
          width: 180px;
          background: linear-gradient(90deg, rgba(184, 134, 79, 0), rgba(184, 134, 79, 0.22));
          pointer-events: none;
        }
        .alluvial-header h1 {
          position: relative;
          z-index: 1;
          font-size: 30px;
          font-weight: 700;
          letter-spacing: 0;
          margin: 0;
        }
        .alluvial-sidebar {
          background: #ffffff;
          border: 1px solid #d8d8d3;
          border-radius: 6px;
          padding: 14px;
          margin-top: 12px;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        }
        .alluvial-main {
          background: #ffffff;
          border: 1px solid #d8d8d3;
          border-radius: 6px;
          padding: 14px;
          margin-top: 12px;
          box-shadow: 0 1px 2px rgba(0, 0, 0, 0.04);
        }
        .alluvial-preview-control {
          max-width: 260px;
          margin: 0 auto 12px;
        }
        .alluvial-preview-control .form-group {
          margin-bottom: 0;
        }
        .alluvial-section-title {
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: #555;
          margin: 4px 0 10px;
        }
        .alluvial-accordion {
          border: 1px solid #e3e3df;
          border-radius: 6px;
          margin-bottom: 10px;
          overflow: hidden;
        }
        .alluvial-accordion[open] {
          border-color: #c8c8c1;
        }
        .alluvial-accordion summary {
          background: #f7f7f4;
          cursor: pointer;
          list-style: none;
          padding: 11px 12px;
          user-select: none;
        }
        .alluvial-accordion summary::-webkit-details-marker {
          display: none;
        }
        .alluvial-accordion summary::after {
          content: '+';
          float: right;
          font-size: 16px;
          line-height: 1;
        }
        .alluvial-accordion[open] summary::after {
          content: '-';
        }
        .alluvial-accordion-title {
          color: #333;
          font-size: 12px;
          font-weight: 700;
          letter-spacing: 0.08em;
          text-transform: uppercase;
        }
        .alluvial-accordion-body {
          padding: 12px;
        }
        .alluvial-accordion-body .form-group,
        .alluvial-accordion-body .shiny-input-container {
          width: 100%;
        }
        .alluvial-accordion-body > :last-child {
          margin-bottom: 0;
        }
        .alluvial-style-checks {
          display: grid;
          grid-template-columns: 1fr 1fr;
          gap: 4px 10px;
          margin-bottom: 12px;
        }
        .alluvial-style-checks .checkbox {
          margin: 0;
        }
        .alluvial-style-checks .checkbox label {
          min-height: 0;
        }
        .alluvial-style-actions {
          margin-top: 12px;
        }
        .alluvial-output-options .btn {
          margin-top: 6px;
          width: 100%;
        }
        .alluvial-xlab-row {
          display: grid;
          grid-template-columns: minmax(0, 1fr) minmax(0, 1.2fr);
          gap: 10px;
          align-items: center;
          margin-bottom: 8px;
        }
        .alluvial-xlab-row label {
          margin-bottom: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .alluvial-xlab-row .form-group {
          margin-bottom: 0;
        }
        .alluvial-status {
          min-height: 20px;
          color: #555;
          font-size: 13px;
        }
        .alluvial-color-actions { margin-bottom: 12px; }
        .btn,
        .btn-default,
        .btn-primary {
          border-radius: 4px;
          border-color: #2b2925;
          background: #2b2925;
          color: #ffffff;
        }
        .btn:hover,
        .btn:focus,
        .btn-default:hover,
        .btn-default:focus,
        .btn-primary:hover,
        .btn-primary:focus {
          border-color: #211f1c;
          background: #211f1c;
          color: #ffffff;
        }
        .form-control:focus {
          border-color: #2b2925;
          box-shadow: 0 0 0 2px rgba(43, 41, 37, 0.14);
        }
        .irs-bar,
        .irs-single,
        .irs-from,
        .irs-to {
          background: #2b2925 !important;
          border-color: #2b2925 !important;
        }
        .irs-handle {
          border-color: #2b2925 !important;
        }
        input[type='checkbox'],
        input[type='radio'] {
          accent-color: #2b2925;
        }
        .alluvial-color-row {
          display: grid;
          grid-template-columns: minmax(0, 1fr) 44px;
          align-items: center;
          gap: 8px;
          margin-bottom: 6px;
        }
        .alluvial-color-row label {
          font-weight: 400;
          margin-bottom: 0;
          overflow: hidden;
          text-overflow: ellipsis;
          white-space: nowrap;
        }
        .alluvial-color-row input[type='color'] {
          width: 42px;
          height: 28px;
          padding: 0;
        }
        "
      ))
    ),
    shiny::tags$script(shiny::HTML(
      "
      document.addEventListener('toggle', function(event) {
        var opened = event.target;
        if (!opened.matches('details.alluvial-accordion') || !opened.open) return;
        var sidebar = opened.closest('.alluvial-sidebar');
        if (!sidebar) return;
        sidebar.querySelectorAll('details.alluvial-accordion[open]').forEach(function(section) {
          if (section !== opened) section.removeAttribute('open');
        });
      }, true);
      "
    )
    ),
    shiny::div(
      class = "alluvial-header",
      shiny::tags$h1("Alluvial Plot Editor")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::div(
          class = "alluvial-sidebar",
          shiny::div(
            class = "alluvial-preview-control",
            shiny::selectInput("output_type", "Preview", choices = output_choices)
          ),
          .alluvial_accordion_section(
            "Style",
            open = FALSE,
            shiny::selectInput("col", "Color column", choices = col_choices, selected = .alluvial_default_col(x, col_choices)),
            shiny::sliderInput("bar_alpha", "Bar alpha", min = 0, max = 1, value = 1, step = 0.05),
            if (has_flows) {
              shiny::sliderInput("flow_alpha", "Flow ribbon alpha", min = 0, max = 1, value = 0.7, step = 0.05)
            },
            if (trace_only) {
              shiny::tagList(
                shiny::sliderInput("trace_alpha", "Trace alpha", min = 0, max = 1, value = 0.5, step = 0.05),
                shiny::numericInput("trace_lwd", "Trace line width", value = 0.5, min = 0.1, step = 0.1)
              )
            },
            shiny::textInput("border_col", "Bar border color", value = "black"),
            shiny::div(
              class = "alluvial-style-checks",
              shiny::checkboxInput("show_legend", "Show legend", value = FALSE),
              shiny::checkboxInput("theme_classic", "theme_classic()", value = TRUE),
              shiny::checkboxInput("remove_y_axis", "Remove y-axis", value = TRUE),
              shiny::checkboxInput("center_title", "Center title", value = TRUE)
            ),
            shiny::div(
              class = "alluvial-style-actions",
              shiny::actionButton("edit_colors", "Edit colors", width = "100%")
            )
          ),
          .alluvial_accordion_section(
            "Labels",
            open = FALSE,
            shiny::textInput("ggtitle", "Title", value = ""),
            shiny::textInput("ggsubtitle", "Subtitle", value = ""),
            shiny::actionButton("edit_xlabs", "Edit x labels", width = "100%"),
            shiny::checkboxInput("show_labels", "Show bar labels", value = FALSE),
            shiny::conditionalPanel(
              "input.show_labels",
              shiny::uiOutput("label_type_ui"),
              shiny::radioButtons("label_what", "Label value", choices = c("Level" = "level", "Count" = "count", "Proportion" = "prop", "Percent" = "perc"), inline = TRUE),
              shiny::checkboxInput("label_all", "Label all bars", value = TRUE),
              shiny::conditionalPanel(
                "!input.label_all",
                shiny::selectizeInput("label_steps", "Label steps", choices = step_choices, selected = x$steps, multiple = TRUE),
                shiny::selectizeInput("label_values", "Label values", choices = value_choices, selected = value_choices, multiple = TRUE)
              )
            )
          ),
          .alluvial_accordion_section(
            "Typography",
            open = FALSE,
            shiny::numericInput("title_size", "Title size", value = 28, min = 6, step = 1),
            shiny::checkboxInput("title_bold", "Bold title", value = TRUE),
            shiny::numericInput("subtitle_size", "Subtitle size", value = 18, min = 6, step = 1),
            shiny::checkboxInput("subtitle_bold", "Bold subtitle", value = FALSE),
            shiny::numericInput("xlab_size", "X label size", value = 18, min = 6, step = 1),
            shiny::checkboxInput("xlab_bold", "Bold x labels", value = TRUE),
            shiny::numericInput("label_size", "Label size", value = 5, min = 1, step = 0.5),
            shiny::checkboxInput("label_bold", "Bold labels", value = FALSE)
          ),
          .alluvial_accordion_section(
            "Geometry",
            open = FALSE,
            shiny::numericInput("bar_width", "Bar width", value = 1, min = 0.05, step = 0.05),
            shiny::numericInput("hpad", "Horizontal padding", value = 0, min = 0, max = 0.95, step = 0.05)
          ),
          shiny::conditionalPanel("input.output_type == 'static'",
            .alluvial_static_export_section()
          ),
          if (trace_only) {
            shiny::conditionalPanel(
              "input.output_type == 'plotly'",
              .alluvial_accordion_section(
                "Plotly",
                open = FALSE,
                shiny::div(
                  class = "alluvial-output-options",
                  shiny::selectizeInput("tooltip", "Tooltip columns", choices = .alluvial_trace_choices(x), selected = x$ID, multiple = TRUE),
                  shiny::textInput("highlight_col", "Highlight color", value = "black"),
                  shiny::selectInput("highlight_on", "Highlight event", choices = c("plotly_hover", "plotly_click", "plotly_selected")),
                  shiny::selectInput("highlight_off", "Clear event", choices = c("plotly_doubleclick", "plotly_deselect", "plotly_relayout")),
                  shiny::sliderInput("opacity_dim", "Dim opacity", min = 0, max = 1, value = 1, step = 0.05),
                  shiny::downloadButton("download_html", "Save HTML")
                )
              )
            )
          }
        )
      ),
      shiny::column(
        width = 9,
        shiny::div(
          class = "alluvial-main",
          shiny::div(class = "alluvial-status", shiny::uiOutput("plot_status")),
          shiny::conditionalPanel("input.output_type == 'static'", shiny::uiOutput("static_plot_ui")),
          shiny::conditionalPanel("input.output_type == 'plotly'", shiny::uiOutput("plotly_plot_ui"))
        )
      )
    )
  )
}


.alluvial_static_export_section <- function() {
  .alluvial_accordion_section(
    "Export",
    open = FALSE,
    shiny::div(
      class = "alluvial-output-options",
      shiny::selectInput("static_format", "Format", choices = c("PNG" = "png", "PDF" = "pdf")),
      shiny::sliderInput("plot_width", "Width", min = 500, max = 1400, value = 1000, step = 25, post = " px", width = "100%"),
      shiny::sliderInput("plot_height", "Height", min = 350, max = 1100, value = 720, step = 25, post = " px", width = "100%"),
      shiny::downloadButton("download_static", "Save static")
    )
  )
}


.alluvial_shiny_server <- function(x) {
  force(x)

  function(input, output, session) {
    color_state <- shiny::reactiveValues(
      bar = .default_palette(.bar_color_values(x)),
      flow = .default_palette(.flow_color_values(x, .alluvial_default_col(x, .alluvial_plot_col_choices(x))))
    )
    label_state <- shiny::reactiveValues(xlabs = x$steps)

    shiny::observeEvent(input$col, {
      flow_values <- .flow_color_values(x, input$col)
      if (!identical(names(color_state$flow), flow_values)) {
        color_state$flow <- .default_palette(flow_values)
      }
    }, ignoreInit = TRUE)

    args <- shiny::reactive(.alluvial_shiny_args(x, input, bar_clrs = color_state$bar, flow_clrs = color_state$flow, xlabs = label_state$xlabs))

    shiny::observeEvent(input$edit_colors, {
      flow_values <- .flow_color_values(x, input$col)
      if (!identical(names(color_state$flow), flow_values)) {
        color_state$flow <- .default_palette(flow_values)
      }
      shiny::showModal(shiny::modalDialog(
        title = "Edit colors",
        shiny::fluidRow(
          shiny::column(6, .alluvial_color_controls("Bar colors", .bar_color_values(x), "draft_bar_clr_", color_state$bar)),
          shiny::column(6, .alluvial_color_controls("Flow/trace colors", flow_values, "draft_flow_clr_", color_state$flow))
        ),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("update_colors", "Update")
        ),
        size = "l"
      ))
    })

    shiny::observeEvent(input$update_colors, {
      color_state$bar <- .palette_from_color_inputs(input, .bar_color_values(x), "draft_bar_clr_", color_state$bar)
      color_state$flow <- .palette_from_color_inputs(input, .flow_color_values(x, input$col), "draft_flow_clr_", color_state$flow)
      shiny::removeModal()
    })

    shiny::observeEvent(input$edit_xlabs, {
      shiny::showModal(shiny::modalDialog(
        title = "Edit x labels",
        .alluvial_xlab_controls(x$steps, label_state$xlabs),
        easyClose = TRUE,
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("update_xlabs", "Update")
        ),
        size = "m"
      ))
    })

    shiny::observeEvent(input$update_xlabs, {
      label_state$xlabs <- .xlabs_from_inputs(input, x$steps, label_state$xlabs)
      shiny::removeModal()
    })

    static_plot <- shiny::reactive({
      do.call(alluvial_plot, c(list(x = x), args()$common))
    })

    plotly_plot <- shiny::reactive({
      shiny::validate(shiny::need(x$type == "trace", "Interactive plotly output is currently available for trace-level models only."))
      do.call(alluvial_plotly, c(list(x = x), args()$common, args()$plotly))
    })

    output$label_type_ui <- shiny::renderUI({
      choices <- if (identical(input$output_type, "plotly")) {
        c("Text" = "text")
      } else {
        c("Text" = "text", "Boxed" = "label")
      }
      current <- input$label_type %||% "text"
      selected <- if (current %in% choices) current else "text"
      shiny::radioButtons("label_type", "Label type", choices = choices, selected = selected, inline = TRUE)
    })

    output$plot_status <- shiny::renderUI({
      if (input$output_type == "plotly" && x$type != "trace") {
        return(shiny::tags$p("Interactive plotly output is currently available for trace-level models only."))
      }
      NULL
    })

    output$static_plot_ui <- shiny::renderUI({
      shiny::plotOutput(
        "static_plot",
        width = paste0(input$plot_width %||% 1000, "px"),
        height = paste0(input$plot_height %||% 720, "px")
      )
    })

    output$plotly_plot_ui <- shiny::renderUI({
      plotly::plotlyOutput(
        "plotly_plot",
        width = "100%",
        height = "720px"
      )
    })

    output$static_plot <- shiny::renderPlot({
      static_plot()
    })

    output$plotly_plot <- plotly::renderPlotly({
      plotly_plot()
    })

    output$download_static <- shiny::downloadHandler(
      filename = function() paste0("alluvial-plot.", input$static_format %||% "png"),
      content = function(file) {
        alluvial_save(
          static_plot(),
          file,
          width = input$plot_width %||% 1000,
          height = input$plot_height %||% 720,
          units = "px",
          dpi = 300
        )
      }
    )

    output$download_html <- shiny::downloadHandler(
      filename = function() "alluvial-plot.html",
      content = function(file) {
        shiny::validate(shiny::need(x$type == "trace", "HTML widget export is currently available for trace-level models only."))
        alluvial_save(plotly_plot(), file)
      }
    )
  }
}


.alluvial_shiny_args <- function(x, input, bar_clrs = NULL, flow_clrs = NULL, xlabs = NULL) {
  labels <- NULL
  if (isTRUE(input$show_labels)) {
    label_where <- "all"
    if (!isTRUE(input$label_all)) {
      label_where <- list(
        steps = input$label_steps %||% x$steps,
        values = input$label_values %||% unique(as.character(x$bars$y_value))
      )
    }
    labels <- list(
      type = if (identical(input$output_type, "plotly")) "text" else input$label_type,
      what = input$label_what,
      where = label_where
    )
  }

  common <- list(
    bar_width = input$bar_width,
    hpad = input$hpad,
    col = input$col,
    xlabs = xlabs %||% .parse_xlabs(input$xlabs, x$steps),
    labels = labels,
    ggtitle = .empty_as_null(input$ggtitle),
    ggsubtitle = .empty_as_null(input$ggsubtitle),
    y_scale = "prop",
    bar_clrs = bar_clrs %||% .palette_from_color_inputs(input, .bar_color_values(x), "bar_clr_"),
    flow_clrs = flow_clrs %||% .palette_from_color_inputs(input, .flow_color_values(x, input$col), "flow_clr_"),
    bar_alpha = input$bar_alpha,
    trace_alpha = input$trace_alpha,
    flow_alpha = input$flow_alpha %||% 0.7,
    border_col = input$border_col,
    show.legend = isTRUE(input$show_legend),
    theme_classic = isTRUE(input$theme_classic),
    remove_y_axis = isTRUE(input$remove_y_axis),
    trace_lwd = input$trace_lwd,
    title_size = input$title_size %||% 28,
    title_bold = isTRUE(input$title_bold),
    subtitle_size = input$subtitle_size %||% 18,
    subtitle_bold = isTRUE(input$subtitle_bold),
    center_title = isTRUE(input$center_title),
    xlab_size = input$xlab_size %||% 18,
    xlab_bold = isTRUE(input$xlab_bold),
    label_size = input$label_size %||% 5,
    label_bold = isTRUE(input$label_bold)
  )

  plotly <- list(
    highlight_col = input$highlight_col %||% "black",
    highlight_on = input$highlight_on %||% "plotly_hover",
    highlight_off = input$highlight_off %||% "plotly_doubleclick",
    opacityDim = input$opacity_dim %||% 0.2,
    dynamic = FALSE,
    tooltip = input$tooltip %||% x$ID
  )

  list(common = common, plotly = plotly)
}


.alluvial_plot_col_choices <- function(x) {
  unique(c(
    if (!is.null(x$traces)) names(x$traces),
    if (!is.null(x$flows)) names(x$flows),
    names(x$bars)
  ))
}


.alluvial_trace_choices <- function(x) {
  if (is.null(x$traces)) {
    return(character())
  }
  names(x$traces)
}


.alluvial_accordion_section <- function(title, ..., open = FALSE) {
  shiny::tags$details(
    class = "alluvial-accordion",
    open = if (open) "open" else NULL,
    shiny::tags$summary(
      shiny::span(class = "alluvial-accordion-title", title)
    ),
    shiny::div(class = "alluvial-accordion-body", ...)
  )
}


.alluvial_default_col <- function(x, choices) {
  defaults <- c("y_from", "y_value")
  default <- defaults[defaults %in% choices][1]
  if (is.na(default)) {
    return(choices[1])
  }
  default
}


.alluvial_color_controls <- function(title, values, prefix, palette = NULL) {
  if (length(values) == 0L) {
    return(NULL)
  }

  defaults <- palette %||% .default_palette(values)
  ids <- .color_input_ids(values, prefix)
  rows <- lapply(seq_along(values), function(i) {
    value <- values[[i]]
    id <- ids[[i]]
    selected <- defaults[[value]] %||% .default_palette(value)[[value]]
    shiny::div(
      class = "alluvial-color-row",
      shiny::tags$label(`for` = id, value),
      shiny::tags$input(
        id = id,
        type = "color",
        value = selected,
        oninput = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", id),
        onchange = sprintf("Shiny.setInputValue('%s', this.value, {priority: 'event'});", id)
      )
    )
  })

  shiny::tagList(
    shiny::tags$strong(title),
    shiny::div(rows)
  )
}


.alluvial_xlab_controls <- function(steps, labels) {
  rows <- lapply(seq_along(steps), function(i) {
    shiny::div(
      class = "alluvial-xlab-row",
      shiny::tags$label(`for` = paste0("draft_xlab_", i), steps[[i]]),
      shiny::textInput(paste0("draft_xlab_", i), label = NULL, value = labels[[i]])
    )
  })

  shiny::tagList(
    shiny::div(
      class = "alluvial-xlab-row",
      shiny::tags$strong("From"),
      shiny::tags$strong("To")
    ),
    shiny::div(rows)
  )
}


.xlabs_from_inputs <- function(input, steps, fallback = steps) {
  values <- vapply(seq_along(steps), function(i) {
    value <- input[[paste0("draft_xlab_", i)]] %||% fallback[[i]]
    if (identical(trimws(value), "")) steps[[i]] else value
  }, character(1))

  unname(values)
}


.bar_color_values <- function(x) {
  sort(unique(as.character(x$bars$y_value)))
}


.flow_color_values <- function(x, col) {
  values <- .values_from_col(x$traces, col)
  if (length(values) == 0L) {
    values <- .values_from_col(x$flows, col)
  }
  if (length(values) == 0L) {
    values <- .values_from_col(x$bars, col)
  }
  values
}


.values_from_col <- function(data, col) {
  if (is.null(data) || is.null(col) || !col %in% names(data)) {
    return(character())
  }
  sort(unique(as.character(data[[col]])))
}


.palette_from_color_inputs <- function(input, values, prefix, fallback = NULL) {
  if (length(values) == 0L) {
    return(NULL)
  }

  defaults <- fallback %||% .default_palette(values)
  ids <- .color_input_ids(values, prefix)
  colors <- vapply(seq_along(values), function(i) {
    input[[ids[[i]]]] %||% defaults[[values[[i]]]]
  }, character(1))
  stats::setNames(colors, values)
}


.color_input_ids <- function(values, prefix) {
  stats::setNames(paste0(prefix, seq_along(values)), values)
}


.default_palette <- function(values) {
  values <- as.character(values)
  colors <- grDevices::hcl.colors(length(values), palette = "Dark 3")
  stats::setNames(colors, values)
}


.parse_xlabs <- function(x, steps) {
  parsed <- trimws(strsplit(x %||% "", ",", fixed = TRUE)[[1]])
  parsed <- parsed[nzchar(parsed)]
  if (length(parsed) == length(steps)) {
    return(parsed)
  }
  NULL
}


.parse_palette <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NULL)
  }

  pieces <- unlist(strsplit(x, "[,\n;]+"))
  pieces <- trimws(pieces)
  pieces <- pieces[nzchar(pieces)]
  has_names <- grepl("=", pieces, fixed = TRUE)

  if (all(has_names)) {
    names <- trimws(sub("=.*$", "", pieces))
    values <- trimws(sub("^[^=]*=", "", pieces))
    stats::setNames(values, names)
  } else {
    pieces
  }
}


.empty_as_null <- function(x) {
  if (is.null(x) || !nzchar(trimws(x))) {
    return(NULL)
  }
  x
}


`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    return(y)
  }
  x
}

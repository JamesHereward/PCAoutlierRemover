#' Interactively select outlier samples from a 2D PCA
#'
#' Opens a Shiny gadget for exploring 2D PCA scatter plots and marking
#' samples for removal. Drag a rectangle on the plot to brush-select
#' multiple samples at once, or click points one at a time. The
#' selected sample IDs are returned as a character vector when "Done"
#' is pressed.
#'
#' Features:
#' * Drag to brush-select (debounced — fires when you stop dragging)
#' * Click to toggle individual points
#' * Double-click inside a brushed area to zoom; double-click empty
#'   space to reset zoom
#' * Switch axes between any PCs without restarting
#' * Adjustable point size
#' * Live R filter code generated as you select
#' * Export selection as `.txt`
#'
#' @param x Either a list returned by `compute_pca()` (with `$scores`
#'   and `$var_pct` components), or a data frame of scores with `id`,
#'   `pop`, `PC1`, `PC2`... columns.
#' @param var_pct Numeric vector of percent variance per PC. Required
#'   if `x` is a data frame. Ignored if `x` is a `compute_pca()` result.
#' @param pc_x,pc_y Initial axes to display. Default "PC1" and "PC2".
#' @param palette Optional named hex colour vector keyed by population.
#'   If NULL, uses `default_palette()`.
#'
#' @return Character vector of selected sample IDs (invisibly).
#'
#' @examples
#' \dontrun{
#' # From a genind object
#' res <- compute_pca(Genind.90, popmap)
#' remove_ids <- remove_outliers(res)
#'
#' # Apply selections downstream:
#' popmap    <- popmap[!popmap$id %in% remove_ids, ]
#' Genind.90 <- Genind.90[!indNames(Genind.90) %in% remove_ids, ]
#'
#' # Or from a precomputed scores data frame:
#' remove_ids <- remove_outliers(my_scores, var_pct = my_var_pct)
#' }
#'
#' @export
remove_outliers <- function(x,
                            var_pct = NULL,
                            pc_x    = "PC1",
                            pc_y    = "PC2",
                            palette = NULL) {

  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Please install shiny: install.packages('shiny')")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Please install ggplot2: install.packages('ggplot2')")

  # Accept either a compute_pca() result or a raw scores data frame
  if (is.list(x) && !is.data.frame(x) && all(c("scores", "var_pct") %in% names(x))) {
    scores  <- x$scores
    var_pct <- x$var_pct
  } else {
    scores <- x
    if (is.null(var_pct))
      stop("If `x` is a data frame, you must supply `var_pct`.")
  }

  validate_scores(scores)
  populations <- unique(scores$pop)
  if (is.null(palette)) palette <- default_palette(populations)

  pc_cols <- grep("^PC[0-9]+$", colnames(scores), value = TRUE)

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(gadget_css())),
    shiny::titlePanel("PCA outlier removal \u2014 drag to brush-select, click to toggle"),

    shiny::div(class = "axis-controls",
      shiny::tags$label("X axis:"),
      shiny::selectInput("pcx", NULL, choices = pc_cols,
                         selected = pc_x, width = "80px"),
      shiny::tags$label("Y axis:"),
      shiny::selectInput("pcy", NULL, choices = pc_cols,
                         selected = pc_y, width = "80px"),
      shiny::tags$label("Point size:", style = "margin-left:16px;"),
      shiny::div(style = "width:160px;",
        shiny::sliderInput("psize", NULL, min = 0.5, max = 6,
                           value = 2.5, step = 0.5, ticks = FALSE))
    ),

    shiny::plotOutput("plot",
      height   = "600px",
      brush    = shiny::brushOpts(id = "brush", resetOnNew = TRUE,
                                  fill = "#E24B4A", stroke = "#E24B4A",
                                  delay = 300, delayType = "debounce"),
      click    = "click",
      dblclick = "dblclick"
    ),

    shiny::div(class = "info-row",
      shiny::span(shiny::textOutput("count", inline = TRUE)),
      shiny::span("\u2022 Drag to select \u2022 Click to toggle one \u2022 Double-click to zoom")
    ),

    shiny::div(class = "controls",
      shiny::actionButton("clear",    "Clear selection"),
      shiny::downloadButton("export", "Export .txt"),
      shiny::actionButton("done",     "Done \u2014 return to R",
                          class = "btn-success")
    ),

    shiny::h4("Selected samples to remove:"),
    shiny::div(class = "selected-box", shiny::textOutput("rcode"))
  )

  server <- function(input, output, session) {

    selected <- shiny::reactiveVal(character(0))
    ranges   <- shiny::reactiveValues(x = NULL, y = NULL)

    output$plot <- shiny::renderPlot({
      df <- scores
      df$selected <- df$id %in% selected()
      ps <- input$psize

      ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$pcx]], y = .data[[input$pcy]])) +
        ggplot2::geom_point(ggplot2::aes(colour = .data$pop,
                                         alpha  = .data$selected),
                            size = ps) +
        ggplot2::geom_point(data = df[df$selected, ],
                            ggplot2::aes(x = .data[[input$pcx]], y = .data[[input$pcy]]),
                            colour = "#E24B4A",
                            size   = ps + 1.5,
                            shape  = 21,
                            stroke = max(1, ps * 0.5),
                            fill   = NA) +
        ggplot2::scale_colour_manual(values = palette) +
        ggplot2::scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.7),
                                    guide  = "none") +
        ggplot2::coord_cartesian(xlim = ranges$x, ylim = ranges$y) +
        ggplot2::labs(
          x = sprintf("%s (%.2f%%)", input$pcx,
                      var_pct[as.integer(sub("PC", "", input$pcx))]),
          y = sprintf("%s (%.2f%%)", input$pcy,
                      var_pct[as.integer(sub("PC", "", input$pcy))]),
          colour = "Population"
        ) +
        ggplot2::theme_bw(base_size = 13) +
        ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
    })

    shiny::observeEvent(input$brush, {
      brushed <- shiny::brushedPoints(scores, input$brush,
                                      xvar = input$pcx, yvar = input$pcy)
      if (nrow(brushed) > 0) selected(union(selected(), brushed$id))
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$click, {
      near <- shiny::nearPoints(scores, input$click,
                                xvar = input$pcx, yvar = input$pcy,
                                threshold = 10, maxpoints = 1)
      if (nrow(near) > 0) {
        id  <- near$id
        cur <- selected()
        selected(if (id %in% cur) setdiff(cur, id) else c(cur, id))
      }
    })

    shiny::observeEvent(input$dblclick, {
      br <- input$brush
      if (!is.null(br)) {
        ranges$x <- c(br$xmin, br$xmax)
        ranges$y <- c(br$ymin, br$ymax)
        session$resetBrush("brush")
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })

    shiny::observeEvent(input$clear, { selected(character(0)) })

    output$count <- shiny::renderText({
      n <- length(selected())
      if (n == 0) "No samples selected" else paste0(n, " selected")
    })

    output$rcode <- shiny::renderText({
      sel <- selected()
      if (length(sel) == 0) return("(click or brush points to select)")
      paste0(
        "remove_ids <- c(",
        paste0('"', sel, '"', collapse = ", "), ")\n\n",
        "popmap     <- popmap[!popmap$id %in% remove_ids, ]\n",
        "Genind.90  <- Genind.90[!indNames(Genind.90) %in% remove_ids, ]\n",
        "keep_cols  <- c(TRUE, !colnames(vcfR@gt)[-1] %in% remove_ids)\n",
        "vcfR       <- vcfR[, keep_cols]"
      )
    })

    output$export <- shiny::downloadHandler(
      filename = function() "remove_list.txt",
      content  = function(file) writeLines(selected(), file)
    )

    shiny::observeEvent(input$done, { shiny::stopApp(selected()) })
  }

  result <- shiny::runGadget(ui, server,
                             viewer = shiny::paneViewer(minHeight = 850))
  invisible(result)
}

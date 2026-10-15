#' Fast PNG save
#'
#' Uses ragg::agg_png for faster rendering when available.
#' For dot plots with scattermore, this is significantly faster than
#' the default cairo PNG device.
#'
#' @param filename Output path
#' @param plot ggplot object
#' @param width Width in inches (default 14)
#' @param height Height in inches (default 14)
#' @param dpi Resolution (default 150)
#' @param bg Background colour (default "white")
#' @param ... Additional arguments
#' @return Invisibly, the filename
#' @keywords internal
.fast_save <- function(filename, plot, width = 14, height = 14, dpi = 150,
                       bg = "white", ...) {
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename, width = width, height = height,
                   units = "in", res = dpi, background = bg)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot)
  } else {
    grDevices::png(filename, width = width, height = height,
                    units = "in", res = dpi, bg = bg, type = "cairo")
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot)
  }
  invisible(filename)
}

#' Fast PNG save
#'
#' Opens a ragg device, prints the plot, closes. Faster than ggsave
#' because it avoids ggsave's overhead.
#'
#' @param filename Output path
#' @param plot ggplot or patchwork object
#' @param width Width in inches (default 14)
#' @param height Height in inches (default 14)
#' @param dpi Resolution (default 150)
#' @param bg Background colour (default "white")
#' @return Invisibly, the filename
#' @keywords internal
.fast_save <- function(filename, plot, width = 14, height = 14, dpi = 150,
                       bg = "white") {

  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(filename, width = width, height = height,
                   units = "in", res = dpi, background = bg)
  } else {
    grDevices::png(filename, width = width, height = height,
                    units = "in", res = dpi, bg = bg, type = "cairo")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  print(plot)
  invisible(filename)
}

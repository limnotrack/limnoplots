#' Aggregate time series to pixel-width resolution
#'
#' Bins a time series to a target number of bins (matching render width)
#' and computes min, median, max per bin. Stacks into a long format
#' suitable for plotting as a ribbon + line.
#'
#' Uses data.table internally for speed on large datasets.
#'
#' @param datetime POSIXct vector
#' @param value Numeric vector
#' @param n_bins Target number of bins (default 2000, matching typical plot width in px)
#' @param xlim Optional POSIXct range to constrain binning
#' @return Data frame with columns: dt (bin midpoint), min, median, max
#' @keywords internal
.aggregate_to_pixels <- function(datetime, value, n_bins = 2000, xlim = NULL) {

  mask <- !is.na(datetime) & !is.na(value)
  if (sum(mask) < 10) {
    return(data.frame(dt = datetime[mask], min = value[mask],
                       median = value[mask], max = value[mask]))
  }

  dt_num <- as.numeric(datetime[mask])
  val <- value[mask]

  if (!is.null(xlim)) {
    xl <- as.numeric(xlim)
    keep <- dt_num >= xl[1] & dt_num <= xl[2]
    dt_num <- dt_num[keep]; val <- val[keep]
    if (length(dt_num) < 10) {
      return(data.frame(dt = as.POSIXct(dt_num, origin = "1970-01-01"),
                         min = val, median = val, max = val))
    }
  }

  rng <- range(dt_num)
  bin_width <- (rng[2] - rng[1]) / n_bins

  if (requireNamespace("data.table", quietly = TRUE)) {
    # data.table path (fast)
    DT <- data.table::data.table(dt = dt_num, val = val)
    DT[, bin := floor((dt - rng[1]) / bin_width)]
    agg <- DT[, list(
      dt     = as.POSIXct(mean(dt), origin = "1970-01-01"),
      min    = min(val),
      median = stats::median(val),
      max    = max(val)
    ), by = bin]
    agg[, bin := NULL]
    as.data.frame(agg)
  } else {
    # Base R fallback
    bins <- floor((dt_num - rng[1]) / bin_width)
    data.frame(
      dt     = as.POSIXct(tapply(dt_num, bins, mean), origin = "1970-01-01"),
      min    = tapply(val, bins, min),
      median = tapply(val, bins, stats::median),
      max    = tapply(val, bins, max)
    )
  }
}


#' Plot aggregated time series as ribbon + line
#'
#' Renders min-max as a ribbon with median as a line on top.
#' Ensures extreme values are visible even at high compression.
#'
#' @param agg Data frame from [.aggregate_to_pixels()]
#' @param colour Line/ribbon colour
#' @param ribbon_alpha Ribbon transparency (default 0.3)
#' @return List of ggplot layers
#' @keywords internal
.agg_layers <- function(agg, colour = "#377EB8", ribbon_alpha = 0.3) {
  list(
    ggplot2::geom_ribbon(data = agg,
                          ggplot2::aes(x = dt, ymin = min, ymax = max),
                          fill = colour, alpha = ribbon_alpha, colour = NA),
    ggplot2::geom_line(data = agg,
                        ggplot2::aes(x = dt, y = median),
                        colour = colour, linewidth = 0.3)
  )
}

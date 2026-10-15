#' Device timeline plot
#'
#' Multi-panel faceted plot showing time series coloured by device serial
#' number, with raw data in grey behind. Designed for tracking sensor
#' swaps and device-specific behaviour (e.g. pCO2 sensors).
#'
#' @param data Data frame with datetime, value, site, and device columns
#' @param data_raw Optional raw data frame (plotted in grey behind)
#' @param col_datetime,col_value,col_site,col_device Column names
#' @param device_colours Named vector of colours per device. If NULL, auto-generated.
#' @param title Plot title
#' @param xlim,ylim Axis limits
#' @param gap_days Gaps longer than this are broken (default 1)
#' @return A ggplot object
#' @export
lp_device_timeline <- function(data, data_raw = NULL,
                                col_datetime = "DateTime",
                                col_value = "pCO2",
                                col_site = "site",
                                col_device = "device",
                                device_colours = NULL,
                                title = NULL, xlim = NULL, ylim = NULL,
                                gap_days = 1) {

  # Ensure device is factor with all levels
  all_devices <- sort(unique(data[[col_device]]))
  data[[col_device]] <- factor(data[[col_device]], levels = all_devices)

  # Default colours
  if (is.null(device_colours)) {
    n <- length(all_devices)
    if (n <= 6) {
      cols <- c("#E41A1C", "#FF7F00", "#4DAF4A", "#377EB8", "#984EA3", "#A65628")[1:n]
    } else {
      cols <- scales::hue_pal()(n)
    }
    device_colours <- stats::setNames(cols, all_devices)
  }

  # Add dummy rows so legend shows all devices in every facet
  sites <- unique(data[[col_site]])
  site_order <- if (is.factor(data[[col_site]])) levels(data[[col_site]]) else sites

  dummy <- do.call(rbind, lapply(site_order, function(s) {
    present <- unique(as.character(data[[col_device]][data[[col_site]] == s]))
    missing <- setdiff(all_devices, present)
    if (length(missing) == 0) return(NULL)
    d <- data.frame(x = as.POSIXct(NA), y = NA_real_, s = s,
                     dev = factor(missing, levels = all_devices))
    names(d) <- c(col_datetime, col_value, col_site, col_device)
    d
  }))
  if (!is.null(dummy)) data <- rbind(data, dummy)

  data[[col_site]] <- factor(data[[col_site]], levels = site_order)

  # Gap breaking
  .break_line_gaps <- function(df, dt_col, val_col, gap_secs) {
    df <- df[order(df[[dt_col]]), ]
    if (nrow(df) < 2) return(df)
    dt <- as.numeric(df[[dt_col]])
    gaps <- which(diff(dt) > gap_secs)
    if (length(gaps) == 0) return(df)
    gap_rows <- df[gaps, ]
    gap_rows[[dt_col]] <- gap_rows[[dt_col]] + 1
    gap_rows[[val_col]] <- NA_real_
    rbind(df, gap_rows)[order(c(seq_len(nrow(df)), gaps + 0.5)), ]
  }

  gap_secs <- gap_days * 86400
  data <- do.call(rbind, lapply(split(data, interaction(data[[col_site]], data[[col_device]], drop = TRUE)),
                                 function(d) .break_line_gaps(d, col_datetime, col_value, gap_secs)))

  # Build plot
  p <- ggplot2::ggplot()

  if (!is.null(data_raw)) {
    data_raw[[col_site]] <- factor(data_raw[[col_site]], levels = site_order)
    data_raw <- do.call(rbind, lapply(split(data_raw, data_raw[[col_site]]),
                                       function(d) .break_line_gaps(d, col_datetime, col_value, gap_secs)))
    p <- p + ggplot2::geom_line(
      data = data_raw,
      ggplot2::aes(x = .data[[col_datetime]], y = .data[[col_value]]),
      colour = "grey85", linewidth = 0.3, na.rm = TRUE
    )
  }

  p <- p +
    ggplot2::geom_line(
      data = data,
      ggplot2::aes(x = .data[[col_datetime]], y = .data[[col_value]],
                    colour = .data[[col_device]]),
      linewidth = 0.4, na.rm = TRUE
    ) +
    ggplot2::scale_colour_manual(values = device_colours, drop = FALSE,
                                  name = "Device S/N") +
    ggplot2::facet_wrap(stats::as.formula(paste("~", col_site)),
                         ncol = 1, scales = "fixed") +
    ggplot2::guides(colour = ggplot2::guide_legend(
      nrow = 1, override.aes = list(linewidth = 1.5)
    )) +
    ggplot2::labs(x = NULL, y = .var_label(col_value)) +
    .lp_theme() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(2, "cm"),
      legend.text = ggplot2::element_text(size = 10),
      strip.text = ggplot2::element_text(hjust = 0, size = 11)
    )

  if (!is.null(xlim)) p <- p + ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  else if (!is.null(ylim)) p <- p + ggplot2::coord_cartesian(ylim = ylim)

  if (!is.null(title)) p <- p + ggplot2::labs(title = title)
  p
}

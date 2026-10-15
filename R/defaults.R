#' Default variable definitions for limnoplots
#'
#' Returns a data frame of standard variable names, labels, and units
#' used across limnological profiler/buoy datasets. Used internally
#' by plotting functions but can be customised.
#'
#' @param type One of "profile", "met", or "all" (default "all")
#' @return Data frame with columns: var, label, unit, type
#' @export
lp_defaults <- function(type = "all") {

  pro_vars <- data.frame(
    var   = c("TmpWtr", "DOconc", "DOpsat", "pH", "SpCond", "Cond",
              "TurbRT", "FlChlr", "FlPhyc", "fDOM", "Trypt", "pCO2",
              "pCO2_MED", "ORP"),
    label = c("Temperature", "DO", "DO", "pH", "SpCond", "Conductivity",
              "Turbidity", "Chlorophyll", "Phycocyanin", "fDOM", "Tryptophan", "pCO2",
              "pCO2", "ORP"),
    unit  = c("\u00b0C", "mg/L", "%sat", "", "\u00b5S/cm", "\u00b5S/cm",
              "FNU", "RFU", "RFU", "QSU", "ppb", "ppm",
              "ppm", "mV"),
    type  = "profile",
    stringsAsFactors = FALSE
  )

  met_vars <- data.frame(
    var   = c("TmpAir", "HumRel", "PrBaro", "WndSpd", "WndDir",
              "WndGst", "PpRain", "RadSWD"),
    label = c("Air Temp", "Humidity", "Pressure", "Wind Speed", "Wind Dir",
              "Wind Gust", "Rainfall", "Solar Rad"),
    unit  = c("\u00b0C", "%", "hPa", "m/s", "\u00b0",
              "m/s", "mm/hr", "W/m\u00b2"),
    type  = "met",
    stringsAsFactors = FALSE
  )

  all_vars <- rbind(pro_vars, met_vars)

  switch(type,
    profile = pro_vars,
    met = met_vars,
    all = all_vars,
    stop("type must be 'profile', 'met', or 'all'")
  )
}


#' Get label with unit for a variable
#' @keywords internal
.var_label <- function(var, defaults = NULL) {
  if (is.null(defaults)) defaults <- lp_defaults()
  row <- defaults[defaults$var == var, ]
  if (nrow(row) == 0) return(var)
  if (nchar(row$unit) > 0) {
    paste0(row$label, " (", row$unit, ")")
  } else {
    row$label
  }
}

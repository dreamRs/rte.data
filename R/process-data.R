

#' @importFrom data.table as.data.table rbindlist :=
process_data <- function(json) {
  data <- json[[1]]
  if (!is.null(data[["unit"]])) {
    unit <- data[["unit"]]
    data[["unit"]] <- NULL
    data <- cbind(data, unit)
  }
  if (!is.null(data[["production_unit"]])) {
    production_unit <- data[["production_unit"]]
    data[["production_unit"]] <- NULL
    data <- cbind(data, production_unit)
  }
  data <- as.data.table(data)
  if (!is.null(data[["updated_date"]])) {
    data <- data[, updated_date := parse_datetime(updated_date)]
  }
  if (!is.null(data$values)) {
    values <- rbindlist(l = data$values, idcol = TRUE, fill = TRUE)
    data[, .id := seq_len(.N)]
    data[, values := NULL]
    data[, start_date := NULL]
    data[, end_date := NULL]
    data <- merge(x = data, y = values, by = ".id", all.x = TRUE, all.y = TRUE)
    var_dates <- grep(pattern = "_date", x = names(data), value = TRUE)
    data <- data[, (var_dates) := lapply(.SD, parse_datetime), .SDcols = var_dates]
    data <- data[, .id := NULL]
  }
  data
}



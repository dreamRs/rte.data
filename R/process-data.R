

#' @importFrom data.table as.data.table rbindlist :=
process_data <- function(json) {
  data <- json[[1]]
  data <- as.data.table(data)
  values <- rbindlist(l = data$values, idcol = TRUE, fill = TRUE)
  data[, .id := seq_len(.N)]
  data[, values := NULL]
  data[, start_date := NULL]
  data[, end_date := NULL]
  data <- merge(x = data, y = values, by = ".id", all.x = TRUE, all.y = TRUE)
  data <- data[, start_date := parse_datetime(start_date)]
  data <- data[, end_date := parse_datetime(end_date)]
  data <- data[, updated_date := parse_datetime(updated_date)]
  data <- data[, .id := NULL]
  data
}



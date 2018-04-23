
#' Retrieve generation per unit
#'
#' @param start_date Optional, starting date to filter results.
#' @param end_date Optional, ending date to filter results.
#'
#' @return a \code{data.table}
#' @export
#'
#' @examples
#' \dontrun{
#'
#' library(rte.data)
#' library(data.table)
#'
#' dat <- retrieve_active_units()
#' dat
#'
#' dat[name %chin% "TRICASTIN 1"]
#'
#' }
#' @importFrom data.table setattr
retrieve_active_units <- function(start_date = NULL, end_date = NULL) {

  # actual generation per units
  prod_unit <- get_actual_generation(
    resource = "actual_generations_per_unit",
    start_date = start_date, end_date = end_date
  )

  # installed capacities per units
  gen_inst_unit <- get_open_api(
    api = "generation_installed_capacities",
    resource = "capacities_per_production_unit"
  )

  # corresp code eic
  prodgen <- merge(
    x = prod_unit[, list(
      prod_max = max(value, na.rm = TRUE),
      prod_min = min(value, na.rm = TRUE),
      start_date = min(start_date, na.rm = TRUE),
      end_date = max(end_date, na.rm = TRUE)
    ), by = list(eic_code, name)],
    y = code_eic[, list(eic_code = code_eic, eic_parent)],
    by = "eic_code", all.x = TRUE, all.y = FALSE
  )
  prodgen <- merge(
    x = prodgen,
    y = gen_inst_unit[, list(installed_capacity, eic_parent = code_eic, type)],
    by = "eic_parent", all.x = TRUE, all.y = FALSE
  )
  prodgen <- prodgen[!is.na(type)]
  setattr(prodgen, "class", c(class(prodgen), "rte.data.table"))
  setattr(prodgen, "api.name", "retrieve_active_units")
  return(prodgen)
}





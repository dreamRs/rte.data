
# cat("\u00c9coutez !",
#     "\nPuisqu\u2019on allume les \u00e9toiles,",
#     "\nc\u2019est qu\u2019elles sont \u00e0",
#     "\nquelqu\u2019un n\u00e9cessaires ?",
#     "\nC\u2019est que quelqu\u2019un d\u00e9sire",
#     "\nqu\u2019elles soient ?",
#     "\n\nVladimir Ma\u00efakovski")


#' @title Autplot method
#'
#' @description Quick \code{ggplot} to explore data
#'
#' @param object a \code{rte.data.table} obtained from the API.
#' @param interactive Logical, produce an interactive visualisation.
#' @param ... other arguments passed to specific methods
#'
#' @return A ggplot object.
#' @export
#'
#'
#' @examples
#' # todo
autoplot.rte.data.table <- function(object, interactive = FALSE, ...) {
  api_name <- attr(object, "api.name")
  api_resource <- attr(object, "api.resource")

  args <- list(...)

  if (is.null(api_name))
    stop("No autoplot defined for this API !", call. = FALSE)

  if (api_name == "consumption") {

    if (interactive) {
      viz_consumption(object)
    } else {
      plot_consumption(object)
    }

  } else if (api_name == "physical_flow") {


    if (!is.null(args$by_country) && args$by_country) {
      if (interactive) {
        viz_exchange(object)
      } else {
        plot_exchange(object)
      }
    } else {
      if (interactive) {
        viz_balance(object)
      } else {
        plot_balance(object)
      }
    }

  } else if (api_name == "actual_generation") {

    if (api_resource == "actual_generations_per_production_type") {

      if (interactive) {
        viz_actual_generation(object, by_day = args$by_day)
      } else {
        plot_actual_generation(object, by_day = args$by_day)
      }


    }

  } else if (api_name == "retrieve_active_units") {

    if (!is.null(args$map) && args$map) {

      if (interactive) {
        imap_p_active_units(object)
      } else {
        map_p_active_units(object)
      }

    } else {

      if (interactive) {
        viz_p_active_units(object)
      } else {
        plot_p_active_units(object)
      }

    }


  } else if (api_name == "generation_installed_capacities") {

    if (api_resource == "capacities_per_production_unit") {

      if (interactive) {
        imap_installed_capacities(object)
      } else {
        map_installed_capacities(object)
      }

    }

  } else {
    stop("No autoplot defined for this API !", call. = FALSE)
  }
}





#' rte.data exported operators and S3 methods
#'
#' The following functions are imported and then re-exported
#' from the rte.package package to avoid loading them.
#'
# @noRd
#' @importFrom ggplot2 autoplot
#' @name autoplot
#' @export
NULL










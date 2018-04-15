
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




#' @importFrom data.table copy melt :=
#' @importFrom ggplot2 ggplot geom_col aes geom_point geom_text coord_flip
#'  theme_minimal theme labs scale_fill_manual scale_colour_manual
#'  guide_legend scale_y_continuous scale_x_discrete margin
#' @importFrom scales percent
plot_p_active_units <- function(object) {
  # data
  object <- copy(object)

  # date range
  subtitle <- paste0(
    "From ", format(min(object$start_date, na.rm = TRUE)),
    " to ", format(max(object$end_date, na.rm = TRUE))
  )

  # munging
  object[type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), type := "HYDRO"]
  object <- object[, list(
    working = round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2),
    stopped = 100 - round(sum(prod_max, na.rm = TRUE) / sum(installed_capacity, na.rm = TRUE) * 100, 2),
    n = .N
  ), by = type]
  object <- melt(data = object, id.vars = c("type", "n"), measure.vars = c("working", "stopped"))
  object[, variable := factor(x = variable, levels = c("stopped", "working"))]
  object <- object[is.finite(value)]
  object[value < 0, value := 0]
  object[value > 100, value := 100]

  # plot
  ggplot(data = object) +
    geom_col(
      mapping = aes(x = type, y = value, fill = variable),
      position = "fill", width = 0.5, alpha = 0.8, show.legend = FALSE
    ) +
    geom_point(
      mapping = aes(x = NA_character_, y = NA_real_, colour = variable),
      na.rm = TRUE
    ) +
    geom_text(
      # position = "fill",
      mapping = aes(
        label = paste(capitalize(type), paste0(round(value, 1), "% (n=", n, ")"), sep = " : "),
        x = type, y = 0
      ),
      hjust = 0, nudge_x = 0.5, size = 4.5,
      color = "black", data = object[variable == "working"]
    ) +
    coord_flip() + theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.subtitle = element_text(margin = margin(0, 0, 20, 0))
    ) +
    labs(
      title = "Proportion of active units by branch",
      subtitle = subtitle,
      fill = NULL, y = NULL, x = NULL, colour = NULL,
      caption = "https://data.rte-france.com"
    ) +
    scale_fill_manual(
      values = c("stopped" = "firebrick", "working" = "forestgreen")
    ) +
    scale_colour_manual(
      values = c("stopped" = "firebrick", "working" = "forestgreen"),
      guide = guide_legend(
        override.aes = list(shape = 16, size = 5), title.position = "top", label.position = "right",
        title.hjust = 0.5, label.hjust = 1, nrow = 1, byrow = TRUE, reverse = TRUE
      )
    ) +
    scale_y_continuous(labels = percent, expand = c(0.01, 0)) +
    scale_x_discrete(labels = NULL)
}



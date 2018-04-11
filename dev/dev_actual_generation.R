

#  ------------------------------------------------------------------------
#
# Title : Actual generation data
#    By : VP
#  Date : 2018-03-01
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( data.table )


# API key -----------------------------------------------------------------

# set_key(
#   api = "actual_generation",
#   key = "BASE64KEY=="
# )




# Actual generations per production type ----------------------------------


prod_type <- get_actual_generation("actual_generations_per_production_type", start_date = Sys.Date() - 1)
str(prod_type)
prod_type
autoplot(prod_type)


prod_type_ <- copy(prod_type)
prod_type_ <- prod_type_[production_type != "TOTAL"]
prod_type_[production_type %chin% c("HYDRO_RUN_OF_RIVER_AND_POUNDAGE", "HYDRO_WATER_RESERVOIR"), production_type := "HYDRO"]
prod_type_ <- prod_type_[, list(value = sum(value)), by = list(production_type, start_date)]
prod_type_ <- prod_type_[, order := factor(
  x = production_type, levels = c("BIOMASS", "FOSSIL_GAS", "FOSSIL_HARD_COAL", "FOSSIL_OIL",
                                  "HYDRO", "NUCLEAR", "SOLAR", "WASTE",
                                  "WIND_ONSHORE", "HYDRO_PUMPED_STORAGE")
)]
prod_type_ <- prod_type_[, order := as.numeric(order)]

ggplot(data = prod_type_) +
  geom_area(aes(x = start_date, y = value, fill = production_type, group = order), position = "stack") +
  labs(
    title = "French electricity generation per production type",
    subtitle = paste("Poduced on", Sys.time()),
    y = "Production (in MW)", x = NULL
  ) +
  scale_fill_manual(
    values = c(
      "BIOMASS" = "#166a57",
      "FOSSIL_GAS" = "#f30a0a",
      "FOSSIL_HARD_COAL" = "#ac8c35",
      "FOSSIL_OIL" = "#8356a2",
      "HYDRO_PUMPED_STORAGE" = "#114774",
      "HYDRO" = "#2772b2",
      "NUCLEAR" = "#f8ca4c",
      "SOLAR" = "#f27406",
      "WASTE" = "#61380B",
      "WIND_ONSHORE" = "#74cdb9"
    ), guide = guide_legend(
      title = "Production type", title.position = "top", title.hjust = 0,
      nrow = 2, label.position = "bottom", keywidth = 5, keyheight = 0.5
    ),
    labels = function(x) {
      lo <- substring(text = x, first = 2)
      up <- substring(text = x, first = 1, last = 1)
      lo <- tolower(lo)
      lo <- gsub(pattern = "_", replacement = " ", x = lo)
      paste0(up, lo)
    }
  ) +
  theme_minimal() + theme(legend.position = "bottom")






# Prod by unit ------------------------------------------------------------

prod_unit <- get_actual_generation("actual_generations_per_unit")
str(prod_unit, max.level = 2)

prod_unit[, list(mean = mean(value), median = median(value), max = max(value)), by = name]
prod_unit[name %like% "TRICASTIN", list(mean = mean(value), median = median(value), max = max(value)), by = name]





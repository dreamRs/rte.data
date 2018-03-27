

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
library(data.table)



# API key -----------------------------------------------------------------

# set_key(
#   api = "actual_generation",
#   key = "BASE64_KEY=="
# )




# Actual generations per production type ----------------------------------


prod_type <- get_actual_generation("actual_generations_per_production_type")
prod_type

autoplot(prod_type)


# Data since early 2018

prod_type_h <- get_actual_generation(
  resource = "actual_generations_per_production_type",
  start_date = "2018-01-01", end_date = "2018-03-01"
)
prod_type_h



prod_type_h_day <- prod_type_h[
  production_type != "TOTAL",
  list(value = mean(value)),
  by = list(start_date = as.Date(start_date), production_type)
]
setattr(prod_type_h_day, "api.name", attr(prod_type_h, "api.name"))
setattr(prod_type_h_day, "api.resource", attr(prod_type_h, "api.resource"))
setattr(prod_type_h_day, "api.time", attr(prod_type_h, "api.time"))

autoplot(prod_type_h_day)


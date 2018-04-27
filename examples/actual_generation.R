

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
autoplot(prod_type, interactive = TRUE)


# Data since early 2018
prod_type_h <- get_actual_generation(
  resource = "actual_generations_per_production_type",
  start_date = "2018-01-01", end_date = "2018-03-01"
)
prod_type_h

autoplot(prod_type_h, by_day = TRUE)



# Last 30 days
prod_type_30 <- get_actual_generation(
  resource = "actual_generations_per_production_type",
  start_date = Sys.Date() - 30, end_date = Sys.Date()
)
prod_type_30

autoplot(prod_type_30, by_day = TRUE)
autoplot(prod_type_30, by_day = TRUE, interactive = TRUE)


# Last 7 days
prod_type_7 <- get_actual_generation(
  resource = "actual_generations_per_production_type",
  start_date = Sys.Date() - 7, end_date = Sys.Date()
)
prod_type_7

autoplot(prod_type_7, by_day = TRUE)
autoplot(prod_type_7, by_day = TRUE, interactive = TRUE)

autoplot(prod_type_7, by_day = FALSE, interactive = TRUE)



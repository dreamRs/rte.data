

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



# API key -----------------------------------------------------------------

# set_key(
#   api = "actual_generation",
#   key = "BASE64_KEY=="
# )




# Actual generations per production type ----------------------------------


prod_type <- get_actual_generation("actual_generations_per_production_type")
prod_type


ggplot(data = prod_type[production_type != "TOTAL"]) +
  geom_line(aes(x = start_date, y = value, color = production_type)) +
  labs(
    title = "French electricity generation per production type",
    subtitle = paste("Poduced on", Sys.time())
  ) +
  theme_minimal()



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
prod_type_h_day

ggplot(data = prod_type_h_day) +
  geom_col(aes(x = start_date, y = value, fill = production_type), position = "stack") +
  labs(
    title = "French electricity generation per production type",
    subtitle = paste("Poduced on", Sys.time())
  ) +
  theme_minimal() + theme(legend.position = "bottom")

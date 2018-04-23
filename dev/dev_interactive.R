

#  ------------------------------------------------------------------------
#
# Title : Dev interactive viz
#    By : VP
#  Date : 2018-04-11
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )
library( data.table )
library( billboarder )




# Consumption -------------------------------------------------------------

short_term <- get_consumption("short_term", type = c("REALISED", "D-1"))
short_term

autoplot(short_term)

range_date <- function(x) {
  x <- unique(format(x, format = "%Y-%m-%d"))
  x <- sort(x)
  if (length(x) == 1) {
    return(x)
  } else {
    return(paste(range(x), collapse = " - "))
  }
}

billboarder(data = short_term) %>%
  bb_linechart(mapping = bbaes(x = start_date, y = value, group = type)) %>%
  bb_x_axis(tick = list(format = "%H:%M", fit = FALSE)) %>%
  bb_y_grid(show = TRUE) %>%
  bb_x_grid(show = TRUE) %>%
  bb_legend(position = "right") %>%
  bb_labs(
    title = "French electricity consumption : forecast vs realised",
    caption = "https://data.rte-france.com",
    y = "Electricity consumption (MW)"
  ) %>%
  bb_x_axis(label = list(text = range_date(short_term$start_date), position = "outer-center"))





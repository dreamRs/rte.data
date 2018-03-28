

#  ------------------------------------------------------------------------
#
# Title : Comsumption data
#    By : VP
#  Date : 2018-03-01
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( ggplot2 )



# API key -----------------------------------------------------------------

# set_key(
#   api = "consumption",
#   key = "BASE64_KEY=="
# )



# Short term consumption forecast -----------------------------------------

short_term <- get_consumption("short_term", type = c("REALISED", "D-1"))
short_term

autoplot(short_term)



short_term <- get_consumption("short_term", type = c("REALISED", "D-1"), start_date = Sys.Date() - 7)
short_term

autoplot(short_term)




# Weekly forecast ---------------------------------------------------------

# weekly_forecast <- get_consumption("weekly_forecast", type = c("REALISED", "D-1"))
# weekly_forecast
#
# weekly_forecast <- get_consumption("weekly_forecast")

weekly_forecast <- get_open_api(api = "consumption", resource = "weekly_forecast")


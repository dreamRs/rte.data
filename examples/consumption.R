

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







#  ------------------------------------------------------------------------
#
# Title : Physical flows
#    By : VP
#  Date : 2018-03-01
#
#  ------------------------------------------------------------------------



# Packages ----------------------------------------------------------------

library( rte.data )
library( data.table )
library( ggplot2 )



# API key -----------------------------------------------------------------

# set_key(
#   api = "physical_flow",
#   key = "BASE64KEY=="
# )




# Physical flows ----------------------------------------------------------


# commercial balance for France on electricity market
balance <- get_physical_flows(start_date = "2018-02-01", end_date = "2018-03-15")
autoplot(balance)






physfl <- get_open_api(api = "physical_flow", resource = "physical_flows")
physfl

physfl_a <- physfl[, list(value = sum(value)), by = list(from = sender_country_name, to = receiver_country_name)]
physfl_a

physfl_a[, type := ifelse(to == "France", "Imports", "Exports")]
physfl_a[, country := ifelse(to == "France", from, to)]
physfl_a[type == "Imports", value := -value]
physfl_a

ggplot(data = physfl_a) +
  geom_col(aes(x = country, y = value, fill = type))



# Balance

balance2 <- get_physical_flows()
autoplot(balance2)




balance[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
balance <- balance[, list(value = sum(value)), by = list(start_date = as.Date(format(start_date)), type)]
balance

ggplot(data = balance) +
  geom_line(aes(x = start_date, y = value, color = type)) +
  geom_ribbon(aes(x = start_date, ymin = ymin, ymax = ymax), fill = "firebrick", alpha = 0.3, data = function(data) {
    res <- data[, list(ymin = min(value), ymax = max(value), diff = diff(value)), by = start_date]
    res[diff > 0, `:=`(ymin = NA, ymax = NA)]
  }) +
  geom_ribbon(aes(x = start_date, ymin = ymin, ymax = ymax), fill = "goldenrod3", alpha = 0.3, data = function(data) {
    res <- data[, list(ymin = min(value), ymax = max(value), diff = diff(value)), by = start_date]
    res[diff <= 0, `:=`(ymin = NA, ymax = NA)]
  }) +
  scale_color_manual(values = c("firebrick", "goldenrod3")) +
  theme_minimal()


ggplot(data = balance) +
  geom_line(aes(x = as.POSIXct(start_date), y = value, color = type), size = 1) +
  geom_ribbon(aes(x = start_date, ymin = ymin, ymax = ymax, fill = fill, group = fill), alpha = 0.3, data = function(data) {
    res <- data[, list(ymin = min(value), ymax = max(value), diff = diff(value)), by = start_date]
    res <- res[, fill := ifelse(diff > 0, "favour", "against")]
    res <- res[, start_date := as.POSIXct(format(start_date))]
    data <- data[order(start_date)]
    x1 <- data$value[data$type == "Exports"]
    x2 <- data$value[data$type == "Imports"]
    above <- x1 > x2
    intp <- which(diff(above) != 0)
    x1s <- x1[intp + 1] - x1[intp]
    x2s <- x2[intp + 1] - x2[intp]
    xp <- intp + ((x2[intp] - x1[intp]) / (x1s - x2s))
    yp <- x1[intp] + (x1s * (xp - intp))
    addin <- data.table(
      start_date = rep(as.POSIXct(format(min(data$start_date))) + (xp - 1) * 60 * 60 * 24, each = 2),
      ymin = rep(yp, each = 2), ymax = rep(yp, each = 2),
      fill = rep(c("favour", "against"), times = 2),
      diff = rep(0, each = 2)
    )
    res <- rbind(res, addin)
    res[order(start_date, fill)]
  }) +
  scale_color_manual(values = c("firebrick", "goldenrod3"), name = "Flow") +
  scale_fill_manual(values = c("firebrick", "goldenrod3"), name = "Balance") +
  theme_minimal()

res <- balance[, list(ymin = min(value), ymax = max(value), diff = diff(value)), by = start_date]
res[, fill := ifelse(diff < 0, "favour", "against")]



cm <- rbind(balance$value[balance$type=="Exports"],balance$value[balance$type=="Imports"]) # Coefficient matrix
c(-solve(cbind(cm[,2],-1)) %*% cm[,1])

x1 <- balance$value[balance$type == "Exports"]
x2 <- balance$value[balance$type == "Imports"]
above <- x1 > x2
intp <- which(diff(above) != 0)
x1s <- x1[intp + 1] - x1[intp]
x2s <- x2[intp + 1] - x2[intp]
xp <- intp + ((x2[intp] - x1[intp]) / (x1s - x2s))
yp <- x1[intp] + (x1s * (xp - intp))
plot(x1,type='l')
lines(x2,type='l',col='red')
points(xp,yp,col='blue')



data <- copy(balance2)
data[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
data <- data[, list(value = sum(value)), by = list(start_date, type)]

res <- data[, list(ymin = min(value), ymax = max(value), diff = diff(range(value))), by = start_date]
res <- res[, fill := ifelse(diff > 0, "favour", "against")]
res <- res[, start_date := as.POSIXct(format(start_date))]
# data <- data[order(start_date)]
# x1 <- data$value[data$type == "Exports"]
# x2 <- data$value[data$type == "Imports"]
# above <- x1 > x2
# intp <- which(diff(above) != 0)
# x1s <- x1[intp + 1] - x1[intp]
# x2s <- x2[intp + 1] - x2[intp]
# xp <- intp + ((x2[intp] - x1[intp]) / (x1s - x2s))
# yp <- x1[intp] + (x1s * (xp - intp))
# addin <- data.table(
#   start_date = rep(as.POSIXct(format(min(data$start_date))) + (xp-1) * 60 * 60 * 24, each = 2),
#   ymin = rep(yp, each = 2), ymax = rep(yp, each = 2),
#   fill = rep(c("favour", "against"), times = 2),
#   diff = rep(0, each = 2)
# )
# res <- rbind(res, addin)
res[order(start_date, fill)]












library(data.table)

object <- copy(balance2)
object[, type := ifelse(receiver_country_name == "France", "Imports", "Exports")]
dd <- as.numeric(diff(range(object$start_date), unit = "days"))
if (dd > 7) {
  object <- object[, list(value = sum(value)), by = list(start_date = as.Date(format(start_date)), type)]
} else {
  object <- object[, list(value = sum(value)), by = list(start_date, type)]
}
ggplot(data = object) +
  geom_line(aes_(x = ~as.POSIXct(start_date), y = ~value, color = ~type), size = 1) +
  geom_ribbon(aes_(x = ~start_date, ymin = ~ymin, ymax = ~ymax, fill = ~fill, group = ~fill),
              alpha = 0.3, data = function(data) {
                res <- data[, list(ymin = min(value), ymax = max(value), diff = diff(value)), by = start_date]
                res <- res[, fill := ifelse(diff > 0, "in favour", "against")]
                res <- res[, start_date := as.POSIXct(format(start_date))]
                data <- data[order(start_date)]
                x1 <- data$value[data$type == "Exports"]
                x2 <- data$value[data$type == "Imports"]
                above <- x1 > x2
                intp <- which(diff(above) != 0)
                x1s <- x1[intp + 1] - x1[intp]
                x2s <- x2[intp + 1] - x2[intp]
                xp <- intp + ((x2[intp] - x1[intp]) / (x1s - x2s))
                yp <- x1[intp] + (x1s * (xp - intp))
                if (length(xp) > 0) {
                  addin <- data.table(
                    start_date = rep(as.POSIXct(format(min(data$start_date))) + (xp - 1) * 60 * 60 * 24, each = 2),
                    ymin = rep(yp, each = 2), ymax = rep(yp, each = 2),
                    fill = rep(c("in favour", "against"), times = 2),
                    diff = rep(0, each = 2)
                  )
                  res <- rbind(res, addin)
                }
                res[order(start_date, fill)]
              }) +
  labs(
    title = "Electricity balance for France",
    # subtitle = format(attr(object, "api.time"), "data for %Y-%m-%d, forecast at %Hh%M"),
    caption = "https://data.rte-france.com",
    x = NULL, y = "Physical flows (MW)"
  ) +
  scale_color_manual(values = c("firebrick", "goldenrod3"), name = "Flow") +
  scale_fill_manual(values = c("firebrick", "goldenrod3"), name = "Balance") +
  theme_minimal()









# By country --------------------------------------------------------------

dat <- get_physical_flows(start_date = "2018-02-01", end_date = "2018-03-15")
dat
autoplot(dat)


range_dat <- paste(format(range(dat$start_date), format = "%Y-%m-%d %H:%M"), collapse = " to ")
range_dat <- paste("From", range_dat)
datag <- dat[, list(value = sum(value)), by = list(sender_country_name, receiver_country_name)]
datag[, flow := ifelse(sender_country_name == "France", "Exports", "Imports")]
datag[flow == "Exports", value := value * -1L]
datag

datag[, country := ifelse(flow == "Imports", sender_country_name, receiver_country_name)]
datag[, country := factor(x = country, levels = rev(sort(unique(country))))]
datag <- datag[order(country)]
datag


ggplot(data = datag) +
  geom_col(mapping = aes(x = country, y = value, fill = flow)) +
  geom_hline(yintercept = 0, lty = "longdash", size = 1) +
  scale_fill_manual(values = c("firebrick", "goldenrod3"), name = "Flow") +
  scale_y_continuous(limits = c(-max(abs(datag$value)), max(abs(datag$value)))) +
  labs(
    title = "Exchanges with neighbouring countries",
    subtitle = range_dat,
    caption = "https://data.rte-france.com",
    x = NULL, y = "Physical flows (MW)"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(plot.title = element_text(face = "bold"))





# rte.data

> Access data from [RTE data portal](https://data.rte-france.com/)

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Travis build
status](https://travis-ci.org/dreamRs/rte.data.svg?branch=master)](https://travis-ci.org/dreamRs/rte.data)
[![R-CMD-check](https://github.com/dreamRs/rte.data/workflows/R-CMD-check/badge.svg)](https://github.com/dreamRs/rte.data/actions)
<!-- badges: end -->

## Overview

RTE, the french electricity transmission system operator, provides
access to various data through an API on its [data
portal](https://data.rte-france.com/home). You can retrieve those data
with `rte.data`.

Installation (from Github) :

``` r
remotes::install_github("dreamRs/rte.data")
```

## Authentication

To access the API, you need to [create an
account](https://data.rte-france.com/create_account), or login if you
have one.

Once logged, you can subscribe (create an application) to the desired
API (each APIs must be subscribed individualy), you’ll obtain an `client
id` and a `client secret` (or directly a base64 encoded key). Use those
to get a token :

``` r
# To create a token you can use id_client and id_secret
id_client <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
id_secret <- "XXXXX-XXXXX-XXXXX-XXXXX-XXXXX"
token <- get_token(
  key = list(id_client = id_client, id_secret = id_secret)
)

# or the base64 encoded key
key <- "TGEgZGF0YXNjaWVuY2UgYXZlYyB1biBncmFuZCBS="
token <- get_token(key)
```

With this token, you can query the API. You need to generate a token
every \~two hours, but you can use the same key each time.

So for future use, you can store your credentials in your `.Renviron`
with `set_key`.

## Examples

### Consumption

Data on French electricity consumption in real time, with forecast :

``` r
short_term <- get_consumption("short_term", type = c("REALISED", "D-1"))
short_term

autoplot(short_term)
```

<img src="images/consumption-1.png" width="864" />

### Physical flows

Data about physical cross-border schedules detailing electricity flows
actually transiting across the interconnection lines directly linking
countries.

``` r
balance <- get_physical_flows(start_date = "2018-02-01", end_date = "2018-03-15")
autoplot(balance)
```

<img src="images/balance-1.png" width="864" />

``` r
autoplot(balance, by_country = TRUE)
```

<img src="images/balance-2.png" width="864" />

## Actual generation

Generation data aggregated by sector and produced per group (in MW) on
an intradaily basis for net generation injected into the network.

``` r
prod_type_30 <- get_actual_generation(
  resource = "actual_generations_per_production_type",
  start_date = Sys.Date() - 30, end_date = Sys.Date()
)

autoplot(prod_type_30, by_day = TRUE)
```

<img src="images/generation-1.png" width="864" />

## Active production units

``` r
active_units <- retrieve_active_units()
autoplot(active_units)
```

<img src="images/active-units-1.png" width="864" />

or see on a map :

``` r
autoplot(active_units, map = TRUE)
```

<img src="images/active-units-map-1.png" width="864" />

## Generation installed capacities

``` r
inst_cap <- get_open_api(
  api = "generation_installed_capacities",
  resource = "capacities_per_production_unit"
)

autoplot(inst_cap)
```

<img src="images/installed-capacities-1.png" width="864" />

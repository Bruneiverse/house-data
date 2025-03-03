library(tidyverse)
library(bruneimap)
library(gt)
library(gtsummary)
library(lubridate)
library(GGally)

# Main data set
hsp <- 
  read_csv("data/hspbn_2025-03-03.csv") |>
  mutate(
    type = factor(type, levels = c("Detached", "Semi-Detached", "Terrace",
                                   "Apartment", "Land")),
    tenure = factor(tenure, levels = c("Freehold", "Leasehold", "Strata")),
    status = factor(status, levels = c("Proposed", "Under Construction", "New", "Resale")),
    date = as.Date(date, format = "%d/%m/%y"),
    quarter = zoo::as.yearqtr(quarter)
  )

# RPPI from BDCB
rppi <- 
  read_csv("data/rppi.csv") |>
  mutate(
    quarter = zoo::as.yearqtr(quarter),
    rppi = rppi / 100
  )

# To create median price per square foot, need to filter out "Land" types as
# well as missing property types. Then create an "Overall" type.
hsp_all <- 
  bind_rows(
    hsp, 
    mutate(hsp, type = "Overall")
  ) |> 
  arrange(date) 

# Create a median price per square foot index
hsp_rppi <-
  slider::slide_period_dfr(hsp, hsp$date, "month", \(df) {
    df |>
      filter(type != "Land") |>
      drop_na(floor_area) |>
      summarise(
        quarter = first(quarter),
        price = median(price, na.rm = TRUE),
        # plot_area = median(plot_area, na.rm = TRUE),
        floor_area = median(floor_area, na.rm = TRUE)
      )
  }, .before = 1, .after = 1) |>
  summarise(across(price:floor_area, \(x) median(x, na.rm = TRUE)), .by = quarter) |> 
  drop_na(quarter) |>
  mutate(
    price_per_sqft = price / floor_area,
    index = price_per_sqft / price_per_sqft[quarter == "2015 Q1"],
  ) |>
  right_join(rppi, by = join_by(quarter)) 

rppi_mae <-
  hsp_rppi |>
  summarise(
    rmse = (mean( abs(rppi - index) ^ 1)),
    range = max(c(rppi)) - min(c(rppi)),
    mean = mean(c(rppi)),
    sd = sd(c(rppi))
  ) |>
  unlist()

# (rmse <- as.numeric(rppi_mae[1]))
# (nmae <- (rppi_mae[1] / rppi_mae[-1])[2])

# Create an sf data frame for plotting Brunei map
hsp_mkm <-
  hsp |>
  summarise(
    price = median(price, na.rm = TRUE, trim = 0.05),
    .by = mukim
  ) |>
  left_join(x = bruneimap::mkm_sf, by = join_by(mukim)) 

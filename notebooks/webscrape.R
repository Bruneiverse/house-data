library(tidyverse)
library(rvest)

# This is how you get read the HTML into R
url <- "https://BRUNEIPROPERTYLISTINGWEBSITE.COM/blablabla"  # change this to the actual URL
html <- read_html(url)

# Extract the house prices
prices <-
  html |>
  html_elements(".property-price") |>
  html_text2()

# Clean up
prices <- 
  str_remove_all(prices, "[^0-9]") |>  # Remove non-numeric characters
  as.integer()

# Do same thing for number of beds, baths, location, and other remarks
beds <-
  html |>
  html_elements(".property-bed") |>
  html_text2() |>
  as.integer()

baths <-
  html |>
  html_elements(".property-bath") |>
  html_text2() |>
  as.integer()

location <-
  html |>
  html_elements(".property-address") |>
  html_text2()

# Put it all in a data frame
hsp_df <- tibble(
  location = location,
  price = prices,
  beds = beds,
  baths = baths,
)

# Some pages require you to click a "load more" button to see more data
links <-
  html |>
  html_elements(".property-link") |>
  html_attr("href")

# Create a function that can extract the info I want from a single page
extract_info <- function(i) {
  link <- paste0("https://BRUNEIPROPERTYLISTINGWEBSITE.COM", properties[i])  # change this URL
  html <- read_html(link)
  out <-
    html |>
    html_elements("p") |>  # The descriptions are the .p element
    html_text2()
  out[1]
}

res <- map(
  .x = seq_along(properties),
  .f = extract_info,
  .progress = TRUE
)
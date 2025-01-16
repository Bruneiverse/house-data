library(tidyverse)
# remotes::install_github("AlbertRapp/tidychatmodels")
library(tidychatmodels)

# Note: Before starting, ensure to install Ollama (https://ollama.com), and use
# the terminal to download the model you wish to use. Here, we use llama3.1.

# Sample description (artificially created)
x <- "
🌟 MODERN DOUBLE STOREY DETACHED HOUSE FOR SALE — TUNGKU 🌟

🏡 Prime Location: Nestled in the heart of Kampong Tungku, just a 5-minute drive to The Mall Gadong, this property offers a blend of modern design and urban convenience.

✨ Property Details:
	•	Land Title: In-Perpetuity
	•	Selling Price: $350,000
	•	Land Size: 0.15 acre
	•	House Size: 3,000 sq. ft
	•	Bedrooms: 4
	•	Bathrooms: 4
	
✨ House Features:
	•	Ground Floor: Spacious Living & Dining Area, Wet & Dry Kitchen, 1 Guest Bedroom (en-suite), Guest Toilet, Laundry Room.
	•	First Floor: Master Bedroom (en-suite with walk-in closet + balcony), 2 Bedrooms (en-suite).
	
🚗 Parking: Car porch fits 2 cars.
🌳 Nearby Amenities: Schools, hospitals, cafes, and parks all within a 10-minute radius.

📲 Contact Us Today!
For viewing and further details, please contact:
📞 +673 xxxxxxx | 📱 WhatsApp: +673 xxxxxxx
📧 email@example.com

🛠️ Services We Provide:
	•	🏦 Bank Loan Application Assistance
	•	📑 Legal Documentation Support
	
AGENT 🏘️ – Your Trusted Real Estate Partner
"

# The prompt
the_prompt <- "
The following is the description from a property sale listing in Brunei. This description will contain the information about the property, including its characteristics, price, and location. However, some of these descriptions may not contain property listings, and instead contain other or no information at all.

In the case where this description is in fact a property listing, I would like you to extract the following information:

1. Location / area of the property in Brunei, CHARACTER.
2. Price of the property in Brunei Dollars, NUMERIC.
3. Type of property, CHARACTER -- select from Detached, Semi-Detached, Terrace, Apartment, or Land.
4. Land tenure, CHARACTER -- select from Freehold, Leasehold, or Strata. If other than this, return 'NA'.
5. Status of the property, CHARACTER -- select from Proposed, Under Construction, New, or Resale.
6. Land area in acres, NUMERIC.
7. Built up area in square feet, NUMERIC.
8. Number of storeys, INTEGER.
9. Number of bedrooms, INTEGER.
10. Number of bathrooms, INTEGER.

Further instructions:

- Please return **semicolon** separated values like this:

  Kg Tanah Jambu; 250000; Detached; Freehold; New; 0.3; 2500; 2;  3; 3
  Kg Tungku; 300000; Terrace; Leasehold; Resale; 0.25; 1700; 2;  3; 2 
  Kg Kiarong; 200000; Apartment; Strata; Proposed; 0.1; 1000; NA; 2; 2
  etc.
  NUMBERS SHOULD NOT CONTAIN comma (,) for thousands separator

- If any of the 10 values are missing, please return 'NA' for that value.

- If the description does not contain a property listing (for example, it is a rental property advertisement), return 'NA' for all 10 values.

- DO NOT RESPOND WITH ANYTHING ELSE OTHER THAN THE REQUIRED INFORMATION.

------------------------------
"

# Function to clean the descriptions
clean_desc <- function(caption) {
  create_chat("ollama") |>
    add_model("llama3.1") |>
    add_message(paste0(the_prompt, caption)) |>
    perform_chat() |>
    extract_chat(silent = TRUE) |>
    dplyr::filter(role == "assistant") |>
    dplyr::pull(message)
}

# Test Ollama works
create_chat("ollama") |>
  add_model("llama3.1") |>
  add_message("Hello, how are you?") |>
  perform_chat() |>
  extract_chat()

# Test the function
microbenchmark::microbenchmark(
  y <- clean_desc(x),
  times = 10
)

# Next, might want to map this function to a column of descriptions in a data
# frame, like so:
# cleaned_descriptions <-
#   map(
#     .x = hsp$desc,
#     .f = possibly(clean_desc, NA),
#     .progress = TRUE
#   )

# In any case, to convert the (cleaned) character vector to a data frame, we
# separate the strings by ';' as follows:
df <-
  tibble(clean = y) |>
  separate(
    clean,
    into = c("location", "price", "type", "tenure", "status", "plot_area", "floor_area", "storeys", "beds", "baths"),
    sep = ";",
    remove = FALSE
  ) |> 
  mutate(across(plot_area:baths, as.numeric))

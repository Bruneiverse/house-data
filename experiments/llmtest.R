library(tidyverse)
library(microbenchmark)
library(ellmer)
library(stringdist)
set.seed(123) 
hsp_test <- read_csv("experiments/llmtest.csv") 

## ----- The prompt ------------------------------------------------------------
# the_prompt <- "
# The following is the description from a property sale listing in Brunei. This description will contain the information about the property, including its characteristics, price, and location. However, some of these descriptions may not contain property listings, and instead contain other or no information at all.
# 
# In the case where this description is in fact a property listing, I would like you to extract the following information:
# 
# 1. Location / area of the property in Brunei, CHARACTER.
# 2. Price of the property in Brunei Dollars, NUMERIC.
# 3. Type of property, CHARACTER -- select from Detached, Semi-Detached, Terrace, Apartment, or Land.
# 4. Land tenure, CHARACTER -- select from Freehold, Leasehold, or Strata. If other than this, return 'NA'.
# 5. Status of the property, CHARACTER -- select from Proposed, Under Construction, New, or Resale.
# 6. Land area in acres, NUMERIC.
# 7. Built up area in square feet, NUMERIC.
# 8. Number of storeys, INTEGER.
# 9. Number of bedrooms, INTEGER.
# 10. Number of bathrooms, INTEGER.
# 
# Further instructions:
# 
# - Please return **semicolon** separated values like this:
# 
#   Kg Tanah Jambu; 250000; Detached; Freehold; New; 0.3; 2500; 2;  3; 3
#   Kg Tungku; 300000; Terrace; Leasehold; Resale; 0.25; 1700; 2;  3; 2
#   Kg Kiarong; 200000; Apartment; Strata; Proposed; 0.1; 1000; NA; 2; 2
#   etc.
#   NUMBERS SHOULD NOT CONTAIN comma (,) for thousands separator
# 
# - If any of the 10 values are missing, please return 'NA' for that value.
# 
# - If the description does not contain a property listing (for example, it is a rental property advertisement), return 'NA' for all 10 values.
# 
# - DO NOT RESPOND WITH ANYTHING ELSE OTHER THAN THE REQUIRED INFORMATION.
# 
# ------------------------------
# "

the_prompt <- "
You are an information extraction engine. You will be given a text description that may or may not be a property sale listing from Brunei. If the description is a property listing, extract the following 10 fields in order; if not, output 'NA' for all values.

Extract the fields as follows (in this exact order), separating each with a semicolon (;) with no additional text:
	1.	Location/Area – The location or area of the property in Brunei (text).
	2.	Price – The price in Brunei Dollars (numeric, do not include commas).
	3.	Property Type – One of: Detached, Semi-Detached, Terrace, Apartment, or Land. If not one of these or not present, output 'NA'.
	4.	Land Tenure – One of: Freehold, Leasehold, or Strata. If it is any other value or missing, output 'NA'.
	5.	Property Status – One of: Proposed, Under Construction, New, or Resale. If not present, output 'NA'.
	6.	Land Area – The land area in acres (numeric).
	7.	Built-up Area – The built-up area in square feet (numeric).
	8.	Number of Storeys – The number of storeys (integer). If missing, output 'NA'.
	9.	Number of Bedrooms – The number of bedrooms (integer).
	10.	Number of Bathrooms – The number of bathrooms (integer).

Additional rules:
	- The fields should be in the exact order as listed above, separated by semicolons.
	- If any of the fields are missing, output 'NA' for that field.
	- If the description does not contain a property sale listing (e.g., it’s a rental ad or irrelevant text), return 'NA' for all 10 fields.
	- NUMBERS SHOULD NOT CONTAIN comma (,) for thousands separator
	- DO NOT RESPOND WITH ANYTHING ELSE OTHER THAN THE REQUIRED INFORMATION.

Example output (one line, semicolon-separated):
Kg Tanah Jambu; 250000; Detached; Freehold; New; 0.3; 2500; 2;  3; 3
Kg Tungku; 300000; Terrace; Leasehold; Resale; 0.25; 1700; 2;  3; 2 
Kg Kiarong; 200000; Apartment; Strata; Proposed; 0.1; 1000; NA; 2; 2

------------------------------

"

## ----- Function to clean the descriptions ------------------------------------
clean_desc <- function(caption, vendor = "ollama", model = "deepseek-r1:8b") {
  if (vendor == "tcm") {
    # cli::cli_alert_info("Using tidychatmodels")
    require(tidychatmodels)
    out <-
      create_chat("ollama") |>
      add_model(model) |>
      add_message(paste0(the_prompt, caption)) |>
      add_params(temperature = 0.1) |>
      perform_chat() |>
      extract_chat(silent = TRUE) |>
      dplyr::filter(role == "assistant") |>
      dplyr::pull(message)
  } else {
    if (vendor == "openai") {
      chat <- chat_openai(
        system_prompt = NULL,
        turns = NULL,
        base_url = "https://api.openai.com/v1",
        model = model,
        seed = NULL,
        # api_args = list(temperature = 0.1),
        echo = "none"
      )
    } else if (vendor == "google") {
      chat <- chat_gemini(
        system_prompt = NULL,
        turns = NULL,
        base_url = "https://generativelanguage.googleapis.com/v1beta/",
        model = model,
        # api_args = list(temperature = 0.1),
        echo = "none"
      )
    } else if (vendor == "deepseek") {
      chat <- chat_deepseek(
        system_prompt = NULL,
        turns = NULL,
        base_url = "https://api.deepseek.com",
        seed = NULL,
        model = model,
        api_args = list(temperature = 0.1),
        echo = "none"
      )
    } else if (vendor == "ollama") {
      chat <- chat_ollama(
        system_prompt = NULL,
        turns = NULL,
        base_url = "http://localhost:11434",
        model = model,
        seed = NULL,
        api_args = list(temperature = 0.1),
        echo = "none"
      )
    }
    out <- chat$chat(paste0(the_prompt, caption))
  }

  out <- gsub("(?s)<think>.*?</think>", "", out, perl = TRUE)
  out <- gsub(pattern = "\n", replacement = "", out)
  out
}

# Test
# clean_desc(hsp_test$listing[1], vendor = "openai", model = "gpt-4o")
# clean_desc(hsp_test$listing[1], vendor = "google", model = "gemini-2.0-pro-exp-02-05")
# clean_desc(hsp_test$listing[1], vendor = "tcm", model = "mistral")
# clean_desc(hsp_test$listing[1], vendor = "tcm", model = "llama3.2")
# clean_desc(hsp_test$listing[1], vendor = "deepseek", model = "deepseek-reasoner")

# Function to compute standardised tolerance-based similarity
calc_simil <- function(x, y, eps = 0.02) {
  res <- x
  res[] <- NA
  
  for (col in colnames(x)) {
    if (is.numeric(x[[col]])) {
      # Numeric: Check if values are within the allowable tolerance
      mean_val <- mean(abs(x[[col]]), na.rm = TRUE)
      res[, col] <- abs(x[[col]] - y[[col]]) <= eps * mean_val
      
    } else if (is.character(x[[col]])) {
      # Character: Check if Normalized Levenshtein Distance is within the tolerance
      distances <- stringdist(x[[col]], y[[col]], method = "lv")
      max_length <- pmax(nchar(x[[col]]), nchar(y[[col]]), na.rm = TRUE)
      norm_dist <- ifelse(max_length > 0, distances / max_length, NA)
      res[, col] <- norm_dist <= eps
      
    } else {
      res[, col] <- NA
      warning(paste("Unsupported column type in", col))
    }
    # Check match NA
    # res[, col] <- (is.na(x[[col]]) == is.na(y[[col]])) & res[, col]
  }
  
  list(
    columnwise = colMeans(res, na.rm = TRUE),
    overall = mean(unlist(res), na.rm = TRUE),
    overall2 = mean(unlist(res[c(2,3,4,6,7,8,9)]), na.rm = TRUE)
  )
}

## ----- Timing ----------------------------------------------------------------
# z <- hsp_test$listing[1]
# microbenchmark(
#   `llama3.2` = clean_desc(z, "tcm", "llama3.2"),
#   `mistral` = clean_desc(z, "tcm", "mistral"),
#   `phi4` = clean_desc(z, "tcm", "phi4"),
#   `deepseek-r1:8b` = clean_desc(z, "tcm", "deepseek-r1:8b"),
#   `deepseek-r1:14b` = clean_desc(z, "tcm", "deepseek-r1:14b"),
#   `gpt-4o` = clean_desc(z, "openai", "gpt-4o"),
#   `o1-mini` = clean_desc(z, "openai", "o1-mini"),
#   # `gemini-2.0-flash` = clean_desc(z, "google", "gemini-2.0-flash"),
#   times = 5
# )

## ----- Run models ------------------------------------------------------------
vm_df <- tibble(
  vendor = c(rep("tcm", 5), rep("openai", 2), rep("google", 3)),
  model = c("llama3.2", "mistral", "phi4", "deepseek-r1:8b", "deepseek-r1:14b", 
            "gpt-4o", "o1-mini", "gemini-2.0-flash", "gemini-2.0-pro-exp-02-05", 
            "gemini-2.0-flash-thinking-exp-01-21")
) 
nmods <- nrow(vm_df)
  
for (i in seq_len(nmods)) {
  the_vendor <- vm_df$vendor[i]
  the_model <- vm_df$model[i]
  cli::cli_alert_info(glue::glue("[{i}/{nrow(vm_df)}]: Running model {the_model} from {the_vendor}"))
  
  idx <- seq_len(nrow(hsp_test))
  cleaned_descriptions <- rep(NA_character_, nrow(hsp_test))
  ntry <- 0
  while (length(idx) > 0 & ntry <= 10) {
    cd_tmp <-
      map(
        .x = hsp_test$listing[idx],
        .f = possibly(\(x) clean_desc(x, the_vendor, the_model), NA),
        .progress = TRUE
      )
    cleaned_descriptions[idx] <- cd_tmp
    idx <- which(str_count(unlist(cleaned_descriptions), ";") < 9)
    ntry <- ntry + 1
  }
  
  # Convert the (cleaned) character vector to a data frame, we separate the
  # strings by ';' as follows:
  df <-
    tibble(clean = cleaned_descriptions) |>
    separate(
      clean,
      into = c("location", "price", "type", "tenure", "status", "plot_area", "floor_area", "storeys", "beds", "baths"),
      sep = ";"
    ) |> 
    mutate(
      across(c(price, plot_area:baths), as.numeric),
      across(c(type, tenure, status), \(x) {
        x <- gsub(" ", "", x)
        x[grepl("NA", x)] <- NA
        x
      }),
      location = case_when(
        grepl("NA", location) ~ NA_character_,
        TRUE ~ location
      )
    )
  save(df, file = paste0("experiments/", the_model, ".RData"))
}

## ----- Calculate similarity --------------------------------------------------
mods <- paste0("experiments/", vm_df$model, ".RData")
RES <- vector("list", nrow(vm_df))
names(RES) <- vm_df$model
y <- hsp_test |>
  select(location = kampong, price:status, plot_area, floor_area, beds, baths) |>
  mutate(
    location = gsub("Kg. ", "", location),
    status = gsub(" ", "", status)
  )

for (i in seq_len(nmods)) {
  load(mods[i])

  x <- df |>
    select(-storeys) |>
    mutate(
      location = gsub("Kg. |Kg ", "", location),
      location = gsub("^ ", "", location),
      status = gsub(" ", "", status)
    )
  
  RES[[i]] <- calc_simil(x, y, eps = 0.01)
}

p_llm_test <-
  map(RES, \(x) {
    out <- as.data.frame(as.list(x$columnwise))
    out$overall <- x$overall
    out
  }) |>
  bind_rows(.id = "model") |>
  pivot_longer(cols = -model, names_to = "field", values_to = "precision") |>
  mutate(
    model = factor(model, levels = vm_df$model),
    field = factor(field, levels = c("overall", "location", "price", "type", "tenure", "status", "plot_area", "floor_area", "beds", "baths")),
    kind = case_when(
      field == "overall" ~ "Overall",
      TRUE ~ "Variables"
    )
  ) |>
  filter(!grepl("gemini", model)) |>
  ggplot(aes(field, precision, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_fill_viridis_d(option = "mako", end = 0.94) +
  # scale_fill_brewer(palette = "Paired") +
  # scale_fill_manual(values = c(
  #   "#1F78B4", "#33A02C", "#E31A1C", "#A6CEE3", "#FF7F00", "#6A3D9A", "#CAB2D6"
  # )) +
  scale_y_continuous(labels = scales::percent, expand = c(0,0,0,0.025)) +
  theme_bw() +
  facet_grid(~kind, scales = "free_x", space = "free_x") +
  labs(x = NULL, y = "Accuracy", fill = "Model"); p_llm_test

save(RES, p_llm_test, file = "experiments/llm_test.RData")

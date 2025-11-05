library(RCurl)
library(jsonlite)
library(dplyr)
library(httr)
library(tidyverse)
library(janitor)
library(rvest)

paper_search <- function(search_text, 
                         n_papers = 200, 
                         year_start = 2000, 
                         year_end = 2012, 
                         NBER = FALSE,
                         jel_codes = NULL) {
  
  # Define headers
  headers <- c(
    "accept" = "*/*",
    "accept-language" = "en-US,en;q=0.9",
    "origin" = "https://paulgp.com",
    "priority" = "u=1, i",
    "referer" = "https://paulgp.com/",
    "sec-ch-ua" = "\"Google Chrome\";v=\"141\", \"Not?A_Brand\";v=\"8\", \"Chromium\";v=\"141\"",
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = "\"Windows\"",
    "sec-fetch-dest" = "empty",
    "sec-fetch-mode" = "cors",
    "sec-fetch-site" = "cross-site",
    "user-agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/141.0.0.0 Safari/537.36"
  )
  
  # Define journals based on NBER flag
  if (NBER) {
    journals <- c("NBER+Working+Papers")
    only_full_text <- "true"  # Optional: NBER is often only full-text
  } else {
    journals <- c(
      "American+Economic+Journal%3A+Applied+Economics",
      "American+Economic+Journal%3A+Economic+Policy",
      "American+Economic+Journal%3A+Macroeconomics",
      "American+Economic+Journal%3A+Microeconomics",
      "American+Economic+Review"
    )
    only_full_text <- "false"
  }
  
  # Construct journals part of URL
  journals_str <- paste0("&journals=", paste(journals, collapse = "&journals="))
  
  # Construct JEL code part (if provided)
  jel_str <- ""
  if (!is.null(jel_codes) && length(jel_codes) > 0) {
    jel_str <- paste0("&jel_codes=", paste(jel_codes, collapse = "&jel_codes="))
  }
  
  # Build full URL
  url <- paste0(
    "https://econlit-api.onrender.com/api/search?q=", 
    URLencode(search_text), 
    "&limit=", n_papers,
    "&search_in=all&only_full_text=", only_full_text,
    journals_str,
    "&year_start=", year_start, 
    "&year_end=", year_end,
    jel_str
  )
  
  # Make request
  res <- getURL(url, .opts = list(httpheader = headers, followlocation = TRUE))
  
  # Parse JSON safely
  json_content <- fromJSON(res)
  
  # Convert to dataframe (if results exist)
  if (!is.null(json_content$results)) {
    df <- json_content$results %>% as.data.frame()
  } else {
    df <- data.frame()
    warning("No results found or invalid response.")
  }
  
  return(df)
}

# Example usage: JEL are "OR"
# AER search only: goes back till 2011 only
papers_aer <- paper_search("public finance", year_start = 2011, year_end = 2011, jel_codes = c("D", "H"))

# NBER search only: it goes way back (1990 ish)
papers_nber <- paper_search("public finance", NBER = TRUE, year_start = 2011, year_end = 2011, jel_codes = c("H"))

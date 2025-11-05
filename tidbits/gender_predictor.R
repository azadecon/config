library(httr)
library(readr)  # for reading CSV easily

predict_gender <- function(local_file, save_result = TRUE, dest_csv = "gender_prediction_result.csv") {
  # Check if file exists
  if (!file.exists(local_file)) {
    stop("The specified file does not exist.")
  }
  
  # Read CSV and validate structure
  data <- read_csv(local_file, show_col_types = FALSE)
  
  if (ncol(data) != 1 || names(data) != "name") {
    stop("CSV must have exactly one column named 'name'.")
  }
  
  # Send POST request to the gender classifier
  response <- POST(
    url = "http://gender-classifier.devdatalab.org/",
    body = list(file = upload_file(local_file)),
    encode = "multipart"
  )
  
  # Get content as text
  content_text <- content(response, as = "text", encoding = "UTF-8")
  
  # Optionally save result
  if (save_result) {
    writeLines(content_text, dest_csv)
  }
  
  # Return content
  return(content_text)
}

# Example usage:
result <- predict_gender("./tidbits/test.csv", save_result = TRUE, dest_csv = "./tidbits/result.csv")
cat(result)

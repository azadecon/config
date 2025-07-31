######################################################################
## PURPOSE: to check if one column uniquely identifies a dataframe  ##
######################################################################

library(dplyr)

isid_r <- function(data_fram, ...) {
  # Ensure that the provided data_frame is actually a data frame
  if (!is.data.frame(data_fram)) {
    stop("The input is not a data frame.")
  }
  
  # Capture the column names passed after the data frame and create a vector
  varz <- c(...)
  
  # Use group_by and count to check for duplicates
  result <- data_fram %>%
    group_by(across(all_of(varz))) %>%
    tally() %>%
    filter(n > 1)
  
  # If there are any groups with more than one row, return FALSE (not unique), otherwise TRUE
  return(nrow(result) == 0)
}

# Example usage:
# Assuming 'df' is your data frame and you want to check uniqueness for columns "DISTRICT_ID", "TP_ID", "VILLAGE_ID"
mx <- mtcars %>% mutate(nk = 1:n())
result <- isid_r(mx, "nk")
print(result)

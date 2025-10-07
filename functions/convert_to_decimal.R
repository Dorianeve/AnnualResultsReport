# function for availability of evidence

convert_to_decimal <- function(x) {
  if (grepl("%", x)) {
    # Remove the '%' sign and convert to numeric
    x_numeric <- as.numeric(gsub("%", "", x))
    # Convert to decimal
    return(x_numeric / 100)
  } else {
    # Convert already decimal strings to numeric
    return(as.numeric(x))
  }
}

# function to check which LeadGRN and GMGRN are missing in GrantDB and CR

create_missing_df <- function(missing_ids, type_label) {
  if (length(missing_ids) > 0) {
    return(data.frame(ID = missing_ids, MissingType = type_label, stringsAsFactors = FALSE))
  } else {
    return(NULL)  # Return NULL if no missing values
  }
}
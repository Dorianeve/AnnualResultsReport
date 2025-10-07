## functioning

merge_stata_style <- function(df1, df2, by) {
  # Identify overlapping non-key columns to avoid duplications and conflicts
  overlap <- setdiff(intersect(names(df1), names(df2)), by)
  rename_cols <- setNames(paste0(overlap, ".y"), overlap)
  df2_renamed <- df2
  names(df2_renamed)[names(df2_renamed) %in% overlap] <- rename_cols[names(df2_renamed)[names(df2_renamed) %in% overlap]]
  
  # Perform the merge with all.x and all.y set to TRUE for full outer join
  merged_df <- merge(df1, df2_renamed, by = by, all = TRUE)
  
  # Create the '_merge' indicator column based on NA presence in key columns
  merged_df$merge <- with(merged_df, ifelse(!is.na(merged_df[[by]]) & rowSums(is.na(merged_df[names(df1)])) == 0 & 
                                              rowSums(is.na(merged_df[rename_cols])) == length(rename_cols), 1, 
                                            ifelse(rowSums(is.na(merged_df[names(df1)])) == 0, 3, 2)))
  
  # Replace NA values in df1 columns with values from df2 where applicable
  for (col in overlap) {
    original_col <- col
    renamed_col <- paste0(col, ".y")
    merged_df[[original_col]] <- ifelse(is.na(merged_df[[original_col]]), merged_df[[renamed_col]], merged_df[[original_col]])
  }
  
  # Remove the renamed columns
  merged_df[rename_cols] <- NULL
  
  return(merged_df)
}



# function for filtering the AvailabilityOfEvidence dataset

collapse_rows <- function(df) {
  df <- df %>%
    mutate(EvidenceavailableNumeric = ifelse(is.na(EvidenceavailableNumeric), 0, EvidenceavailableNumeric))
  if (all(df$EvidenceavailableNumeric == 0, na.rm = TRUE) > 0) {
    return(df[1, ])
  } else if (sum(df$EvidenceavailableNumeric == 1, na.rm=TRUE) == 1) {
    return (df[df$EvidenceavailableNumeric == 1, ] [1, ])
  } else if (sum(df$EvidenceavailableNumeric == 1, na.rm=TRUE) > 1) {
    prim_rows <- df[df$EvidenceavailableNumeric==1 & df$include==1, ]
    #print(prim_rows)
    if (nrow(prim_rows) > 0) {
      return(prim_rows[1, ])}
    else {
      return(df[df$EvidenceavailableNumeric == 1, ] [1, ])
    }
  }
}

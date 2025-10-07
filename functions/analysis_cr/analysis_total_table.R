# function to create total tables
# usage total_table(df, "Reached")

total_table <- function(df, status) {
  
  # filter
  df %<>%
    filter(Status == status &
             ActiveStrategicPlan == "Yes" &
             (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
             (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
             Covid19related == "Non-Covid-19 related data")
  
  # totals
  total <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    mutate(Attribute = "Total (FER + MYRP)")
  total
  
  # gender
  gender <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    rename(Attribute = Gender)
  
  # type of beneficiary
  bens <- df %>%
    filter(Typeofbeneficiary != "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofbeneficiary) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    rename(Attribute = Typeofbeneficiary)
  bens
  
  # levelofeducation
  edu_level <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation != "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Levelofeducation) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    rename(Attribute = Levelofeducation)
  
  # typeofeducation
  edu_type <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofeducation) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    rename(Attribute = Typeofeducation)
  
  # join
  tb <- bind_rows(total, gender, bens, edu_level, edu_type)
  
  # Prep bens gender disag
  gender_bens <- df %>%
    filter(Typeofbeneficiary != "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofbeneficiary, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    rename(Attribute = Typeofbeneficiary) %>%
    pivot_wider(names_from = Gender, values_from = Total) %>%
    mutate(Total = Female + Male,
           PerFemale = Female / Total) %>%
    select(-c(Female, Male, Total)) %>%
    mutate(Exercise = paste0(Exercise, "_", "PerFemale")) %>%
    pivot_wider(names_from = Exercise, values_from = PerFemale)
  
  # Prep edu gender disag
  gender_edu <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation != "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Levelofeducation, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    rename(Attribute = Levelofeducation) %>%
    pivot_wider(names_from = Gender, values_from = Total) %>%
    mutate(Total = Female + Male,
           PerFemale = Female / Total) %>%
    select(-c(Female, Male, Total)) %>%
    mutate(Exercise = paste0(Exercise, "_", "PerFemale")) %>%
    pivot_wider(names_from = Exercise, values_from = PerFemale)
  
  # Prep edu_type gender disag
  gender_edu_type <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation != "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofeducation, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    rename(Attribute = Typeofeducation) %>%
    pivot_wider(names_from = Gender, values_from = Total) %>%
    mutate(Total = Female + Male,
           PerFemale = Female / Total) %>%
    select(-c(Female, Male, Total)) %>%
    mutate(Exercise = paste0(Exercise, "_", "PerFemale")) %>%
    pivot_wider(names_from = Exercise, values_from = PerFemale)
  
  
  gender_disag <- bind_rows(gender_bens, gender_edu, gender_edu_type)
  
  
  # Table processing ----
  # pivot wider
  tb %<>%
    pivot_wider(names_from = Attribute, values_from = Total)
  
  # Step 1: Add percentage columns
  tb %<>%
    mutate(
      `PerFemale` = Female / `Total (FER + MYRP)`,
      `PerMale` = Male / `Total (FER + MYRP)`,
      `PerRefugee` = Refugee / `Total (FER + MYRP)`,
      `PerIDP` = IDP / `Total (FER + MYRP)`,
      `PerOAPs` = OAPs / `Total (FER + MYRP)`,
      `PerUnknown` = Unknown / `Total (FER + MYRP)`,
      `PerPre-primary` = `Pre-primary` / `Total (FER + MYRP)`,
      `PerPrimary` = Primary / `Total (FER + MYRP)`,
      `PerSecondary` = Secondary / `Total (FER + MYRP)`,
      `PerUnknown level of education` = `Unknown level of education` / `Total (FER + MYRP)`,
      `PerChildren with disabilities` = `Children with disabilities` / `Total (FER + MYRP)`,
      `PerFormal education` = `Formal education` / `Total (FER + MYRP)`,
      `PerNon-formal education` = `Non-formal education` / `Total (FER + MYRP)`
    )
  
  # Step 2: Pivot longer and extract correct BaseVar
  tb_long <- tb %>%
    pivot_longer(-Exercise, names_to = "Variable", values_to = "Value") %>%
    mutate(
      Type = if_else(str_starts(Variable, "Per"), "Per", "Total"),
      BaseVar = if_else(
        Type == "Per",
        str_remove(Variable, "^Per"),
        Variable
      )
    )
  
  # Step 3: Pivot wider with aligned BaseVar
  tb_wide <- tb_long %>%
    pivot_wider(
      id_cols = BaseVar,
      names_from = c(Exercise, Type),
      values_from = Value,
      names_sep = " "
    ) %>%
    rename(Variable = BaseVar)
  
  # Step 4: Join with gender disaggregation
  tb_wide %<>%
    left_join(gender_disag, by = c("Variable" = "Attribute"))
  
  # Get all the current column names (excluding 'Variable')
  col_order <- names(tb_wide)[-1]
  
  # Separate and sort into cumulative and annual groups
  cumulative_cols <- col_order[str_detect(col_order, "Cumulative")]
  annual_cols     <- col_order[str_detect(col_order, "Annual")]
  
  # Sort within each group to preserve year + Total/Per pairing
  sort_within_group <- function(cols) {
    years <- str_extract(cols, "ARR\\d{2}")
    types <- str_extract(cols, "Total|Per")
    tibble(col = cols, year = years, type = types) %>%
      arrange(year, type) %>%
      pull(col)
  }
  
  # Final ordered column list
  final_order <- c("Variable",
                   sort_within_group(cumulative_cols),
                   sort_within_group(annual_cols))
  
  # Apply reordered columns to tb_wide
  tb_wide <- tb_wide %>% select(all_of(final_order))
  tb_wide %<>%   mutate(across(starts_with("Per"), ~ replace_na(.x, 0)))

  
  return(tb_wide)
}

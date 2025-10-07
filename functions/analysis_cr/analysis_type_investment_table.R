# function to create investment tables
# usage type_investment_table(df, "Reached")

type_investment_table <- function(df, status) {
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
    group_by(Exercise, Typeofinvestment) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    mutate(Attribute = "Total")
  
  # gender
  gender <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Gender, Typeofinvestment) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    rename(Attribute = Gender)
  
  # type of beneficiary
  bens <- df %>%
    filter(Typeofbeneficiary != "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofbeneficiary, Typeofinvestment) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    rename(Attribute = Typeofbeneficiary)
  
  # levelofeducation
  edu_level <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation != "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Levelofeducation, Typeofinvestment) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    rename(Attribute = Levelofeducation)
  
  # typeofeducation
  edu_type <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofeducation, Typeofinvestment) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    rename(Attribute = Typeofeducation)
  
  # join
  tb <- bind_rows(total, gender, bens, edu_level, edu_type)
  
  
  # Prep bens gender disag
  gender_bens <- df %>%
    filter(Typeofbeneficiary != "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Exercise, Typeofinvestment, Typeofbeneficiary, Gender) %>%
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
    group_by(Exercise, Typeofinvestment, Levelofeducation, Gender) %>%
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
    group_by(Exercise, Typeofinvestment, Typeofeducation, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    rename(Attribute = Typeofeducation) %>%
    pivot_wider(names_from = Gender, values_from = Total) %>%
    mutate(Total = Female + Male,
           PerFemale = Female / Total) %>%
    select(-c(Female, Male, Total)) %>%
    mutate(Exercise = paste0(Exercise, "_", "PerFemale")) %>%
    pivot_wider(names_from = Exercise, values_from = PerFemale)
  
  
  gender_disag <- bind_rows(gender_bens, gender_edu, gender_edu_type)
  
  
  gender_disag %<>%
    ungroup() %>%
    mutate(Concat = paste0(Attribute, Typeofinvestment)) %>%
    select(-c(Typeofinvestment))
  
  # Table processing ----
  # pivot wider
  tb %<>%
    pivot_wider(names_from = Attribute, values_from = Total)
  
  # Step 1: Add percentage columns
  tb %<>%
    mutate(
      `PerFemale` = Female / Total,
      `PerMale` = Male / Total,
      `PerRefugee` = Refugee / Total,
      `PerIDP` = IDP / Total,
      `PerOAPs` = OAPs / Total,
      `PerUnknown` = Unknown / Total,
      `PerPre-primary` = `Pre-primary` / Total,
      `PerPrimary` = Primary / Total,
      `PerSecondary` = Secondary / Total,
      `PerUnknown level of education` = `Unknown level of education` / Total,
      `PerChildren with disabilities` = `Children with disabilities` / Total,
      `PerFormal education` = `Formal education` / Total,
      `PerNon-formal education` = `Non-formal education` / Total
    )
  
  # Step 3: Pivot longer
  tb_long <- tb %>%
    pivot_longer(cols = -c(Exercise, Typeofinvestment), names_to = "Variable", values_to = "Value") %>%
    mutate(
      Type = if_else(str_starts(Variable, "Per"), "Per", "Total"),
      BaseVar = if_else(str_starts(Variable, "Per"), str_remove(Variable, "^Per"), Variable)
    )
  
  # Step 4: Pivot wider, keeping TypeofInvestment in rows
  tb_wide <- tb_long %>%
    pivot_wider(
      id_cols = c(BaseVar, Typeofinvestment),
      names_from = c(Exercise, Type),
      values_from = Value,
      names_sep = " "
    ) %>%
    rename(Variable = BaseVar) %>%
    mutate(Concat = paste0(Variable, Typeofinvestment))
  
  # Step 4: Join with gender disaggregation
  tb_wide %<>%
    left_join(gender_disag, by = "Concat") %>%
    select(-c(Concat, Attribute))
  
  # Step 5: Reorder columns (Cumulative before Annual, Total before Per)
  col_order <- names(tb_wide)[-c(1,2)]  # skip Variable, TypeofInvestment
  
  # Split into cumulative/annual and sort
  sort_cols <- function(cols) {
    tibble(col = cols) %>%
      mutate(
        Year = str_extract(col, "ARR\\d{2}"),
        Period = if_else(str_detect(col, "Cumulative"), "Cumulative", "Annual"),
        Type = str_extract(col, "Total|Per")
      ) %>%
      arrange(Period, Year, Type) %>%
      pull(col)
  }
  
  final_cols <- c("Variable", "Typeofinvestment", sort_cols(col_order))
  tb_wide <- tb_wide %>% select(all_of(final_cols))
  tb_wide %<>%   mutate(across(starts_with("Per"), ~ replace_na(.x, 0)))

  return(tb_wide)
}

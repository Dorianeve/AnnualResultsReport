# function to create programme tables
# usage programme_table(df, "Reached")

programme_table <- function(df, status) {
  
  # filter
  df %<>%
    filter(Status == status &
             exercice == cumulative,
           ActiveStrategicPlan == "Yes",
           (Typeofreporting != "Individual" | is.na(Typeofreporting)) &
             (Covid19relatedgrant == "No" | is.na(Covid19relatedgrant)) &
             Covid19related == "Non-Covid-19 related data")
  
  
  # totals
  total <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(ProgrammeID) %>%
    summarise(Total = sum(Number, na.rm = TRUE))
  total
  
  # gender
  gender <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(ProgrammeID, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    pivot_wider(names_from = Gender, values_from = Total)
  gender
  
  # type of beneficiary
  bens <- df %>%
    filter(Typeofbeneficiary != "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(ProgrammeID, Typeofbeneficiary) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    pivot_wider(names_from = Typeofbeneficiary, values_from = Total)
  bens
  
  # levelofeducation
  edu_level <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation != "Total" &
             Gender != "Total") %>%
    group_by(ProgrammeID, Levelofeducation) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    pivot_wider(names_from = Levelofeducation, values_from = Total)
  edu_level
  
  # typeofeducation
  edu_type <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(ProgrammeID, Typeofeducation) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    pivot_wider(names_from = Typeofeducation, values_from = Total)
  edu_type
  
  df %<>%
    select(ProgrammeID, Typeofinvestment, Country) %>%
    distinct()
  
  df %<>%
    left_join(total, by = "ProgrammeID") %>%
    left_join(gender, by = "ProgrammeID") %>%
    left_join(bens, by = "ProgrammeID") %>%
    left_join(edu_level, by = "ProgrammeID") %>%
    left_join(edu_type, by = "ProgrammeID")
  
  
  df %<>%
    mutate(
      # Gender
      PerFemale = Female / Total,
      PerMale = Male / Total,
      
      # Vulnerable groups
      PerChildrenWithDisabilities = `Children with disabilities` / Total,
      PerIDP = IDP / Total,
      PerOAPs = OAPs / Total,
      PerRefugees = Refugee / Total,
      PerUnknownBen = Unknown / Total,
      
      # Education levels
      PerPrePrimary = `Pre-primary` / Total,
      PerPrimary = Primary / Total,
      PerSecondary = Secondary / Total,
      PerUnknownLevel = `Unknown level of education` / Total,
      
      # Education types
      PerFormalEdu = `Formal education` / Total,
      PerNonFormalEdu = `Non-formal education` / Total
    )
  
  df <- df %>%
    select(
      # Keep ID columns first
      ProgrammeID, Typeofinvestment, Country, Total,
      
      # Gender
      Female, Male, PerFemale, PerMale,
      
      # Disability
      `Children with disabilities`, PerChildrenWithDisabilities,
      
      # Vulnerable groups
      IDP, PerIDP,
      OAPs, PerOAPs,
      Refugee, PerRefugees,
      Unknown, PerUnknownBen,
      
      # Education levels
      `Pre-primary`, PerPrePrimary,
      Primary, PerPrimary,
      Secondary, PerSecondary,
      `Unknown level of education`, PerUnknownLevel,
      
      # Education types
      `Formal education`, PerFormalEdu,
      `Non-formal education`, PerNonFormalEdu
    )
  
  df %<>% mutate(across(starts_with("Per"), ~ replace_na(.x, 0)))
  
  return(df)
  
}
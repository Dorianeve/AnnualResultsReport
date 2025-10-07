# function to create programme tables
# usage programme_table(df, "Reached")

country_investment_table <- function(df, status) {
  
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
    group_by(Country, Typeofinvestment) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    mutate(ID = paste0(Country, Typeofinvestment))
  total
  
  # gender
  gender <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Country, Typeofinvestment, Gender) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    pivot_wider(names_from = Gender, values_from = Total) %>%
    mutate(ID = paste0(Country, Typeofinvestment))
  gender
  
  # type of beneficiary
  bens <- df %>%
    filter(Typeofbeneficiary != "Total" & Typeofbeneficiary != "Unknown" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Country, Typeofinvestment, Typeofbeneficiary) %>%
    summarise(Total = sum(Number, na.rm = TRUE))%>%
    pivot_wider(names_from = Typeofbeneficiary, values_from = Total) %>%
    mutate(ID = paste0(Country, Typeofinvestment))
  bens
  
  # levelofeducation
  edu_level <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation != "Total" & Levelofeducation != "Unknown level of education" &
             Gender != "Total") %>%
    group_by(Country, Typeofinvestment, Levelofeducation) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    pivot_wider(names_from = Levelofeducation, values_from = Total) %>%
    mutate(ID = paste0(Country, Typeofinvestment))
  edu_level
  
  # typeofeducation
  edu_type <- df %>%
    filter(Typeofbeneficiary == "Total" &
             Levelofeducation == "Total" &
             Gender != "Total") %>%
    group_by(Country, Typeofinvestment, Typeofeducation) %>%
    summarise(Total = sum(Number, na.rm = TRUE)) %>%
    pivot_wider(names_from = Typeofeducation, values_from = Total) %>%
    mutate(ID = paste0(Country, Typeofinvestment))
  edu_type
  
  df %<>%
    select(Country, Typeofinvestment) %>%
    distinct() %>%
    mutate(ID = paste0(Country, Typeofinvestment)) %>%
    select(ID)
  
  df %<>%
    left_join(total, by = "ID") %>%
    left_join(gender, by = "ID") %>%
    left_join(bens, by = "ID") %>%
    left_join(edu_level, by = "ID") %>%
    left_join(edu_type, by = "ID") %>%
    select(-c(ID))
  
  
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
      
      # Education levels
      PerPrePrimary = `Pre-primary` / Total,
      PerPrimary = Primary / Total,
      PerSecondary = Secondary / Total,
      
      # Education types
      PerFormalEdu = `Formal education` / Total,
      PerNonFormalEdu = `Non-formal education` / Total
    )
  
  df <- df %>%
    select(
      # Keep Country columns first
      Country, Typeofinvestment, Total,
      
      # Gender
      Female, Male, PerFemale, PerMale,
      
      # Disability
      `Children with disabilities`, PerChildrenWithDisabilities,
      
      # Vulnerable groups
      IDP, PerIDP,
      OAPs, PerOAPs,
      Refugee, PerRefugees,
      
      # Education levels
      `Pre-primary`, PerPrePrimary,
      Primary, PerPrimary,
      Secondary, PerSecondary,
      
      # Education types
      `Formal education`, PerFormalEdu,
      `Non-formal education`, PerNonFormalEdu
    )
  
  df %<>% mutate(across(starts_with("Per"), ~ replace_na(.x, 0)))
  
  return(df)
}
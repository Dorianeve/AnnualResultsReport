# function for CR Step3 SPOT CHECKS TEMPLATE / COMBINER 

cr_random_checker <- function(beneficiary, gender, education_level, gmgrn) {
  df %>%
    filter(Typeofeducation == "Formal education" &
             Typeofbeneficiary == beneficiary &
             Gender == gender &
             Levelofeducation == education_level &
             GMGRN == gmgrn) %>%
    select(GMGRN, Typeofeducation, Typeofbeneficiary, Levelofeducation, Gender, Number)
}

# original stata code
# *21-ECW-FER-13-CMR-UHC 20-ECW-MYR-BI-10-TCD-UNI 21-ECW-FER-09-MOZ-PLA 21-ECW-MYR-18-IRQ-STC 
# di "21-ECW-FER-13-CMR-UHC"
# list  Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="Refugee" & Gender=="Female" & Levelofeducation=="Primary" & GMGRN=="21-ECW-FER-13-CMR-UHC"
# list Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="OAPs" & Gender=="Male" & Levelofeducation=="Secondary" & GMGRN=="21-ECW-FER-13-CMR-UHC"
# *ok
# di "20-ECW-MYR-BI-10-TCD-UNI"
# list  Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="Refugee" & Gender=="Female" & Levelofeducation=="Primary" & GMGRN=="20-ECW-MYR-BI-10-TCD-UNI"
# list Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="OAPs" & Gender=="Male" & Levelofeducation=="Secondary" & GMGRN=="20-ECW-MYR-BI-10-TCD-UNI"
# *ok
# di "21-ECW-FER-09-MOZ-PLA"
# list  Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="Refugee" & Gender=="Female" & Levelofeducation=="Primary" & GMGRN=="21-ECW-FER-09-MOZ-PLA"
# list Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="OAPs" & Gender=="Male" & Levelofeducation=="Secondary" & GMGRN=="21-ECW-FER-09-MOZ-PLA"
# *ok
# di "21-ECW-MYR-18-IRQ-STC"
# list  Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="Refugee" & Gender=="Female" & Levelofeducation=="Primary" & GMGRN=="21-ECW-MYR-18-IRQ-STC"
# list Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="OAPs" & Gender=="Male" & Levelofeducation=="Secondary" & GMGRN=="21-ECW-MYR-18-IRQ-STC"
# *ok
# 
# di "19-ECW-MYR-09-SOM-ADR"
# list  Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="Refugee" & Gender=="Female" & Levelofeducation=="Primary" & GMGRN=="19-ECW-MYR-09-SOM-ADR"
# list Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="OAPs" & Gender=="Male" & Levelofeducation=="Secondary" & GMGRN=="19-ECW-MYR-09-SOM-ADR"
# 
# di "22-ECW-FER-04-SDN-UHC"
# list  Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="Refugee" & Gender=="Female" & Levelofeducation=="Primary" & GMGRN=="22-ECW-FER-04-SDN-UHC"
# list Typeofeducation Typeofbeneficiary Levelofeducation Gender Number if Typeofeducation=="Formal education" & Typeofbeneficiary=="OAPs" & Gender=="Male" & Levelofeducation=="Secondary" & GMGRN=="22-ECW-FER-04-SDN-UHC"
# /*level of educ is total or unknown only, and only refugees or total because the joint tab was not extracted*/
#   
#   
#   * Note: the code above could be simplified/improved for ARR23
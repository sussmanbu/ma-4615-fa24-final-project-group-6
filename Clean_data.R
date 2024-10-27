library(tidyverse)
library(haven)


#cleaning data

#removing rows with missing values

library(dplyr)


#filtered all incomplete interviews
data2 <- read_xpt("dataset-ignore/LLCP2023.XPT")|>
  filter(DISPCODE == 1100)
  


# cleaned general health condition
data2_clean <- data2|>
    mutate(Health_status = recode(as.factor(GENHLTH), `1` = "Excellent",
                                  `2` = "Very Good",
                                  `3` = "Good",
                                  `4` = "Fair",
                                  `5` = "Poor",
                                  `7` = "Don’t Know/Not Sure",
                                  `9` = "Refused"))|>

# cleaned race variable
<<<<<<< HEAD
data2_clean <- data2|>
  mutate(`_RACE` = case_when(
    `_RACE` == 1 ~ "White",
    `_RACE` == 2 ~ "Black", 
    `_RACE` == 3 ~ "American Indian or Alaskan Native", 
    `_RACE` == 4 ~ "Asian",
    `_RACE` == 5 ~ "Native Hawaiian or Other Pacific Islander",
    `_RACE` == 6 ~ "Other", `_RACE` == 7 ~ "Multiracial",
    `_RACE` == 8 ~ "Hispanic", `_RACE` == 9 ~ "Uncertain/Refused"))|>
  rename(RACE = `_RACE`)
=======

  mutate(`_RACE` = case_when(`_RACE` == 1 ~ "White",
                              `_RACE` == 2 ~ "Black", 
                             `_RACE` == 3 ~ "American Indian or Alaskan Native", 
         `_RACE` == 4 ~ "Asian",
         `_RACE` == 5 ~ "Native Hawaiian or Other Pacific Islander",
         `_RACE` == 6 ~ "Other", `_RACE` == 7 ~ "Multiracial",
         `_RACE` == 8 ~ "Hispanic", `_RACE` == 9 ~ "Uncertain/Refused"))|>
  rename(RACE = `_RACE`)|>
>>>>>>> 0a3648f0e1bdedfd1211ee50280e2a7e6cbc20f7

# cleaned lonely variable
data2_clean <- data2|>  
  mutate(`SDLONELY` = case_when(
    `SDLONELY` == 1 ~ "Always",
    `SDLONELY` == 2 ~ "Usually", 
    `SDLONELY` == 3 ~ "Sometimes", 
    `SDLONELY` == 4 ~ "Rarely",
    `SDLONELY` == 5 ~ "Never",
    `SDLONELY` == 7 ~ "Don’t know/Not sure", 
    `SDLONELY` == 9 ~ "Refused"))|>
  rename(loneliness_feeling_frequency = `SDLONELY`)


# cleaned stressed variable
data2_clean <- data2|>  
  mutate(`SDHSTRE1` = case_when(
    `SDHSTRE1` == 1 ~ "Always",
    `SDHSTRE1` == 2 ~ "Usually", 
    `SDHSTRE1` == 3 ~ "Sometimes", 
    `SDHSTRE1` == 4 ~ "Rarely",
    `SDHSTRE1` == 5 ~ "Never",
    `SDHSTRE1` == 7 ~ "Don’t know/Not sure", 
    `SDHSTRE1` == 9 ~ "Refused"))|>
  rename(stress_feeling_frequency = `SDHSTRE1`)


# cleaned stressed variable
data2_clean <- data2|>  
  mutate(`SDHSTRE1` = case_when(
    `SDHSTRE1` == 1 ~ "Always",
    `SDHSTRE1` == 2 ~ "Usually", 
    `SDHSTRE1` == 3 ~ "Sometimes", 
    `SDHSTRE1` == 4 ~ "Rarely",
    `SDHSTRE1` == 5 ~ "Never",
    `SDHSTRE1` == 7 ~ "Don’t know/Not sure", 
    `SDHSTRE1` == 9 ~ "Refused"))|>
  rename(stress_feeling_frequency = `SDHSTRE1`)


# cleaned satisfaction variable
data2_clean <- data2|>  
  mutate(`EMTSUPRT` = case_when(
    `EMTSUPRT` == 1 ~ "Always",
    `EMTSUPRT` == 2 ~ "Usually", 
    `EMTSUPRT` == 3 ~ "Sometimes", 
    `EMTSUPRT` == 4 ~ "Rarely",
    `EMTSUPRT` == 5 ~ "Never",
    `EMTSUPRT` == 7 ~ "Don’t know/Not sure", 
    `EMTSUPRT` == 9 ~ "Refused"))|>
  rename(emotional_support = `EMTSUPRT`)


#cleaned education variable
  mutate(EDUCA = case_when (
    EDUCA == 1 ~ "None/Kindergarten",
    EDUCA == 2 ~ "Elementary",
    EDUCA == 3 ~ "Some High School",
    EDUCA == 4 ~ "High School Graduate",
    EDUCA == 5 ~ "Some College",
    EDUCA == 6 ~ "College Graduate",
    EDUCA == 9 ~ "Refused",
  ))|>



#cleaned insurance status variable
  mutate(
    PRIMINS1 = case_when(
      PRIMINS1 == 1 ~ "Employer-based Insurance",
      PRIMINS1 %in% c(3, 4, 5, 6, 7, 8, 9, 10) ~ "Government Insurance", 
      PRIMINS1 == 2 ~ "Private Insurance", 
      PRIMINS1 == 88 ~ "No Insurance", 
      PRIMINS1 %in% c(77, 99) ~ "Uncertain/Refused", 
      TRUE ~ "Other"
    )
  )|>
  rename(insurace_status = PRIMINS1)|>

# cleaned frequency of exercise per month
  mutate(EXEROFT1 = case_when(EXEROFT1 %in% c(77,99) ~ NA,
                              EXEROFT1 >= 101 & EXEROFT1 <= 199 ~ (EXEROFT1 - 100)*4.33, 
                              EXEROFT1 >= 201 & EXEROFT1 <= 299 ~ EXEROFT1 - 200),
         EXEROFT2 = case_when(EXEROFT2 == 77 ~ 0,
                              EXEROFT2 == 99 ~ 0,
                              EXEROFT2 >= 101 & EXEROFT2 <= 199 ~ (EXEROFT2 - 100)*4.33, 
                              EXEROFT2 >= 201 & EXEROFT2 <= 299 ~ EXEROFT2 - 200),
         Exercise_frequency = EXEROFT1 + EXEROFT2)|>
  select(-EXEROFT1, -EXEROFT2)|>

# Cleaning the BMI categories
  mutate(BMI_category = recode(as.factor(`_BMI5CAT`),
                               `1` = "Underweight",
                               `2` = "Normal Weight",
                               `3` = "Overweight",
                               `4` = "Obese",
                               `9999` = NA_character_))


#Cleaning States
# Recode '_STATE' values into state names

data2_clean <- data2_clean |>
  mutate(State = case_when(
    _STATE == 1 ~ "Alabama", 
    _STATE == 2 ~ "Alaska", 
    _STATE == 4 ~ "Arizona", 
    _STATE == 5 ~ "Arkansas", 
    _STATE == 6 ~ "California", 
    _STATE == 8 ~ "Colorado", 
    _STATE == 9 ~ "Connecticut", 
    _STATE == 10 ~ "Delaware", 
    _STATE == 11 ~ "District of Columbia", 
    _STATE == 12 ~ "Florida", 
    _STATE == 13 ~ "Georgia", 
    _STATE == 15 ~ "Hawaii", 
    _STATE == 16 ~ "Idaho", 
    _STATE == 17 ~ "Illinois", 
    _STATE == 18 ~ "Indiana", 
    _STATE == 19 ~ "Iowa", 
    _STATE == 20 ~ "Kansas", 
    _STATE == 22 ~ "Louisiana", 
    _STATE == 23 ~ "Maine", 
    _STATE == 24 ~ "Maryland", 
    _STATE == 25 ~ "Massachusetts", 
    _STATE == 26 ~ "Michigan", 
    _STATE == 27 ~ "Minnesota", 
    _STATE == 28 ~ "Mississippi", 
    _STATE == 29 ~ "Missouri", 
    _STATE == 30 ~ "Montana", 
    _STATE == 31 ~ "Nebraska", 
    _STATE == 32 ~ "Nevada", 
    _STATE == 33 ~ "New Hampshire", 
    _STATE == 34 ~ "New Jersey", 
    _STATE == 35 ~ "New Mexico", 
    _STATE == 36 ~ "New York", 
    _STATE == 37 ~ "North Carolina", 
    _STATE == 38 ~ "North Dakota", 
    _STATE == 39 ~ "Ohio", 
    _STATE == 40 ~ "Oklahoma", 
    _STATE == 41 ~ "Oregon", 
    _STATE == 44 ~ "Rhode Island", 
    _STATE == 45 ~ "South Carolina", 
    _STATE == 46 ~ "South Dakota", 
    _STATE == 47 ~ "Tennessee", 
    _STATE == 48 ~ "Texas", 
    _STATE == 49 ~ "Utah", 
    _STATE == 50 ~ "Vermont", 
    _STATE == 51 ~ "Virginia", 
    _STATE == 53 ~ "Washington", 
    _STATE == 54 ~ "West Virginia", 
    _STATE == 55 ~ "Wisconsin", 
    _STATE == 56 ~ "Wyoming", 
    _STATE == 66 ~ "Guam", 
    _STATE == 72 ~ "Puerto Rico", 
    _STATE == 78 ~ "Virgin Islands"
  ))



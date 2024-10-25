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

  mutate(`_RACE` = case_when(`_RACE` == 1 ~ "White",
                              `_RACE` == 2 ~ "Black", 
                             `_RACE` == 3 ~ "American Indian or Alaskan Native", 
         `_RACE` == 4 ~ "Asian",
         `_RACE` == 5 ~ "Native Hawaiian or Other Pacific Islander",
         `_RACE` == 6 ~ "Other", `_RACE` == 7 ~ "Multiracial",
         `_RACE` == 8 ~ "Hispanic", `_RACE` == 9 ~ "Uncertain/Refused"))|>
  rename(RACE = `_RACE`)|>


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
  select(-EXEROFT1, -EXEROFT2)

# Cleaning the BMI categories
data2_clean <- data2_clean |> 
  mutate(BMI_category = recode(as.factor(_BMI5CAT),
                               `1` = "Underweight",
                               `2` = "Normal Weight",
                               `3` = "Overweight",
                               `4` = "Obese",
                               `9999` = NA_character_))


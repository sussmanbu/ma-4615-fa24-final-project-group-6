library(tidyverse)
library(haven)
<<<<<<< HEAD
data2 <- read_xpt("LLCP2023.XPT")

#cleaning data

#removing rows with missing values


=======
library(dplyr)
#HEAD
data2 <- read_xpt("dataset-ignore/LLCP2023.XPT")

colnames(data2)

if ("GENHLTH" %in% colnames(data2)) {
  
  colnames(data2)[colnames(data2) == "GENHLTH"] <- "Health Status"
  
  data2$`Health Status` <- as.factor(data2$`Health Status`)
  
  data2$`Health Status` <- recode(data2$`Health Status`, 
                                  `1` = "Excellent",
                                  `2` = "Very Good",
                                  `3` = "Good",
                                  `4` = "Fair",
                                  `5` = "Poor",
                                  `7` = "Don’t Know/Not Sure",
                                  `9` = "Refused")
  
  head(data2)
  
} else {
  print("GENHLTH column not found in the dataset.")
}


# cleaned race variable
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
data2_clean <- data2 |>
  mutate(EDUCA = case_when (
    EDUCA == 1 ~ "None/Kindergarten",
    EDUCA == 2 ~ "Elementary",
    EDUCA == 3 ~ "Some High School",
    EDUCA == 4 ~ "High School Graduate",
    EDUCA == 5 ~ "Some College",
    EDUCA == 6 ~ "College Graduate",
    EDUCA == 9 ~ "Refused",
  ))



#cleaned insurance status variable
data2_clean <- data2 |>
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
  rename(insurace_status = PRIMINS1)

>>>>>>> cf5e5e124ccc5f6851e9b78d6a379769e09f9356

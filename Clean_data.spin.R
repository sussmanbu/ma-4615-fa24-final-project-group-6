library(tidyverse)
library(haven)

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
  mutate(`_RACE` = case_when(`_RACE` == 1 ~ "White",
                              `_RACE` == 2 ~ "Black", 
                             `_RACE` == 3 ~ "American Indian or Alaskan Native", 
         `_RACE` == 4 ~ "Asian",
         `_RACE` == 5 ~ "Native Hawaiian or Other Pacific Islander",
         `_RACE` == 6 ~ "Other", `_RACE` == 7 ~ "Multiracial",
         `_RACE` == 8 ~ "Hispanic", `_RACE` == 9 ~ "Uncertain/Refused"))|>
  rename(RACE = `_RACE`)


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


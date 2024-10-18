
library(tidyverse)
library(haven)
data2 <- read_xpt("dataset-ignore/LLCP2023.XPT")

data2 <- data2|>
  mutate(`_RACE` = case_when(`_RACE` == 1 ~ "White",
                              `_RACE` == 2 ~ "Black", `_RACE` == 3 ~ "American Indian or Alaskan Native", 
         `_RACE` == 4 ~ "Asian", `_RACE` == 5 ~ "Native Hawaiian or Other Pacific Islander",
         `_RACE` == 6 ~ "Other", `_RACE` == 7 ~ "Multiracial",
         `_RACE` == 8 ~ "Hispanic", `_RACE` == 9 ~ "Uncertain/Refused"))|>
  rename(RACE = `_RACE`)

print(data2$`RACE`)
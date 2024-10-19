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


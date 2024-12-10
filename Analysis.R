library(tidyverse)
library(haven)
library(ggplot2)
library(readxl)
library(tidycensus)
library(sf) 
library(viridis)
library(dplyr)

#cleaning data

#removing rows with missing values


source(
  "scripts/load_and_clean_data.R",
  echo = FALSE 
)

#new version for the medical cost, race, health status, glm
ht_mc<-brfss_clean|>
  drop_na(medical_cost, Health_status, `_INCOMG1`)|>
  filter(`_INCOMG1` != 9 & `medical_cost` != "Don’t know/Not sure" & medical_cost != "Refused")|>
  mutate(
    medical_cost = recode(as.character(medical_cost), 'Yes' = 1, 'No' = 0), 
    Health_status = recode(as.character(Health_status),  "Excellent"= 5,
                           "Very Good"=4,
                           "Good"=3,
                           "Fair"=2,
                           "Poor"= 1,
                           "Don’t Know/Not Sure"=7,
                           "Refused"=9)
  )


ht_mc_model <- glm(medical_cost ~ Health_status, data = ht_mc, family = "binomial")
summary(ht_mc_model)

predicted_prob <- predict(ht_mc_model, type = "response")
ht_mc$Y_hat <- predicted_prob

ggplot(ht_mc, aes(x = Health_status, y = Y_hat )) +
  geom_point( alpha = 0.6,color = "blue") + 
  geom_line(color = "darkred") +  
  labs(title = "Predicted Probability of Unaffordability of Medical Cost by Health Status",
       x = "Health Status",
       y = "Predicted Probability of 'No' for Medical Cost") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = "none")

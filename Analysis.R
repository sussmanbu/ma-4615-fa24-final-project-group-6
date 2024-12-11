library(tidyverse)
library(haven)
library(ggplot2)
library(readxl)
library(tidycensus)
library(sf) 
library(viridis)
library(dplyr)
library(corrplot)
#cleaning data

#removing rows with missing values


source(
  "scripts/load_and_clean_data.R",
  echo = FALSE 
)

#new version for the medical cost, race, health status, glm
ht_mc<-brfss_clean|>
  drop_na(`medical_cost`, Health_status, insurance_status)|>
  filter(`_INCOMG1` != 9 & `medical_cost` != "Don’t know/Not sure" & `medical_cost` != "Refused"
         & insurance_status != "Uncertain/Refused")|>
  mutate(
    medical_cost = recode(as.character(medical_cost), 'Yes' = 0, 'No' = 1), 
    Health_status = recode(as.character(Health_status),  "Excellent"= 5,
                           "Very Good"=4,
                           "Good"=3,
                           "Fair"=2,
                           "Poor"= 1,
                           "Don’t Know/Not Sure"=7,
                           "Refused"=9),
    insurance_status = case_when(
      insurance_status %in% c("Employer-based Insurance", "Government Insurance", "Private Insurance") ~ 1,
      insurance_status == "No Insurance" ~ 0,
    ))


ht_mc_model <- glm(medical_cost ~ Health_status, data = ht_mc, family = "binomial")
summary(ht_mc_model)


predicted_prob <- predict(ht_mc_model, type = "response")
ht_mc$Y_hat <- predicted_prob

ggplot(ht_mc, aes(x = Health_status, y = Y_hat )) +
  geom_point( alpha = 0.6,color = "blue") + 
  geom_line(color = "darkred") +  
  labs(title = "Predicted Probability of Affordability of Medical Cost by Health Status",
       x = "Health Status",
       y = "Predicted Probability of Affordability for Medical Cost") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = "none")

htmcis <- ht_mc %>% select(`medical_cost`, Health_status, insurance_status)
cor_matrix <- cor(htmcis,method = "pearson")
print(cor_matrix)
corrplot(cor_matrix, method = "circle")


# relationship between education and loneliness, faceted by race
summary <- brfss_clean|>
  filter(loneliness_feeling_frequency != "Don’t know/Not sure" & loneliness_feeling_frequency != "Refused" 
         & !is.na(RACE) & RACE != "Uncertain/Refused" & !is.na(EDUCA) & EDUCA != "Refused")|>
  group_by(EDUCA, RACE)|>
  summarize(`Percentage of Population Feeling Lonely` = mean(loneliness_feeling_frequency == "Usually" | loneliness_feeling_frequency == "Always", na.rm = TRUE))

summary$EDUCA_factor <- factor(summary$EDUCA,levels = c("None/Kindergarten", "Elementary", "Some High School", 
                                                        "High School Graduate","Some College", "College Graduate"))

ggplot(summary, aes(x = reorder(EDUCA_factor, desc(EDUCA_factor)), 
                    y = `Percentage of Population Feeling Lonely`, fill = EDUCA_factor)) +
  geom_col() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~RACE)+
  coord_flip()+
  theme(             
    axis.text.y = element_blank(),                        
    axis.title.y = element_blank()
  )+
  labs(fill = "Education level", title = "Education Level vs. Loneliness by Race", x = "Loneliness")+
  scale_fill_viridis_d()+
  theme(strip.text = element_text(size = 8))

brfss_clean|>
  filter(!is.na(EDUCA) & EDUCA != "Refused" & `_INCOMG1` != 9)|>
  group_by(EDUCA)|>
  summarize(higher_income = mean(`_INCOMG1` == 6 |`_INCOMG1` == 7))

cor_income_satisfaction <- brfss_clean|>
  filter(LSATISFY %in% c(1, 2, 3, 4), `_INCOMG1` != 9)
cor(cor_income_satisfaction$LSATISFY,cor_income_satisfaction$`_INCOMG1`, use = "complete.obs")

# loneliness and state income on mental health
loneliness <- merged_data|>
  filter(loneliness_feeling_frequency == "Always" | loneliness_feeling_frequency == "Usually" |
           loneliness_feeling_frequency == "Rarely" | loneliness_feeling_frequency == "Never", !is.na(MENTHLTH))|>
  mutate(Binary_lonely = if_else(loneliness_feeling_frequency %in% c("Always", "Usually"), 1, 0))|>
  select(`Gross domestic product (GDP)`, AGE_GROUP, Binary_lonely, EDUCA, `Personal income`, MENTHLTH, State)|>
  rename(GDP = `Gross domestic product (GDP)`)

poisson_model <- glm(MENTHLTH ~ log(GDP) + Binary_lonely, 
                     family = quasipoisson, data = loneliness)
summary(poisson_model)
(exp(coefficients(poisson_model))-1)*100




#logistic regression on health status vs alcohol drinks per day and cholesterol status
brfss_regre <- brfss_clean|>
  filter(!(Health_status %in% c("Fair", "Don’t Know/Not Sure", "Refused")))|>
  filter((TOLDHI3 == 1| TOLDHI3 ==2), !(is.na(Alcohol_Drinks_Per_Day)))|>
  mutate(Binary_health = if_else(Health_status %in% c("Excellent", "Very Good", "Good"), 0, 1))|>
  mutate(high_cholesterol = if_else(TOLDHI3 == 1, 1, 0))|>
  select(Binary_health, Alcohol_Drinks_Per_Day, high_cholesterol)


binary <- glm(Binary_health~ Alcohol_Drinks_Per_Day + 
                high_cholesterol, data = brfss_regre, family= binomial)
summary(binary)
(exp(coefficients(binary))-1)*100

brfss_regre$predicted <- predict(binary, type = "response")
ggplot(brfss_regre, aes(x = Alcohol_Drinks_Per_Day, y = predicted, color = as_factor(high_cholesterol))) +
  geom_line() +
  labs(title = "Probability of Bad Health Based on Alcohol Drinks Per Day",
       x = "Alcohol Drinks Per Day",
       y = "Predicted Probability of Poor Health",
       color = "Health Status") +
  scale_y_continuous(labels = scales::percent)+
  scale_color_manual(values = c("red", "blue"), labels = c("normal cholesterol", "high cholesterol"))+
  theme_minimal()



# relationship between adverse childhod experiences


mari_alch_combined <- mari_alch |>
  pivot_longer(cols = c(ACEDEPRS, ACEDRINK, ACEDRUGS), 
               names_to = "ExposureType", 
               values_to = "ExposureLevel"
  ) |>
  pivot_longer(cols = c(MARIJAN1), 
               names_to = "Substance", 
               values_to = "Frequency") |>
  filter(!is.na(Frequency), !is.na(ExposureLevel))

mari_alch_means <- mari_alch_combined %>%
  group_by(ExposureType, ExposureLevel) %>%
  summarize(MeanFrequency = mean(Frequency, na.rm = TRUE), .groups = "drop")

ggplot(mari_alch_combined, aes(x = ExposureLevel, y = Frequency, fill = ExposureType)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ ExposureType, scales = "free") +
  labs(
    title = "Distribution of Marijuana Usage Frequency by Exposure and Type",
    x = "Exposure Level",
    y = "Frequency",
    fill = "Exposure Type"
  ) +
  theme_minimal() 

#racial disparities across health
race_health <- brfss_clean|>
  filter(!is.na(MENTHLTH), !is.na(PHYSHLTH),!is.na(RACE) & RACE != "Uncertain/Refused" )|>
  group_by(RACE)|>
  summarize(mental_avg = mean(MENTHLTH), physical_avg = mean(PHYSHLTH))

ggplot(data = race_health, aes(x = reorder(RACE, -mental_avg), y = mental_avg, fill = reorder(RACE, -mental_avg)))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")+
  scale_fill_viridis_d()+
  labs(x = "Race", y = "Days of Mental Health Unwell", title = "Racial Disparities in Mental Health")
ggplot(data = race_health, aes(x = reorder(RACE, -physical_avg), y = physical_avg, fill = reorder(RACE, -physical_avg)))+
  geom_col()+
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "none")+
  scale_fill_viridis_d()+
  labs(x = "Race", y = "Days of Physical Health Unwell", title = "Racial Disparities in Mental Health") 
  



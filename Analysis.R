library(tidyverse)
library(haven)
library(ggplot2)
library(readxl)
library(tidycensus)
library(sf) 
library(viridis)
library(dplyr)
library(tidyr)
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
(exp(coefficients(ht_mc_model))-1)*100


predicted_prob <- predict(ht_mc_model, type = "response")
ht_mc$Y_hat <- predicted_prob

ggplot(ht_mc, aes(x = Health_status, y = Y_hat )) +
  geom_point( alpha = 0.6,color = "blue") + 
  geom_line(color = "darkred") +  
  labs(title = "Affordability of Medical Cost by Health Status",
       x = "Health Status",
       y = "Predicted Probability of Affordability for Medical Cost") +
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = "none")

htmcis <- ht_mc %>% select(`medical_cost`, Health_status, insurance_status)
cor_matrix <- cor(htmcis,method = "pearson")
print(cor_matrix)
library(corrplot)
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
  scale_fill_viridis_d() +
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


# Minority-to-white ratio & Income & average days mental health unwell
min_ratio <- merged_data |> 
  group_by(State, RACE) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(State) |> 
  mutate(total_population = sum(count)) |> 
  ungroup() |> 
  pivot_wider(names_from = RACE, values_from = count, values_fill = 0) |> 
  mutate(minority_to_white_ratio = (total_population - White) / White)

merged_data <- merged_data |> 
  left_join(min_ratio |> select(State, minority_to_white_ratio), by = "State")

avg_ment_unwell_data <- merged_data |>
  group_by(State) |>
  summarise(
    avg_personal_income = mean(`Personal income`, na.rm = TRUE),
    minority_to_white_ratio = mean(minority_to_white_ratio, na.rm = TRUE),
    avg_ment_unwell_days = mean(MENTHLTH, na.rm = TRUE),
    .groups = "drop"
  ) |>
  drop_na(avg_personal_income, minority_to_white_ratio, avg_ment_unwell_days)

# Linear model and correlation
income_model <- lm(`Personal income` ~ minority_to_white_ratio  + MENTHLTH, data = merged_data)
summary(income_model)
cor(merged_data$`Personal income`, merged_data$minority_to_white_ratio)

# Visualization
ggplot(avg_ment_unwell_data, aes(x = minority_to_white_ratio, y = avg_personal_income, color = avg_ment_unwell_days)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  scale_color_viridis() +
  theme_minimal() +
  labs(
    title = "Diversity Ratio and Days Mental Health Unwell on Income Per State",
    x = "Diversity Ratio",
    y = "Average Personal Income",
    color = "Days Mental Health Unwell "
  )


#logistic regression on health status vs marijuana consumption and inability to pay bills
brfss_regre <- brfss_clean|>
  filter(!(Health_status %in% c("Fair", "Don’t Know/Not Sure", "Refused")))|>
  filter(!(MARIJAN1 %in% c(77 , 88, 99)& !is.na(MARIJAN1)) )|>
  filter(SDHBILLS == 1 | SDHBILLS == 2)|>
  mutate(Binary_health = if_else(Health_status %in% c("Excellent", "Very Good", "Good"), 0, 1))|>
  mutate(cant_pay = if_else(SDHBILLS == 1, 1, 0))|>
  select(Binary_health, Alcohol_Drinks_Per_Day, cant_pay, LANDSEX2 ,MARIJAN1)


binary <- glm(Binary_health~ MARIJAN1 + 
                cant_pay, data = brfss_regre, family= binomial)
summary(binary)

(exp(coefficients(binary))-1)*100




# relationship between adverse childhod experiences

mari_alch <- brfss_clean %>%
  rename(
    Depression = ACEDEPRS,
    Alcohol = ACEDRINK,
    Drug = ACEDRUGS
  ) %>%
  select(Depression, Alcohol, Drug, MARIJAN1) %>%
  filter(!Depression %in% c(7, 9)) %>%
  filter(!Alcohol %in% c(7, 9)) %>%
  filter(!Drug %in% c(7, 9)) %>%
  filter(!MARIJAN1 %in% c(88, 77, 99)) %>%
  mutate(
    Depression = ifelse(Depression == 1, "Yes", "No"),
    Alcohol = ifelse(Alcohol == 1, "Yes", "No"),
    Drug = ifelse(Drug == 1, "Yes", "No")
  )


mari_alch_combined <- mari_alch %>%
  pivot_longer(
    cols = c(Depression, Alcohol, Drug),
    names_to = "ExposureType",
    values_to = "ExposureLevel"
  ) %>%
  pivot_longer(
    cols = c(MARIJAN1),
    names_to = "Substance",
    values_to = "Frequency"
  ) %>%
  filter(!is.na(Frequency), !is.na(ExposureLevel))

# Perform ANOVA
anova_results <- aov(Frequency ~ ExposureType * ExposureLevel, data = mari_alch_combined)
summary(anova_results)

mean_values <- mari_alch_combined %>%
  group_by(ExposureType, ExposureLevel) %>%
  summarise(mean_frequency = mean(Frequency, na.rm = TRUE))

ggplot(mari_alch_combined, aes(x = ExposureLevel, y = Frequency, fill = ExposureType)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(data = mean_values, aes(x = ExposureLevel, y = mean_frequency), color = "red", size = 3, shape = 18) +
  facet_wrap(~ ExposureType, scales = "free") +
  labs(
    title = "Marijuana Usage Frequency by Exposure and Type",
    x = "Exposure Level",
    y = "Frequency",
    fill = "Exposure Type"
  ) +
  scale_fill_viridis_d() +
  theme_minimal()



#  physical and mental health accross race

racial_health <- merged_data %>%
  filter(!PHYSHLTH %in% c(88, 77, 99)) %>%
  filter(!MENTHLTH %in% c(88, 77, 99)) %>%
  filter(!POORHLTH %in% c(88, 77, 99)) %>%
  filter(!RACE %in% c("Other", "Uncertain/Refused", NA)) %>%
  group_by(RACE) %>%
  summarise(
    `Physical Health` = mean(PHYSHLTH, na.rm = TRUE),
    `Mental Health` = mean(MENTHLTH, na.rm = TRUE),
    `Poor Health` = mean(POORHLTH, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(`Physical Health`, `Mental Health`, `Poor Health`), names_to = "status", values_to = "days_unwell")

ggplot(racial_health, aes(x = status, y = days_unwell, fill = status)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ RACE) +
  labs(title = "Average Days Unwell by Health Status for Each Race", x = "Health Status", y = "Average Days Unwell") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
  labs(x = "Race", y = "Days of Physical Health Unwell", title = "Racial Disparities in Physical Health") 
  



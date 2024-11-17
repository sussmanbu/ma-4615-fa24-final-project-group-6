library(tidyverse)
library(haven)
library(ggplot2)
library(readxl)

#cleaning data

#removing rows with missing values

library(dplyr)


source(
  "scripts/load_and_clean_data.R",
  echo = FALSE 
)

# Turley playing
anova_data <- brfss_clean %>%
  mutate(Health_status = as.factor(Health_status),
         stress_feeling_frequency = as.factor(stress_feeling_frequency),
         emotional_support = as.factor(emotional_support),
         PHYSHLTH = as.numeric(PHYSHLTH))

interaction <- aov(PHYSHLTH ~ Health_status * stress_feeling_frequency + emotional_support, data = anova_data)

summary(interaction)

library(ggplot2) 

Phys_Health <- ggplot(anova_data, aes(x = Health_status, y = PHYSHLTH, fill = Health_status)) + 
  theme_bw() +  
  geom_boxplot(width=0.1) +
  scale_fill_brewer(palette = "Accent") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), aspect.ratio = 1) + 
  labs(x = "Health Status",
       y = "Days of Physical Health Unwell Last Month")

# Print the plot
print(Phys_Health)



# relationship between exercise and physical health 
Phys_health_exercise <- ggplot(brfss_clean, aes(x = PHYSHLTH, y = Exercise_frequency)) +
  theme_bw() +
  geom_point()

print(Phys_health_exercise)


# relationship between loneliness feeling frequency and mental health
ment_health_loneliness <- ggplot(brfss_clean, aes(x = factor(loneliness_feeling_frequency, 
                                                             levels = c("Always", "Usually", "Sometimes", "Rarely", "Never")),
                                                  y = MENTHLTH)) +
  theme_bw() +
  geom_col() + 
  labs(x = "Frequency of loneliness feeling",
       y = "Days in past month of unwell mental health")

print(ment_health_loneliness)




brfss_clean|>
  ggplot(aes(as.factor(Health_status)))+
  geom_histogram(stat = "count")

brfss_clean|>
  ggplot(aes(PHYSHLTH))+
  geom_histogram(stat = "count")



#logistic regression on health status vs alcohol drinks per day and heart attack
brfss_regre <- brfss_clean|>
  filter(!(Health_status == "Fair"))|>
  mutate(Binary_health = if_else(Health_status %in% c("Excellent", "Very Good", "Good"), 1, 0))|>
  filter((CVDINFR4 == 1| CVDINFR4 ==2))|>
  mutate(heart_attack = if_else(CVDINFR4 == 1, 1, 0))
  
binary <- glm(brfss_regre$Binary_health~ brfss_regre$Alcohol_Drinks_Per_Day + 
                 brfss_regre$heart_attack)

summary(binary)


# plotting relationships between substance use, adverse childhood experiences, and mental health (can plot all three tbd)

# relationships between exsposure to drug-use at home and current drug use frequency

adverse_drug <- brfss_clean %>%
  pivot_longer(cols = c(ACEDEPRS, ACEDRINK, ACEDRUGS), 
               names_to = "ExposureType", 
               values_to = "ExposureLevel") %>%
  pivot_longer(cols = c(ECIGNOW2, SMOKDAY2), 
               names_to = "CurrentUseType", 
               values_to = "CurrentUseFrequency")
ggplot(adverse_drug, aes(x = ExposureLevel, y = CurrentUseFrequency)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(CurrentUseType ~ ExposureType, scales = "free") + # create facets for current use types and exposure types
  labs(
    title = "Relationships Between Early Exposure to Drug Use and Current Drug Use",
    x = "Exposure to Drugs at Home",
    y = "Current Drug Use Frequency"
  ) +
  theme_minimal()


# statistical analysis method


## marijuana and drinking frequency w adverse childhood
mari_alch <- brfss_clean |>
select(ACEDEPRS, ACEDRINK, ACEDRUGS, AVEDRNK3, MARIJAN1)
mari_alch_combined <- mari_alch |>
  pivot_longer(cols = c(ACEDEPRS, ACEDRINK, ACEDRUGS), 
               names_to = "ExposureType", 
               values_to = "ExposureLevel") |>
  pivot_longer(cols = c(AVEDRNK3, MARIJAN1), 
               names_to = "Substance", 
               values_to = "Frequency")

ggplot(mari_alch_combined, aes(x = ExposureLevel, y = Frequency, color = ExposureType)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE) +  
  facet_wrap(~ Substance) +  
  labs(
    title = "Exposure to Drugs vs Alcohol and Marijuana Frequency",
    x = "Exposure Level",
    y = "Frequency"
     ) +
  theme_minimal() 
  

# linear fitting for both frequencies
alch_model <- lm(AVEDRNK3 ~ ACEDEPRS + ACEDRINK + ACEDRUGS, data = brfss_clean)
summary(alch_model)


mari_model <- lm(MARIJAN1 ~ ACEDEPRS + ACEDRINK + ACEDRUGS, data = brfss_clean)
summary(mari_model)

  
  
#relationship between abusive childhood experiences and mental health struggles

#%>% select(`ACEHURT1`, `ACESWEAR`,`ACETOUCH`, `ACEADSAF`)
#mental health 






###merged data analysis
merged_data|>
  ggplot(aes(x = log(`Personal income`)))+ geom_histogram()

merged_data|>
  ggplot(aes(x = AGE_GROUP))+ geom_histogram(stat = "count")


# correlation between state income and health -> very little
income_on_health <- merged_data|>
  filter(!(Health_status == "Fair"))|>
  mutate(Binary_health = if_else(Health_status %in% c("Excellent", "Very Good", "Good"), 1, 0))|>
  select(`Personal income`, AGE_GROUP, Binary_health, EDUCA, State)

cor(income_on_health$`Personal income`, income_on_health$Binary_health, use = "complete.obs")



#state gdp level on overall loneliness 
loneliness <- merged_data|>
  filter(loneliness_feeling_frequency == "Always" | loneliness_feeling_frequency == "Usually" |
           loneliness_feeling_frequency == "Rarely" | loneliness_feeling_frequency == "Never")|>
  mutate(Binary_lonely = if_else(loneliness_feeling_frequency %in% c("Always", "Usually"), 1, 0))|>
  select(`Gross domestic product (GDP)`, AGE_GROUP, Binary_lonely, EDUCA, `Personal income`)

mod1 <- glm(
  Binary_lonely ~  log10(`Gross domestic product (GDP)`)+ as.factor(AGE_GROUP),
  data = loneliness,
  family = "binomial"
)
  
summary(mod1)


#Alcohol_Drinks_Per_Day on Employment number 
plot_alco_employ<-
  ggplot(data = merged_data, aes(x= Alcohol_Drinks_Per_Day, y = `Total employment (number of jobs)`))+
  geom_point()+  
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Alcohol Drinks Per Day vs Employment",
       x = "Alcohol Drinks Per Day",
       y = "Employment Quantity") +
  theme_minimal()

plot_alco_employ
  

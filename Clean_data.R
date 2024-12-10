#install.packages("viridis")
#install.packages("sf")  
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


# relationship between education and loneliness, faceted by race
brfss_clean|>
  count(Health_status)
summary <- brfss_clean|>
  filter(loneliness_feeling_frequency != "Don’t know/Not sure" & loneliness_feeling_frequency != "Refused" 
         & !is.na(RACE) & RACE != "Uncertain/Refused" & !is.na(EDUCA) & EDUCA != "Refused")|>
  group_by(EDUCA, RACE)|>
  summarize(lonely = mean(loneliness_feeling_frequency == "Usually" | loneliness_feeling_frequency == "Always", na.rm = TRUE))

summary$EDUCA_factor <- factor(summary$EDUCA,levels = c("None/Kindergarten", "Elementary", "Some High School", 
                                         "High School Graduate","Some College", "College Graduate"))

ggplot(summary, aes(x = reorder(EDUCA_factor, desc(EDUCA_factor)), 
                    y = lonely, fill = EDUCA_factor)) +
  geom_col() +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  facet_wrap(~RACE)+
  coord_flip()+
  theme(             
    axis.text.y = element_blank(),                        
    axis.title.y = element_blank(),                      
  )+
  labs(fill = "Education level")+
  scale_fill_viridis_d()

brfss_clean|>
  filter(!is.na(EDUCA) & EDUCA != "Refused" & `_INCOMG1` != 9)|>
  group_by(EDUCA)|>
  summarize(higher_income = mean(`_INCOMG1` == 6 |`_INCOMG1` == 7))

cor_income_satisfaction <- brfss_clean|>
  filter(LSATISFY %in% c(1, 2, 3, 4), `_INCOMG1` != 9)
  cor(cor_income_satisfaction$LSATISFY,cor_income_satisfaction$`_INCOMG1`, use = "complete.obs")

ggplot(brfss_clean,aes(x = stress_feeling_frequency, y = MENTHLTH))+
  geom_boxplot()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Turley playing
anova_data <- brfss_clean %>%
  mutate(Health_status = as.factor(Health_status),
         stress_feeling_frequency = as.factor(stress_feeling_frequency),
         emotional_support = as.factor(emotional_support),
         PHYSHLTH = as.numeric(PHYSHLTH))

interaction <- aov(PHYSHLTH ~ Health_status * stress_feeling_frequency + emotional_support, data = anova_data)

summary(interaction)


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

# box plot version (updated version)
ggplot(adverse_drug, aes(x = factor(ExposureLevel), y = CurrentUseFrequency)) +
  geom_boxplot(aes(fill = factor(ExposureLevel))) +
  facet_grid(CurrentUseType ~ ExposureType, scales = "free") +
  labs(
    title = "Box Plot: Early Exposure to Drug Use and Current Frequency",
    x = "Exposure to Drugs at Home",
    y = "Current Drug Use Frequency"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


# statistical analysis method

# fix this
## marijuana and drinking frequency w adverse childhood # updated
mari_alch <- brfss_clean |>
  select(ACEDEPRS, ACEDRINK, ACEDRUGS, MARIJAN1)|>
  filter(!MARIJAN1 %in% c(88, 77, 99) & ACEDRINK != 7 & ACEDRINK != 9 & !is.na(ACEDRINK))

  
mari_alch_combined <- mari_alch |>
  pivot_longer(cols = c(ACEDEPRS, ACEDRINK, ACEDRUGS), 
               names_to = "ExposureType", 
               values_to = "ExposureLevel"
               ) |>
  pivot_longer(cols = c(MARIJAN1), 
               names_to = "Substance", 
               values_to = "Frequency") |>
  filter(!is.na(Frequency), !is.na(ExposureLevel))

ggplot(mari_alch_combined, aes(x = ExposureLevel, y = Frequency, color = ExposureType)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "loess", se = FALSE) +  
  facet_wrap(~ Substance) +  
  labs(
    title = "Exposure to Drugs vs Alcohol and Marijuana Frequency",
    x = "Exposure Level",
    y = "Frequency"
     ) +
  theme_minimal() 
  

# linear fitting for both frequencies
alch_model <- lm(AVEDRNK3 ~ ACEDEPRS + `ACEDRINK` + ACEDRUGS, data = brfss_clean)
summary(alch_model)


mari_model <- lm(MARIJAN1 ~ ACEDEPRS + ACEDRINK + ACEDRUGS, data = brfss_clean)
summary(mari_model)


#relationship between abusive childhood experiences+ one positve childhood experience  and mental health struggles via correlation matrix

childhood_mental <- brfss_clean %>% select(`_MENT14D`, ACEHURT1, ACESWEAR, ACETOUCH, ACEADSAF, ACEPRISN,ACEDEPRS, ADDEPEV3)
cor_matrix <- cor(childhood_mental, use = "complete.obs", method = "pearson")
print(cor_matrix)

# visualizing correlation matrix w correlation plot
library(corrplot)



#highlight groups of correlations in analysis


# relationship between  mental health status (0days, 1-13 days, 14-30 days) and other childhood experiences


childhood_mental_filtered <- brfss_clean %>%
  select(`_MENT14D`, ACEHURT1, ACESWEAR, ACETOUCH, ACEADSAF, ACEPRISN, ACEDEPRS, ADDEPEV3) %>%
  filter(across(everything(), ~ is.finite(.)))
childhood_mental_long <- childhood_mental_filtered %>%
  pivot_longer(cols = ACEHURT1:ADDEPEV3, names_to = "ACE_Variable", values_to = "ACE_Value")

# scatter plot
ggplot(childhood_mental_long, aes(x = ACE_Value, y = `_MENT14D`)) +
  geom_point(alpha = 0.6, color = "lightblue") +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  facet_wrap(~ ACE_Variable, scales = "free_x") +
  labs(
    title = "Relationship Between Childhood Adversities and Mental Health Days",
    x = "Adverse Childhood Experience Level",
    y = "Mental Health Days in Last 14 Days"
  ) +
  theme_minimal()


# medical cost, race, health status, lm
ggplot(data = brfss_clean, aes(x = Health_status, y = medical_cost)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Medical Cost by Health Status and Race",
    x = "Health Status",
    y = "Medical Cost"
  ) +
  facet_wrap(~ RACE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#new version for the medical cost, race, health status, glm
ht_mc<-brfss_clean|>
  mutate(
    medical_cost = recode(as.character(medical_cost), 'Yes' = 1, 'No' = 0), 
    Health_status = recode(as.character(Health_status),  "Excellent"= 5,
                           "Very Good"=4,
                           "Good"=3,
                           "Fair"=2,
                           "Poor"= 1,
                           "Don’t Know/Not Sure"=7,
                           "Refused"=9)
  )%>%drop_na(medical_cost, Health_status,)
 

ht_mc_model <- glm(medical_cost ~ Health_status, data = ht_mc, family = "binomial")
summary(ht_mc_model)

predicted_prob <- predict(ht_mc_model, type = "response")
ht_mc$Y_hat <- predicted_prob

ggplot(ht_mc, aes(x = Health_status, y = Y_hat )) +
  geom_point( alpha = 0.6,color = "blue") + 
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +  
  labs(title = "Predicted Probability of Unaffordability of Medical Cost by Health Status",
       x = "Health Status",
       y = "Predicted Probability of 'No' for Medical Cost") +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  guides(color = "none")

### merged data analysis
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

cor(merged_data$`Gross domestic product (GDP)`, merged_data$MENTHLTH, use = "complete.obs")


#state gdp level on overall loneliness, mental health 
loneliness <- merged_data|>
  filter(loneliness_feeling_frequency == "Always" | loneliness_feeling_frequency == "Usually" |
           loneliness_feeling_frequency == "Rarely" | loneliness_feeling_frequency == "Never", !is.na(MENTHLTH))|>
  mutate(Binary_lonely = if_else(loneliness_feeling_frequency %in% c("Always", "Usually"), 1, 0))|>
  select(`Gross domestic product (GDP)`, AGE_GROUP, Binary_lonely, EDUCA, `Personal income`, MENTHLTH, State)|>
  rename(GDP = `Gross domestic product (GDP)`)

mod1 <- glm(
  Binary_lonely ~  log10(GDP)+ as.factor(AGE_GROUP),
  data = loneliness,
  family = "binomial"
)
summary(mod1)

poisson_model <- glm(MENTHLTH ~ log(GDP) + Binary_lonely, 
                     family = quasipoisson, data = loneliness)
summary(poisson_model)
(exp(coefficients(poisson_model))-1)*100

loneliness$predicted <- predict(poisson_model, type = "response")

ggplot(loneliness, aes(x = GDP, y = predicted, color = as_factor(Binary_lonely))) +
geom_line() +
labs(title = "Number of Days with Bad Mental Health Based on Your State GDP",
     x = "State GDP",
     y = "Number of Poor Mental Health Days",
     color = "Emotional Status") +
scale_color_manual(values = c("red", "blue"), labels = c("Not Lonely", "Feeling Lonely"))+
  scale_x_log10()+
theme_minimal()

# Filter the dataset to remove missing values for relevant variables
data2_clean_filtered <- merged_data %>%
  filter(!is.na(Alcohol_Drinks_Per_Day), !is.na(BMI_category))

# Fit the negative binomial regression model
nb_model <- glm.nb(Alcohol_Drinks_Per_Day ~ BMI_category, data = data2_clean_filtered)

# Display the model summary
summary(nb_model)

# Generate predictions and add them to the dataset
data2_clean_filtered <- data2_clean_filtered %>%
  mutate(predicted_drinks = predict(nb_model, type = "response"))

# Create a visualization of observed vs. predicted values
ggplot(data2_clean_filtered, aes(x = BMI_category, y = Alcohol_Drinks_Per_Day)) +
  geom_boxplot(aes(fill = BMI_category), outlier.shape = NA) +  # Boxplot for observed data
  geom_jitter(aes(y = Alcohol_Drinks_Per_Day), width = 0.2, alpha = 0.5) +  # Add jitter for observed points
  geom_point(aes(y = predicted_drinks), color = "blue", size = 2, 
             position = position_jitter(width = 0.2)) +  # Add predicted values
  labs(title = "Negative Binomial Regression: Alcohol Consumption by BMI Category",
       x = "BMI Category",
       y = "Average Alcohol Drinks per Day") +
  scale_fill_brewer(palette = "Set2") +  # Optional: adjust color palette
  theme_minimal()

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

test_data <- brfss_clean|>
  filter(!is.na(Alcohol_Drinks_Per_Day) & `_INCOMG1` != 9)

test <- lm(data = test_data, Alcohol_Drinks_Per_Day~ `_INCOMG1` )
summary(test)

# Race (in minority:white ratio) and income
min_ratio <- merged_data |> 
  group_by(State, RACE) |> 
  summarise(count = n(), .groups = "drop") |> 
  group_by(State) |> 
  mutate(total_population = sum(count)) |> 
  ungroup() |> 
  pivot_wider(names_from = RACE, values_from = count, values_fill = 0) |> 
  mutate(minority_to_white_ratio = (total_population - White) / White )

merged_data <- merged_data |> 
  left_join(min_ratio |> select(State, minority_to_white_ratio), by = "State")

income_model <- glm(`Personal income` ~ minority_to_white_ratio, data = merged_data)
income_model <- lm(`Personal income` ~ minority_to_white_ratio, data = merged_data)
merged_data|>
  select(minority_to_white_ratio)

summary(income_model)

cor(merged_data$`Personal income`, merged_data$minority_to_white_ratio)


# Visualization

ggplot(merged_data, aes(x = minority_to_white_ratio, y = `Personal income`)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  theme_minimal() +
  labs(
    title = "Relationship Between Minority-to-White Ratio per Sate and Income",
    x = "Minority-to-White Ratio",
    y = "Personal Income"
  )


# correlation between state income and mental health 
income_on_health <- merged_data|>
  filter(!(Health_status == "Fair"))|>
  mutate(Binary_health = if_else(Health_status %in% c("Excellent", "Very Good", "Good"), 1, 0))|>
  select(`Personal income`, AGE_GROUP, Binary_health, EDUCA, State)

cor(income_on_health$`Personal income`, income_on_health$Binary_health, use = "complete.obs")

  
#insurace_status,  Per capita disposable personal income 
  
  ggplot(data = merged_data, aes(x = `insurance_status`, y = `Per capita disposable personal income 7`, fill = `insurance_status`)) +
  geom_boxplot() +
  labs(
    title = "Per Capita Disposable Personal Income by Insurance Status",
    x = "Insurance Status",
    y = "Per Capita Disposable Personal Income"
  )  + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  #insurance status, Per capita personal income 

  ggplot(data = merged_data, aes(x = `insurance_status`, y = `Per capita personal income 6`   
   , fill = `insurance_status`)) +
    geom_boxplot() +
    labs(
      title = "Per Capita  Personal Income by Insurance Status",
      x = "Insurance Status",
      y = "Per capita personal income 6"
    )  + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#possible variables: "loneliness_feeling_frequency", "insurance_status" , `medical_cost`
# map data
library(tidycensus)
library(sf)
library(viridis)


US_map <- get_acs(geography = "state",
                            variables = "B01003_001E",
                            year =2020,
                            geometry = TRUE)|>
  rename(State = NAME)

# map visualization

# could afford to see doctor, 1yes, 2 no
mean_medical_costs_data <- brfss_clean %>%
  filter(!`MEDCOST1` %in% c(7, 9)) %>%  
  group_by(State) %>%  
  summarize(mean_medical_cost = mean(`MEDCOST1`, na.rm = TRUE))


map_dataset <- merge(US_map, mean_medical_costs_data, by = "State", all.x = TRUE)
map_dataset <- map_dataset %>%
  mutate(
    centroid = st_centroid(geometry),         
    longitude = st_coordinates(centroid)[, 1] 
  ) %>%
  filter(longitude > -60 & longitude < 180)   



#  heatmap with the mean medical costs affordability
ggplot(map_dataset) +
  geom_sf(aes(fill = mean_medical_cost), color = "pink") +
    scale_fill_viridis() +
  labs(
    title = " Mean Medical Cost Affordability by State",
    fill = "Mean Medical Cost Affordability"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


#heatmap with mean mental health by state
#now thinking about your mental health, which includes stress, depression, and problems with emotions, 
#for how many days during the past 30 days was your mental health not good?

mean_mental_data <- brfss_clean %>%
  filter(!`MENTHLTH` %in% c(77, 99, 88)) %>%  
  group_by(State) %>%  
  summarize(mean_mental = mean(`MENTHLTH`, na.rm = TRUE))

map_dataset2 <- merge(US_map, mean_mental_data, by = "State", all.x = TRUE)

#  heatmap with the mean medical costs affordability
ggplot(map_dataset2) +
  geom_sf(aes(fill = mean_mental), color = "pink") + 
  scale_fill_viridis() +
  labs(
    title = " Number of Days With Poor Mental Health",
    fill = "Mean Amount of Days"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

#heat map with insurance access, calculating mean, #1 is have some form and 2 is not
mean_insurance_data <- brfss_clean%>%
  filter(!`_HLTHPL1` %in% c(9)) %>%  
  group_by(State) %>%  
  summarize(mean_insurance = mean(`_HLTHPL1`, na.rm = TRUE))
map_dataset3 <- merge(US_map, mean_insurance_data, by = "State", all.x = TRUE)

ggplot(map_dataset3) +
  geom_sf(aes(fill = mean_insurance), color = "pink") + 
  scale_fill_viridis() +
  labs(
    title = " Access to Insurance",
    fill = "Access Mean"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") 


  
  




#heatnmap with mean physical health



# shiny live data prep

shiny_table <- merged_data |>
  group_by(State) |>
  summarize(
    avg_ment_unwell_days = mean(MENTHLTH, na.rm = TRUE),  
    avg_physical_unwell_days = mean(PHYSHLTH, na.rm = TRUE),
    perc_cannot_afford = sum(MEDCOST1 == 2, na.rm = TRUE) / sum(!is.na(MEDCOST1)), 
    perc_uninsured = sum(insurance_status == "No Insurance", na.rm = TRUE) / n()
  )|>
  arrange(State) 


saveRDS(shiny_table, file = "dataset/shiny_table.rds")

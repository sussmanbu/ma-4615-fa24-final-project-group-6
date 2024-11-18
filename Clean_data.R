library(tidyverse)
library(haven)
library(ggplot2)
library(readxl)

#cleaning data

#removing rows with missing values

library(dplyr)



#filtered all incomplete interviews
data2 <- read_xpt("dataset-ignore/LLCP2023.XPT ")|>

brfss <- read_xpt("dataset-ignore/LLCP2023.XPT ")|>
 filter(DISPCODE == 1100)

  
#new dataset-income_clean
income_data <- read_excel("dataset-ignore/Table.xlsx", skip = 4)
income_clean <- income_data|>
  select(-LineCode)|>
  mutate(`2023` = if_else(`2023` == "(NA)", NA, `2023`))|>
  pivot_wider(names_from = Description, values_from = `2023`)|>
  head(-13)|>
  select_if(~ !any(is.na(.)))|>
  filter(GeoFips != "00000")|>
  mutate(across(`Real GDP (millions of chained 2017 dollars) 1` :
                  `Total employment (number of jobs)`, as.numeric))
  

  #merging income_clean with  brffs  after it is cleaned

# cleaned general health condition
brfss_clean <- brfss|>
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
  
  
  # cleaned lonely variable
  mutate(`SDLONELY` = case_when(
    `SDLONELY` == 1 ~ "Always",
    `SDLONELY` == 2 ~ "Usually", 
    `SDLONELY` == 3 ~ "Sometimes", 
    `SDLONELY` == 4 ~ "Rarely",
    `SDLONELY` == 5 ~ "Never",
    `SDLONELY` == 7 ~ "Don’t know/Not sure", 
    `SDLONELY` == 9 ~ "Refused"))|>
  rename(loneliness_feeling_frequency = `SDLONELY`)|>
  
  
  # cleaned stressed variable
  mutate(`SDHSTRE1` = case_when(
    `SDHSTRE1` == 1 ~ "Always",
    `SDHSTRE1` == 2 ~ "Usually", 
    `SDHSTRE1` == 3 ~ "Sometimes", 
    `SDHSTRE1` == 4 ~ "Rarely",
    `SDHSTRE1` == 5 ~ "Never",
    `SDHSTRE1` == 7 ~ "Don’t know/Not sure", 
    `SDHSTRE1` == 9 ~ "Refused"))|>
  rename(stress_feeling_frequency = `SDHSTRE1`)|>
  
  
  
  # cleaned satisfaction variable
  mutate(`EMTSUPRT` = case_when(
    `EMTSUPRT` == 1 ~ "Always",
    `EMTSUPRT` == 2 ~ "Usually", 
    `EMTSUPRT` == 3 ~ "Sometimes", 
    `EMTSUPRT` == 4 ~ "Rarely",
    `EMTSUPRT` == 5 ~ "Never",
    `EMTSUPRT` == 7 ~ "Don’t know/Not sure", 
    `EMTSUPRT` == 9 ~ "Refused"))|>
  rename(emotional_support = `EMTSUPRT`)|>
  
  
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
  filter(!(is.na(Exercise_frequency)))|>
  select(-EXEROFT1, -EXEROFT2)|>

  
  
  # Cleaning the BMI categories
  mutate(BMI_category = recode(as.factor(`_BMI5CAT`),
                               `1` = "Underweight",
                               `2` = "Normal Weight",
                               `3` = "Overweight",
                               `4` = "Obese",
                               `9999` = NA_character_))


#Cleaning States
# Recode '`_STATE`' values into state names

brfss_clean <- brfss_clean |>
  mutate(State = case_when(
    `_STATE` == 1 ~ "Alabama", 
    `_STATE` == 2 ~ "Alaska", 
    `_STATE` == 4 ~ "Arizona", 
    `_STATE` == 5 ~ "Arkansas", 
    `_STATE` == 6 ~ "California", 
    `_STATE` == 8 ~ "Colorado", 
    `_STATE` == 9 ~ "Connecticut", 
    `_STATE` == 10 ~ "Delaware", 
    `_STATE` == 11 ~ "District of Columbia", 
    `_STATE` == 12 ~ "Florida", 
    `_STATE` == 13 ~ "Georgia", 
    `_STATE` == 15 ~ "Hawaii", 
    `_STATE` == 16 ~ "Idaho", 
    `_STATE` == 17 ~ "Illinois", 
    `_STATE` == 18 ~ "Indiana", 
    `_STATE` == 19 ~ "Iowa", 
    `_STATE` == 20 ~ "Kansas", 
    `_STATE` == 22 ~ "Louisiana", 
    `_STATE` == 23 ~ "Maine", 
    `_STATE` == 24 ~ "Maryland", 
    `_STATE` == 25 ~ "Massachusetts", 
    `_STATE` == 26 ~ "Michigan", 
    `_STATE` == 27 ~ "Minnesota", 
    `_STATE` == 28 ~ "Mississippi", 
    `_STATE` == 29 ~ "Missouri", 
    `_STATE` == 30 ~ "Montana", 
    `_STATE` == 31 ~ "Nebraska", 
    `_STATE` == 32 ~ "Nevada", 
    `_STATE` == 33 ~ "New Hampshire", 
    `_STATE` == 34 ~ "New Jersey", 
    `_STATE` == 35 ~ "New Mexico", 
    `_STATE` == 36 ~ "New York", 
    `_STATE` == 37 ~ "North Carolina", 
    `_STATE` == 38 ~ "North Dakota", 
    `_STATE` == 39 ~ "Ohio", 
    `_STATE` == 40 ~ "Oklahoma", 
    `_STATE` == 41 ~ "Oregon", 
    `_STATE` == 44 ~ "Rhode Island", 
    `_STATE` == 45 ~ "South Carolina", 
    `_STATE` == 46 ~ "South Dakota", 
    `_STATE` == 47 ~ "Tennessee", 
    `_STATE` == 48 ~ "Texas", 
    `_STATE` == 49 ~ "Utah", 
    `_STATE` == 50 ~ "Vermont", 
    `_STATE` == 51 ~ "Virginia", 
    `_STATE` == 53 ~ "Washington", 
    `_STATE` == 54 ~ "West Virginia", 
    `_STATE` == 55 ~ "Wisconsin", 
    `_STATE` == 56 ~ "Wyoming", 
    `_STATE` == 66 ~ "Guam", 
    `_STATE` == 72 ~ "Puerto Rico", 
    `_STATE` == 78 ~ "Virgin Islands"
  ))|>
  

  
  # cleaned number of days physical health unwell last month
  mutate(PHYSHLTH = case_when(
    PHYSHLTH %in% c(77, 99) ~ NA_real_,  
    PHYSHLTH == 88 ~ 0,                    
    TRUE ~ PHYSHLTH                        
  )) |> 
  
  
  # cleaned number of days mental health unwell last month
  mutate(MENTHLTH = case_when(
    MENTHLTH %in% c(77, 99) ~ NA_real_,  
    MENTHLTH == 88 ~ 0,                    
    TRUE ~ MENTHLTH                       
  )) |>
  
  
  #clean age category
  
  mutate(
    AGE_GROUP = case_when(
      `_AGE_G` == 1 ~ "Age 18 to 24",
      `_AGE_G` == 2 ~ "Age 25 to 34",
      `_AGE_G` == 3 ~ "Age 35 to 44",
      `_AGE_G` == 4 ~ "Age 45 to 54",
      `_AGE_G` == 5 ~ "Age 55 to 64",
      `_AGE_G` == 6 ~ "Age 65 or older",
      TRUE ~ NA_character_ 
    )) |>
  
  #cleaning medical cost
  mutate(
    medical_cost = recode(
      as.factor(`MEDCOST1`),
      `1` = "Yes",
      `2` = "No",
      `7` = "Don’t know/Not sure",
      `9` = "Refused"
    ))|>
  
  # cleaned Employment Status
  
  mutate(EMPLOY1 = case_when(
    EMPLOY1 %in% c(1, 2) ~ "Employed",
    EMPLOY1 %in% c(3, 4) ~ "Unemployed", 
    EMPLOY1 %in% c(5, 6, 7, 8) ~ "Not in labor force",
    EMPLOY1 == 9 ~ "Uncertain/Refused", 
    TRUE ~ "Other"
  ))|>
  
  
  # cleaned Metropolitan Status Code
  
  mutate(MSCODE = case_when(
    MSCODE   == 1 ~ "In MSA - Center City",
    MSCODE %in% c(2, 3) ~ "In MSA - Surrounding Area",
    MSCODE == 5 ~ "Not in MSA", 
    TRUE ~ "Other/Unknown" 
  )
  )|>
  
  # number of drinks per day
  mutate(AVEDRNK3 = case_when(
    AVEDRNK3 %in% c(77, 99) ~ NA_real_,  
    AVEDRNK3 == 88 ~ 0, 
    TRUE ~ AVEDRNK3)) |> 
  rename(Alcohol_Drinks_Per_Day = `AVEDRNK3`) |> 
  
  
  mutate(Physical_Activity_Index = case_when(
    `_PAINDX3` == 1 ~ "Low",
    `_PAINDX3` == 2 ~ "Moderate",
    `_PAINDX3` == 3 ~ "High",
    `_PAINDX3` == 9 ~ "Unknown",
    is.na(`_PAINDX3`) ~ "Not asked or Missing",
    TRUE ~ "Other"
  ))


#creating merged_dataset that is a merge of bffrs and economic_table

merged_data <- merge(brfss_clean, income_clean, by.x = "State", by.y = "GeoName", all = FALSE) 
head(merged_data)  

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


#relationship between abusive childhood experiences+ one posiitve childhood experience  and mental health struggles via correlation matrix

childhood_mental <- brfss_clean %>% select(ACEHURT1, ACESWEAR, ACETOUCH, ACEADSAF, ACEPRISN,ACEDEPRS, ADDEPEV3)
cor_matrix <- cor(childhood_mental, use = "complete.obs", method = "pearson")
print(cor_matrix)

colnames(brfss_clean)

# medcost, race, health status
ggplot(data = brfss_clean, aes(x = "Health_status", y = "medical_cost")) +
  geom_point() +
  labs(
    title = "Medical Cost by Health Status and Race",
    x = "Health Status",
    y = "Medical Cost"
  ) +
  facet_wrap(~ RACE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#merged data analysis
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

income_model <- lm(`Personal income` ~ minority_to_white_ratio, data = merged_data)

summary(income_model)

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

  
#insurace_status,  Per capita disposable personal income 
  
  ggplot(data = merged_data, aes(x = insurace_status, y = `Per capita disposable personal income 7`, fill = insurace_status)) +
  geom_boxplot() +
  labs(
    title = "Per Capita Disposable Personal Income by Insurance Status",
    x = "Insurance Status",
    y = "Per Capita Disposable Personal Income"
  )  + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  #insurance status, Per capita personal income 

  ggplot(data = merged_data, aes(x = insurace_status, y = `Per capita personal income 6`   
   , fill = insurace_status)) +
    geom_boxplot() +
    labs(
      title = "Per Capita  Personal Income by Insurance Status",
      x = "Insurance Status",
      y = "Per capita personal income 6"
    )  + theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  

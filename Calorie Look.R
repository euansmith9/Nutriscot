setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MS984/EY Project/Scottish Health Survey 2021")

library(haven)
library(tidyverse)
library(ggplot2)

# Reading in Scottish Health Survey 2021 dataset

healthsurvey21 <- read_sav("shes21i_eul.sav")
healthsurvey21$adtot10c

which( colnames(healthsurvey21)=="Sex" ) # This was to see which column number the variables were to find in key

# Selecting relevant variables

surveydata <- healthsurvey21 %>%
  select(SlfHtDV, SlfWtDV, BMI_SR, age, Sex, adtot10c) %>%
  na.omit()

# Creating .csv file of survey with relevant variables

write.csv(healthsurvey21, "~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MS984/EY Project/UKDA-9048-spss/spss/spss25/Scottish_Health_Survey_21_Full.csv")

surveydata <- read.csv("Scottish_Health_Survey_21.csv")

# Filtered data and assigned activity levels to be used to calculate Basal metabolic rate (BMR) and Total Daily Energy Expenditure (TDEE)

surveydata <- surveydata %>%
  rename(Respondent = X) %>%
  filter(age >= 20, age <= 64) %>%
  mutate(
    activity = ifelse(adtot10c == 0, 1.2, 
                      ifelse(adtot10c == 1, 1.375,
                             ifelse(adtot10c == 2, 1.55, 
                                    ifelse(adtot10c == 3, 1.725, 
                                           ifelse(adtot10c == 4, 1.9, NA))))),
    Sex = ifelse(Sex == 1, "Male",
                 ifelse(Sex == 2, "Female", NA))
  )

# Function to calculate BMR

calculate_bmr <- function(weight, height, age, sex) {
  if (sex == "Male") {
    return((10 * weight) + (6.25 * height) - (5 * age) + 5)
  } else {
    return((10 * weight) + (6.25 * height) - (5 * age) - 161)
  }
}

# Applying funtion and calculating TDEE/Daily Calorie Intake

surveydata <- surveydata %>%
  mutate(
    bmr = mapply(calculate_bmr, SlfWtDV, SlfHtDV, age, Sex),
    calories = bmr * activity
  )

summary(surveydata$SlfWtDV)
summary(surveydata$calories)

# Plots

p1 <- ggplot(surveydata, aes(x = calories)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 50, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Distribution of Total Daily Energy Expenditure",
       x = "Calories", y = "Density") +
  geom_vline(xintercept = 2000, color = "red", linetype = "dashed", size = 0.5) +
  theme_minimal()
p1

# Calculate percentile 2000 calories falls under

quantile(surveydata$calories, probs = seq(0,1,0.01))

calories_ecdf <- ecdf(surveydata$calories)
calories_ecdf(2000)*100

ggplot(surveydata, aes(x = BMI_SR)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  geom_vline(xintercept = 18.5, color = "red", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 24.9, color = "red", linetype = "dashed", size = 0.5) + 
  labs(title = "Distribution of BMI",
       x = "BMI", y = "Density") +
  theme_minimal()

# Filtered plots of those with healthy BMI

healthybmi <- surveydata %>%
  filter(BMI_SR >= 18.5, BMI_SR <= 24.9)

ggplot(healthybmi, aes(x = calories)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 50, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Distribution of Total Daily Energy Expenditure",
       x = "Calories", y = "Density") +
  geom_vline(xintercept = 2000, color = "red", linetype = "dashed", size = 0.5) +
  theme_minimal()

ggplot(healthybmi, aes(x = BMI_SR)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  geom_vline(xintercept = 18.5, color = "red", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 24.9, color = "red", linetype = "dashed", size = 0.5) + 
  labs(title = "Distribution of BMI",
       x = "BMI", y = "Density") +
  theme_minimal()

# Filtered plots of those who do a lot of exercise

fitpeople <- surveydata %>%
  filter(adtot10c >= 3)

ggplot(fitpeople, aes(x = calories)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 50, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Distribution of Total Daily Energy Expenditure",
       x = "Calories", y = "Density") +
  geom_vline(xintercept = 2000, color = "red", linetype = "dashed", size = 0.5) +
  theme_minimal()

ggplot(fitpeople, aes(x = BMI_SR)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", binwidth = 1, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  geom_vline(xintercept = 18.5, color = "red", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = 24.9, color = "red", linetype = "dashed", size = 0.5) + 
  labs(title = "Distribution of BMI",
       x = "BMI", y = "Density") +
  theme_minimal()

# Age is an issue. It doesn't reflect real population of Scotland skewing results

quantile(surveydata$age, probs = seq(0,1,0.1))

ggplot(surveydata, aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, position = "identity", fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Distribution of Age in Survey",
       x = "Age", y = "Density") +
  theme_minimal()

# Population dataset

pop <- read.csv('Scottish Population 2022.csv')

# Cleaning up dataset

pop <- pop %>%
  filter(Year == 2022) %>%
  pivot_longer(
    cols = starts_with("Aged"),
    names_to = "age_group",   
    values_to = "population") %>%
  mutate(Sex = ifelse(Sex == "Males", "Male",
                      ifelse(Sex == "Females", "Female",
                             ifelse(Sex == "Persons", "Both", NA)))) %>%
  select(Sex, age_group, population)

head(pop)
(pop$age_group)

pop <- pop %>%
  mutate(age_group = gsub("\\.{2,}", "-", age_group), 
         age_group = gsub("\\.$", "", age_group),     
         age_group = gsub("\\.", "-", age_group),      
         age_group = gsub("Aged-", "", age_group),
         age_group = gsub("-years", "", age_group),
         age_group = gsub("-year", "", age_group))


pop <- pop %>%
  filter(age_group %in% c("20-24", "25-29", "30-34", 
                          "35-39", "40-44", "45-49", 
                          "50-54", "55-59", "60-64"))

# Getting proportions to apply weights to calorie data

pop <- pop %>%
  filter(Sex %in% c('Male', 'Female')) %>%
  group_by(Sex) %>%
  mutate(total_population = sum(population)) %>%
  mutate(population_prop = population / total_population) %>%
  ungroup()

survey_proportions <- surveydata %>% 
  filter(age >= 20, age <= 64) %>%
  group_by(Sex, age_group = case_when(
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64"
  )) %>%
  summarise(survey_count = n()) %>%
  group_by(Sex) %>%
  mutate(survey_prop = survey_count / sum(survey_count)) %>%
  ungroup()

merged_data <- survey_proportions %>%
  left_join(pop, by = c("Sex", "age_group")) %>%
  mutate(weight = population_prop / survey_prop)

surveydata <- surveydata %>%
  mutate(age_group = case_when(
    age >= 20 & age <= 24 ~ "20-24",
    age >= 25 & age <= 29 ~ "25-29",
    age >= 30 & age <= 34 ~ "30-34",
    age >= 35 & age <= 39 ~ "35-39",
    age >= 40 & age <= 44 ~ "40-44",
    age >= 45 & age <= 49 ~ "45-49",
    age >= 50 & age <= 54 ~ "50-54",
    age >= 55 & age <= 59 ~ "55-59",
    age >= 60 & age <= 64 ~ "60-64"
  )) %>%
  left_join(merged_data %>% select(Sex, age_group, weight), by = c("Sex", "age_group"))

# Weighted plot more representative of real population

p2 <- ggplot(surveydata, aes(x = calories, weight = weight)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, binwidth = 50, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Weighted Distribution of Total Daily Energy Expenditure",
       x = "Calories", y = "Density") +
  geom_vline(xintercept = 2000, color = "red", linetype = "dashed", size = 0.5) +
  theme_minimal()
p2

library(patchwork)

p1/p2

weighted.mean(surveydata$calories, surveydata$weight)
mean(surveydata$calories)

# Not much difference

# Now it can be over 2000 as people are obese shown in BMI plot now let's see if everyone had a perfect BMI (BMI of 22)

surveydata <- surveydata %>%
  mutate(
    perfectweight = 22*((SlfHtDV/100)^2),
    perfectbmr = mapply(calculate_bmr, perfectweight, SlfHtDV, age, Sex),
    perfectcalories = bmr * activity
  )

ggplot(surveydata, aes(x = perfectcalories)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, binwidth = 50, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Distribution of 'Perfect' Total Daily Energy Expenditure",
       x = "Calories", y = "Density") +
  geom_vline(xintercept = 2000, color = "red", linetype = "dashed", size = 0.5) +
  theme_minimal()

# Calculate percentile 2000 calories falls under

perfectcalories_ecdf <- ecdf(surveydata$perfectcalories)
perfectcalories_ecdf(2000)*100

quantile(surveydata$perfectcalories, probs = seq(0,1,0.01))

# Perfect BMI weighted

ggplot(surveydata, aes(x = perfectcalories, weight = weight)) +
  geom_histogram(aes(y = after_stat(density)), alpha = 0.8, binwidth = 50, fill = 'blue') +
  geom_density(alpha = 0.5, adjust = 2, fill = 'blue') +
  labs(title = "Weighted Distribution of 'Perfect' Total Daily Energy Expenditure",
       x = "Calories", y = "Density") +
  geom_vline(xintercept = 2000, color = "red", linetype = "dashed", size = 0.5) +
  theme_minimal()



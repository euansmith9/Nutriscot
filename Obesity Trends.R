setwd("~/Library/CloudStorage/OneDrive-UniversityofStrathclyde/Fifth Year/MS984/Datasets")

library(tidyverse)
library(ggplot2)

health <- read.csv("shes9821.csv")

colnames(health)

health_clean <- health %>%
  filter(bmi > 0, age > 18) %>%
  mutate(bmi_class_c = case_when(bmi < 18.5 ~ "Underweight", 
                                 bmi >= 18.5 & bmi < 25 ~ "Normal",
                                 bmi >= 25 & bmi < 30 ~ "Overweight",
                                 bmi >= 30 ~ "Obese")) %>%
  mutate(sex = ifelse(sex == 1, 'Male', 'Female')) %>%
  mutate(age_group = case_when(age >= 18 & age <= 30 ~ "18-30",
                               age >= 31 & age <= 40 ~ "31-45",
                               age >= 41 & age <= 50 ~ "41-50",
                               age >= 51 & age <= 60 ~ "51-60",
                               age >= 61 & age <= 70 ~ "61-70",
                               age >= 70 ~ "70+"))

bmi_trend <- health_clean %>%
  group_by(year, bmi_class_c) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(prevalence = count / sum(count))

ggplot(bmi_trend, aes(x = year, y = prevalence, colour = bmi_class_c, group = bmi_class_c)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  labs(title = "Prevalence of BMI Class Over Time",
       x = "Year",
       y = "Prevalence",
       colour = "BMI Class") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title = element_text(size = 18, face = "bold"),    
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.title.y = element_text(size = 14, face = "bold"),      
        axis.text.x = element_text(size = 12, face = "bold"),     
        axis.text.y = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face ="bold"), 
        legend.text = element_text(size = 12))
  

bmi_gender21 <- health_clean %>%
  filter(year == 2021) %>%
  group_by(sex, bmi_class_c) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(sex) %>%
  mutate(prevalence = count / sum(count))

ggplot(bmi_gender21, aes(x = sex, y = prevalence, fill = bmi_class_c)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Prevalence of BMI Class in Genders",
       x = "Gender",
       y = "Prevalence",
       fill = "BMI Class") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)
  

bmi_income21 <- health_clean %>%
  filter(year == 2021) %>%
  group_by(eqv_income, bmi_class_c) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(eqv_income) %>%
  mutate(prevalence = count / sum(count)) %>%
  filter(bmi_class_c == 'Obese')

ggplot(bmi_income21, aes(x = eqv_income, y = prevalence)) +
  geom_bar(aes(fill = as.factor(eqv_income)), stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Prevalence of Obesity between Income Groups",
       x = "Income Group",
       y = "Prevalence") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none',
        plot.title = element_text(size = 18, face = "bold"),    
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.title.y = element_text(size = 14, face = "bold"),      
        axis.text.x = element_text(size = 12, face = "bold"),     
        axis.text.y = element_text(size = 12, face = "bold") )

bmi_age21 <- health_clean %>%
  filter(year == 2021) %>%
  group_by(age_group, bmi_class_c) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(age_group) %>%
  mutate(prevalence = count / sum(count)) %>%
  filter(bmi_class_c == 'Obese')

ggplot(bmi_age21, aes(x = age_group, y = prevalence)) +
  geom_bar(aes(fill = age_group), stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Prevalence of Obesity by Age Groups",
       x = "Age Group",
       y = "Prevalence") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none',
        plot.title = element_text(size = 18, face = "bold"),    
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.title.y = element_text(size = 14, face = "bold"),      
        axis.text.x = element_text(size = 12, face = "bold"),     
        axis.text.y = element_text(size = 12, face = "bold") )

survey2021 <- read.table(("shes21i_eul.tab"), sep = "\t", header = T)

survey2021[survey2021 < 0] <- NA

summary(survey2021$Type2)

survey2021 <- survey2021 %>%
  filter(Type2 > 0 & age > 18) %>%
  mutate(diabetes = ifelse(Type2 == 1, "Has Type 2 Diabetes", "Does Not Have Type 2 Diabetes")) %>%
  mutate(Sex = ifelse(Sex == 1, 'Male', 'Female')) %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 30 ~ "18-30",
    age >= 31 & age <= 40 ~ "31-45",
    age >= 41 & age <= 50 ~ "41-50",
    age >= 51 & age <= 60 ~ "51-60",
    age >= 61 & age <= 70 ~ "61-70",
    age >= 70 ~ "70+"))

diabetes_income21 <- survey2021 %>%
  group_by(eqv5_15, diabetes) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(eqv5_15) %>%
  mutate(prevalence = count / sum(count)) %>%
  filter(diabetes == 'Has Type 2 Diabetes')

ggplot(diabetes_income21, aes(x = eqv5_15, y = prevalence)) +
  geom_bar(aes(fill = as.factor(eqv5_15)), stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Prevalence of Type 2 Diabetes by Income Group",
       x = "Income Group",
       y = "Prevalence") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none',
        plot.title = element_text(size = 18, face = "bold"),    
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.title.y = element_text(size = 14, face = "bold"),      
        axis.text.x = element_text(size = 12, face = "bold"),     
        axis.text.y = element_text(size = 12, face = "bold") )

diabetes_age21 <- survey2021 %>%
  group_by(age_group, diabetes) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(age_group) %>%
  mutate(prevalence = count / sum(count)) %>%
  filter(diabetes == 'Has Type 2 Diabetes')

ggplot(diabetes_age21, aes(x = age_group, y = prevalence)) +
  geom_bar(aes(fill = age_group), stat = "identity", position = "dodge", alpha = 0.8) +
  labs(title = "Prevalence of Type 2 Diabetes by Age Group",
       x = "Age Group",
       y = "Prevalence") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  theme(legend.position = 'none',
        plot.title = element_text(size = 18, face = "bold"),    
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.title.y = element_text(size = 14, face = "bold"),      
        axis.text.x = element_text(size = 12, face = "bold"),     
        axis.text.y = element_text(size = 12, face = "bold") )


intake24 <- read.table(("shes21_intake24_person_level_additional_variables_eul.tab"), sep = "\t", header = T)

intake24[intake24 < 0] <- NA

head(survey2021)

head(intake24)

sugardata <- left_join(survey2021, intake24, by = c("Cpseriala" = "Cpseriala"))

sugardata <- sugardata %>%
  filter(bmi_adj > 0, age > 18) %>%
  mutate(bmi_class_c = case_when(bmi_adj < 18.5 ~ "Underweight", 
                                 bmi_adj >= 18.5 & bmi_adj < 25 ~ "Normal",
                                 bmi_adj >= 25 & bmi_adj < 30 ~ "Overweight",
                                 bmi_adj >= 30 ~ "Obese"))

summary(sugardata$Totalsugarsg)

ggplot(sugardata, aes(x = diabetes, y = Totalsugarsg)) +
  geom_boxplot() +
  labs(x = '',
       y = 'Total Sugars (g) diet only') +
  theme_minimal()

ggplot(sugardata, aes(x = bmi_class_c, y = Totalsugarsg)) +
  geom_boxplot() +
  labs(x = '',
       y = 'Total Sugars (g) diet only') +
  theme_minimal()

ggplot(sugardata, aes(x = bmi_class_c, y = Energykcal)) +
  geom_boxplot() +
  labs(x = '',
       y = 'Total Energy (kcal) diet only') +
  theme_minimal()


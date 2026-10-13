
###################################

# Workshop R-Introduction - Lecture

# Date: 13 October 20216

###################################


rm(list = ls())



###################################
# Data manipulation using tidyverse
###################################


# Task
# Load packages and data

# Install and load packages
library(tidyverse)
library(skimr)

# Load and inspect data
library(NHANES)
data(NHANES)
skim(NHANES)


# Lecture
# Select variables and rows

# Select a variable for calculation
select(NHANES, Age) %>% pull() %>%  mean()
mean(pull(select(NHANES, Age)))

# Select variables and rows
NHANES_subset <- select(NHANES, ID, SurveyYr, Gender, Age)
NHANES_subset <- NHANES %>% select(ID, SurveyYr, Gender, Age) # alternative
NHANES_subset <- filter(NHANES_subset, row_number() %in% 1:200)
NHANES_subset <- NHANES %>% select(ID, SurveyYr, Gender, Age) %>% filter(row_number() %in% 1:200) # combined


# Deduplication

# Identify and remove duplicates
NHANES_unique <- NHANES %>% distinct()
NHANES_unique <- NHANES %>% distinct(ID, SurveyYr)

# Question
# Why are the resulting unique sets not of the same size?


# Rename variables

# Rename a variable - rename(NEW = OLD)
NHANES_new <- NHANES %>% 
  rename(Year = SurveyYr,
         Race = Race1)


#------


# Lecture
# Select, order, and remove

# Re-order variables
NHANES_subset <- NHANES %>% select(SurveyYr, ID, Age, everything())

# Select variables
NHANES_subset <- NHANES %>% select(ID, SurveyYr, Age, starts_with(c("BMI", "BP")), contains("Income"), last_col())

# Remove variables
NHANES_subset <- NHANES_subset %>% select(!"Gender")
NHANES_subset <- NHANES_subset %>% select(-c("BPSys3", "BPDia3"))


# Generate or modify variables - mutate

# Calculate BMI
NHANES_new <- NHANES %>%
  mutate(BMI_new = Weight / ((Height/100)^2)) %>% 
  select(ID, Weight, Height, BMI, BMI_new)

# Calculate high income
NHANES_new <- NHANES %>%
  mutate(BMI_new = Weight / ((Height/100)^2)) %>% 
  mutate(HighIncome = if_else(HHIncomeMid > 75000, 1, 0)) %>% 
  select(ID, Age, Weight, Height, BMI, BMI_new, HHIncomeMid, HighIncome)
  
# Modify high income
NHANES_new <- NHANES_new %>%
  mutate(HighIncome = if_else(HHIncomeMid > 50000, 1, 0)) 


# Convert and re-code - mutate

# Convert format
NHANES_new <- NHANES_new %>%
  mutate (ID = as.character(ID),
          HighIncome = as.factor(HighIncome))

# Recode variables – recode(…, OLD = NEW)
NHANES_new <- NHANES_new %>%
  mutate(HighIncome = recode(HighIncome, "0" = "No", "1" = "Yes"))


# Categorize – case_when()

# Generate age in 20-year bands
NHANES_new <- NHANES_new %>%
  mutate(AgeBand = case_when(
    Age < 20 ~ "0-19",
    Age < 40 ~ "20-39",
    Age < 60 ~ "40-59",
    Age < 80 ~ "60-79",
    Age >= 80 ~ "80+"
  ))
table(NHANES_new$AgeBand)


#------


# Filter missing values and subgroups

# Missing values
is.na(NHANES$BMI)
sum(is.na(NHANES$BMI))

# Filter out missing values
NHANES_subset <- NHANES %>%
  filter(!is.na(Education))
NHANES_subset <- NHANES %>%
  drop_na(Education, HHIncome)

# Filter subgroups
NHANES_subset <- NHANES %>%
  filter(Gender == "female")


# Sort data

# Ascending
NHANES_new <- NHANES_new %>%
  arrange(Weight)

# Descending
NHANES_new <- NHANES_new %>%
  arrange(desc(Weight))

# Nested
NHANES_new <- NHANES_new %>%
  arrange(desc(AgeBand), Weight)


#------


# Summary table

library(gtsummary)
table1 <- NHANES_new %>% 
  tbl_summary(include = c(Age, AgeBand, BMI, HighIncome))
table1

# by HighIncome
table1 <- NHANES_new %>% 
  tbl_summary(include = c(Age, AgeBand, BMI), by = HighIncome) %>% 
  add_p()
table1 

# save as HTML
library(gt)
gtsave(as_gt(table1), filename = "Table1.html")


#------














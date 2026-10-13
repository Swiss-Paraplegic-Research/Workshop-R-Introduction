
###################################

# Workshop R-Introduction - Lecture

# Date: 12 October 20216

###################################


rm(list = ls())


##################################
# Data transformation using Base-R
##################################


# Task
# Load and inspect data
library(NHANES)
data(NHANES)
?NHANES


#------


# Lecture
# Review dataframe
class(NHANES)
names(NHANES)
head(NHANES)
str(NHANES)
summary(NHANES)


# Question
# How many variables and entries are there?
# What variable formats do exist?


#------
 
 
# Lecture
# Select variable
weight <- NHANES$Weight
weight <- NHANES[, "Weight"]
weight <- NHANES[, 18]


# Select subset
NHANES_subset <- NHANES[1:200, ]
NHANES_subset <- NHANES_subset[, c("ID", "SurveyYr", "Gender", "Age")]
NHANES_subset <- NHANES[1:300, c("ID", "SurveyYr", "Gender", "Age")]


# Duplicates
sum(duplicated(NHANES))
NHANES_unique <- NHANES[!duplicated(NHANES), ]
NHANES_duplicates <- NHANES[duplicated(NHANES), ]


# Question
# What issues can lead to duplicates?


# Rename the variable
names(NHANES_subset)[names(NHANES_subset) == "SurveyYr"] <- "Year"


# Label the variable
attr(NHANES_subset$Year, "label") <- "Year of survey"


# Calculate BMI
NHANES$BMI_new <- NHANES$Weight / ((NHANES$Height/100)^2)
NHANES_subset <- NHANES[, c("ID", "Weight", "Height", "BMI", "BMI_new")]


# Calculate high income
summary(NHANES$HHIncomeMid)
NHANES$HighIncome <- ifelse(NHANES$HHIncomeMid > 75000, 1, 0)
NHANES_subset <- NHANES[, c("ID", "Weight", "Height", "BMI", "BMI_new", "HHIncomeMid", "HighIncome")]


#------


# Lecture
# Convert format
NHANES_subset$ID <- as.character(NHANES_subset$ID)
NHANES_subset$Height <- as.integer(NHANES_subset$Height)
NHANES_subset$HI_factor <- as.factor(NHANES_subset$HighIncome)
NHANES_subset$HighIncome <- factor(ifelse(NHANES_subset$HighIncome == 1, "Yes", "No"))


# Factor variables
levels(NHANES$Gender)
str(NHANES$Gender)
summary(NHANES$Gender)


# re-order
NHANES$Gender <- factor(NHANES$Gender, levels = c("male", "female"))
summary(NHANES$Gender)


# re-label
levels(NHANES$Gender) <- c("M", "F")
summary(NHANES$Gender)


# add a category 'diverse'
NHANES$Gender <- factor(NHANES$Gender, levels = c(levels(NHANES$Gender), "D"))
summary(NHANES$Gender)


#------


# Lecture
# Dates
Sys.Date()
Sys.time()


NHANES_subset <- NHANES[, c("ID", "SurveyYr", "Gender", "Age")]
NHANES_subset$SurveyYr <- as.character(NHANES_subset$SurveyYr)
NHANES_subset$Survey_date <- paste0(NHANES_subset$SurveyYr, "-01")
NHANES_subset$Survey_date <- as.Date(NHANES_subset$Survey_date, format="%Y_%m-%d")


#------


# Lecture
# lapply
result <- lapply(NHANES[,c("Age", "Weight", "Pulse")], function(x) mean(x, na.rm = TRUE))
result
par(mfrow = c(1, 3))
lapply(NHANES[,c("Age", "Weight", "Pulse")], hist)


# for loop
for (number in 1:5) {
  print(number)
}
variables <- c("Age", "Weight", "Pulse")
for (var in variables) {
  result[[var]] <- mean(NHANES[[var]], na.rm = TRUE)
}
result


# by
by(NHANES$Age, NHANES$Gender, mean)
by(NHANES$Weight, NHANES$Gender, mean)
by(NHANES$Weight, NHANES$Gender, function(x) mean(x, na.rm = TRUE))


# Task
# Calculate median age for each education level


# aggregate
df <- aggregate(NHANES$Age, list(NHANES$Gender), mean)
df
df <- aggregate(Age ~ Gender, data = NHANES, mean)
df


# ifelse
result <- lapply(NHANES[c("Age","Gender","Weight","Pulse")], function(x) {
  if (is.numeric(x) | is.integer(x)) {
    mean(x, na.rm = TRUE)
  } else {
    prop.table(table(x))
  }
})
result


#------


# Solution
by(NHANES$Age, NHANES$Education, median)












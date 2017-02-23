####Step 0 : Load the data in RStudio
my_data = read.csv("titanic_original.csv")
library(dplyr)
library(tidyr)
my_data <- tbl_df(my_data)

####Step 1: Port of embarkation
my_data$embarked[is.na(my_data$embarked)] <- "S"

###Step 2: Calculate Age
my_data %>% summarise(Min = min(age, na.rm=TRUE),
                      Median = median(age, na.rm=TRUE),
                      Mean = mean(age, na.rm=TRUE),
                      Max = max(age, na.rm=TRUE))

age_mean <- mean(my_data$age, na.rm = TRUE)
my_data$age[is.na(my_data$age)] <- age_mean

####Step 3: Lifeboat
my_data$boat <- as.character(my_data$boat)
my_data$boat[is.na(my_data$boat)] <- "NA"

###Step 4 : Cabin
my_data <- my_data %>% mutate(has_cabin_number = ifelse(!is.na(cabin), 1, 0))

####Step 5 : Write into file
write.csv(my_data, "titanic_clean.csv", row.names=FALSE)

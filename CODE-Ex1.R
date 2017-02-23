library(tidyr)
library(dplyr)

###### Step0 Load the data in RStudio
my_data <- read.csv("refine_original.csv")
my_data <- tbl_df(my_data)

#####Step1 Clean up brand names
clean_my_data <- my_data %>% arrange(company) %>% mutate_each(funs(tolower), matches("company"))
clean_my_data[1:7, 1] = "akzo"
clean_my_data[8:16, 1] = "philips"
clean_my_data[17:20, 1] = "unilever"
clean_my_data[21:25, 1] = "van houten"

####Step 2 Separate product code and number
clean_my_data <- separate(clean_my_data, "Product.code...number", c("product_code", "product_number"), sep = "-")

####Step 3 Add product categories
product <- c("Smartphone", "TV", "Laptop", "Tablet")
product_code <- c("p", "v", "x", "q")
product_df <- data.frame(product, product_code)
clean_my_data <- left_join(clean_my_data, product_df, by="product_code")

###Step 4 Add full address for geocoding
clean_my_data <- unite_(clean_my_data, "full_address", c("address","city","country"), sep=", ", remove = FALSE)

###Step 5 Create dummy variables for company and product category
dummy_data <- clean_my_data %>%   mutate(company_akzo = ifelse(company == "akzo", 1, 0), company_philips = ifelse(company == "philips", 1,0), company_unilever = ifelse(company == "unilever",1,0), company_van_houten = ifelse(company == "van houten", 1,0)) %>% 
mutate(product_smartphone = ifelse(product_code == "Smartphone",1,0), product_tablet = ifelse(product_code == "Tablet",1,0), product_tv = ifelse(product_code == "TV",1,0), product_laptop = ifelse(product_code == "Laptop",1,0))

###Generate final refine_clean.csv 
write.csv(dummy_data, file = "refine_clean.csv")

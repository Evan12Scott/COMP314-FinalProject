library(dplyr)
library(tidyverse)
library(sqldf)

departments <- read.csv('departments.csv')
aisles <- read.csv('aisles.csv')
order_products_train <- read.csv('order_products__train.csv')
orders <- read.csv('orders.csv')
products <- read.csv('products.csv')

######################
###### Cleaning ######
######################

# Check and handle NAs in tables
sum(is.na(departments))
sum(is.na(aisles))
sum(is.na(order_products_prior))
sum(is.na(order_products_train))
sum(is.na(orders))
sum(is.na(products))

# Cannot confirm the NA values in 'days_since_prior_order' column indicate a customer's
# first time ordering thus we drop those records to avoid false/incorrect imputations since
# < 5% of total records in this table
orders_cleaned <- na.omit(orders)
sum(is.na(orders_cleaned))

# Remove eval_set column because not relevant
orders_cleaned <- orders_cleaned %>%
  select(-eval_set)
str(orders_cleaned)

# Table structure and merge aisles and departments tables into the tables that reference them to reduce table count
str(aisles)
str(departments)
str(orders_cleaned)
str(products)
str(order_products_train)

# Products is the only table which references aisles and departments so merge aisles and department tables into it
products_cleaned <- products %>%
  left_join(departments, by = "department_id") %>%
  left_join(aisles, by = "aisle_id") %>%
  select(-department_id, -aisle_id)

str(products_cleaned) # No longer need to use aisles or departments tables as both referenced now in products

# Add columns for time frames (day of the week and hour of the day)
orders_cleaned <- orders_cleaned %>% mutate(
  order_day = case_when(
    order_dow == 0 ~ 'Sunday',
    order_dow == 1 ~ 'Monday',
    order_dow == 2 ~ 'Tuesday',
    order_dow == 3 ~ 'Wednesday',
    order_dow == 4 ~ 'Thursday',
    order_dow == 5 ~ 'Friday',
    order_dow == 6 ~ 'Saturday'
  ),
  order_hour = case_when(
    order_hour_of_day ==  0 ~ '12AM',
    order_hour_of_day == 12 ~ '12PM',
    order_hour_of_day < 12 ~ paste0(order_hour_of_day, 'AM'),
    order_hour_of_day > 12 ~ paste0(order_hour_of_day - 12, 'PM')
  )
)

#######################################
###### Queries and Visualization ######
#######################################

# Visualization 1

# Visualization 2

# Visualization 3

# Visualization 4
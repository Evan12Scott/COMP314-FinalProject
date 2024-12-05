library(dplyr)
library(tidyverse)
# Evan
departments <- read.csv('departments.csv')
aisles <- read.csv('aisles.csv')
order_products_train <- read.csv('order_products__train.csv')
orders <- read.csv('orders.csv')
products <- read.csv('products.csv')

# Cleaning


# Check and handle NAs in tables
sum(is.na(departments))
sum(is.na(aisles))
sum(is.na(order_products_prior))
sum(is.na(order_products_train))
sum(is.na(orders))
sum(is.na(products))

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


# Load necessary libraries
library(dplyr)
library(tidyverse)
library(sqldf)
library(ggcorrplot)
library(kableExtra)
library(ggplot2)

# Load in the csv files 
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
sum(is.na(order_products_train))
sum(is.na(orders))
sum(is.na(products))

# proportion of records in orders that have NAs
sum(is.na(orders$days_since_prior_order)) / nrow(orders)

# Cannot confirm the NA values in 'days_since_prior_order' column indicate a customer's
# first time ordering thus we drop those records to avoid false/incorrect imputations since
# < 5% of total records in this table
orders_cleaned <- na.omit(orders)
sum(is.na(orders_cleaned))

# Remove eval_set column because not relevant
orders_cleaned <- sqldf('
                        SELECT order_id, user_id, order_number, order_dow, 
                        order_hour_of_day, days_since_prior_order
                        FROM orders_cleaned
                        WHERE eval_set IS NOT NULL'
                        )

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

str(orders_cleaned)

# Table structure and merge aisles and departments tables into the tables that reference them to reduce table count
str(aisles)
str(departments)
str(orders_cleaned)
str(products)
str(order_products_train)

# Products is the only table which references aisles and departments so merge aisles
# and department tables into it and drop unnecessary aisle_id and department_id attributes
products_cleaned <- sqldf('SELECT p.*, a.aisle, d.department
                          FROM products p
                          LEFT JOIN departments d ON p.department_id = d.department_id
                          LEFT JOIN aisles a ON p.aisle_id = a.aisle_id'
                          )

str(products_cleaned) # No longer need to use aisles or departments tables as both referenced now in products

# Tables to be used in visualizations and queries:
#   - orders_cleaned
#   - products_cleaned
#   - order_products_train

#######################################
###### Queries and Visualization ######
#######################################

# Visualization 1: Table

summary_orders <- sqldf('
                        SELECT COUNT(*) AS total_orders,
                        AVG(days_since_prior_order) AS avg_days_between_orders,
                        COUNT(DISTINCT user_id) AS total_customers
                        FROM orders_cleaned'
                        )

avg_order_size <- sqldf('
                        SELECT AVG(order_size) AS avg_order_size
                        FROM (
                          SELECT order_id, COUNT(*) AS order_size
                          FROM order_products_train
                          GROUP BY order_id
                          )'
                        )

avg_order_size_val <- avg_order_size$avg_order_size

summary_products <- sqldf('SELECT COUNT(*) AS total_products,
                          COUNT(DISTINCT department) AS total_departments,
                          COUNT(DISTINCT aisle) AS total_aisles
                          FROM products_cleaned'
                          )

# Combine important metrics/aggregations from the summary statistics of the different tables into one structure
summary_table <- tibble(
  Metric = c('Orders', 'Days Between Orders (Avg)', 'Customers', 'Order Size (Avg)', 
             'Products', 'Departments', 'Aisles'),
  Value = c(
    summary_orders$total_orders, 
    round(summary_orders$avg_days_between_orders, 2), 
    summary_orders$total_customers, 
    round(avg_order_size, 2), 
    summary_products$total_products, 
    summary_products$total_departments, 
    summary_products$total_aisles
  )
)

# Pretty print a table visualization
summary_table %>%
  kable() %>%
  kable_styling(bootstrap_options = 'striped', full_width = FALSE)


# Visualization 2: Correlation Matrix

# extract the total products that associated with a given order along with how many
# of those products are unique
customer_order_sum <- sqldf('
                            SELECT order_id,
                            COUNT(*) AS total_product_per_order,
                            COUNT(DISTINCT product_id) AS unique_product_per_order
                            FROM order_products_train
                            GROUP BY order_id'
                            )

# Combine numeric columns from orders_cleaned and order_products_train to visualize
# any linear relationships via correlation
corr_numeric_data <- sqldf('
                           SELECT o.order_number, o.order_dow, o.order_hour_of_day, 
                           o.days_since_prior_order, s.total_product_per_order, s.unique_product_per_order
                           FROM orders_cleaned o
                           INNER JOIN customer_order_sum s ON o.order_id = s.order_id'
                           )

corr_matrix <- cor(corr_numeric_data)

ggcorrplot(corr_matrix, lab = TRUE, lab_size = 3, title = 'Correlation Matrix for Order Features',
           colors = c('blue', 'white','red'))


# Visualization 3: Boxplot

# Get the order size by counting the number of products per order
# Join the data together for analysis in the boxplot between order sizes and time gaps between orders
boxplot_data <- sqldf("
                      SELECT o.user_id, o.order_number, o.days_since_prior_order, 
                      CASE 
                        WHEN s.order_size BETWEEN 0 AND 5 THEN '0-5'
                        WHEN s.order_size BETWEEN 6 AND 10 THEN '6-10'
                        WHEN s.order_size BETWEEN 11 AND 15 THEN '11-15'
                        WHEN s.order_size BETWEEN 16 AND 20 THEN '16-20'
                        WHEN s.order_size BETWEEN 21 AND 30 THEN '21-30'
                        WHEN s.order_size BETWEEN 31 AND 40 THEN '31-40'
                        WHEN s.order_size BETWEEN 41 AND 50 THEN '41-50'
                        WHEN s.order_size BETWEEN 51 AND 60 THEN '51-60'
                        WHEN s.order_size BETWEEN 61 AND 70 THEN '61-70'
                        WHEN s.order_size BETWEEN 71 AND 80 THEN '71-80'
                        ELSE '80+'
                      END AS order_size_in_buckets
                      FROM orders_cleaned o
                      JOIN (
                        SELECT order_id, COUNT(product_id) AS order_size
                        FROM order_products_train
                        GROUP BY order_id
                      ) s ON o.order_id = s.order_id"
                      )
# Order the buckets for ggplot 
boxplot_data$order_size_in_buckets <- factor(
  boxplot_data$order_size_in_buckets,
  levels = c('0-5', '6-10', '11-15', '16-20', '21-30', 
             '31-40', '41-50', '51-60', '61-70', '71-80', '80+'),
  ordered = TRUE
)

ggplot(boxplot_data, aes(x = factor(order_size_in_buckets), y = days_since_prior_order)) +
  geom_boxplot(fill = 'lightblue') +
  labs(title = 'Order Size vs Days Between Orders',
       x = 'Order Size',
       y = 'Days Since Prior Order')


# Visualization 4: Stacked Histogram
combined_df <- sqldf('
               SELECT o.add_to_cart_order, p.department
               FROM order_products_train o
               LEFT JOIN products_cleaned p ON o.product_id = p.product_id')

# Calculate the proportion departments are added to a customer's cart at specific spots
stacked_hist_data <- sqldf('
               SELECT add_to_cart_order, department, COUNT(*) AS count,
               COUNT(*) * 1.0 / SUM(COUNT(*)) OVER (PARTITION BY add_to_cart_order) as proportion
               FROM combined_df
               GROUP BY add_to_cart_order, department
               ORDER BY add_to_cart_order, department'
               )

ggplot(stacked_hist_data, aes(x = add_to_cart_order, y = proportion, fill = factor(department))) +
  geom_bar(stat = 'identity') + 
  labs(title = 'Aisle Add-To-Cart Order Spread',
       x = 'Add To Cart Order',
       y = 'Proportion') + 
  scale_fill_discrete(name = 'Department')

# Visualization 5: Violin








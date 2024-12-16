# Load necessary libraries
library(dplyr)
library(tidyverse)
library(sqldf)
library(ggcorrplot)
library(kableExtra)
library(ggplot2)
library(treemapify)
library(reshape2)

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
  labs(title = 'Department Add-To-Cart Order Spread',
       x = 'Add To Cart Order',
       y = 'Proportion') + 
  scale_fill_discrete(name = 'Department')

# Visualization 5: Violin

violin_data <- sqldf('
  SELECT o.order_day, COUNT(p.product_id) AS num_products
  FROM orders_cleaned o
  JOIN order_products_train p ON o.order_id = p.order_id
  GROUP BY o.order_day
')

# Create a violin plot
ggplot(violin_data, aes(x = factor(order_day, levels = 1:7), y = num_products)) +
  geom_violin(trim = FALSE, fill = "skyblue", alpha = 0.7) +  # Violin plot
  labs(title = "Distribution of Products Ordered Over Days of the Week", x = "Day of the Week", y = "Number of Products Ordered") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization 6: Heatmap

# Aggregate products that occur in together in an order
co_occurrence <- sqldf('
  SELECT p1.product_id AS product_id_1, 
         p2.product_id AS product_id_2, 
         COUNT(*) AS co_occurrence
  FROM order_products_train p1
  JOIN order_products_train p2 ON p1.order_id = p2.order_id AND p1.product_id < p2.product_id
  GROUP BY p1.product_id, p2.product_id
')

# Filter for top 20 co-occurrences
top_co_occurrence <- co_occurrence[order(-co_occurrence$co_occurrence), ][1:20, ]

# Join to get the product names for plot visualization interpretability
top_co_occurrence <- sqldf('
  SELECT c.*, p1.product_name AS product_name_1, p2.product_name AS product_name_2
  FROM top_co_occurrence c
  JOIN products_cleaned p1 ON c.product_id_1 = p1.product_id
  JOIN products_cleaned p2 ON c.product_id_2 = p2.product_id
')

# Reshape data for the heatmap
co_occurrence_matrix <- reshape2::acast(top_co_occurrence, product_name_1 ~ product_name_2, value.var = "co_occurrence", fill = 0)

ggplot(as.data.frame(as.table(co_occurrence_matrix)), aes(x = Var2, y = Var1, fill = log(Freq + 1))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red", name = "Co-Occurrence Strength") +
  labs(
    title = "Top 20 Product Co-Occurrences",
    x = "Product 2",
    y = "Product 1"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )


# Visualization 7: Pie Chart

pie_data <- sqldf('
                  SELECT p.department_id, p.department, COUNT(o.product_id) AS products_sold
                  FROM order_products_train o
                  JOIN products_cleaned p ON o.product_id = p.product_id
                  GROUP BY p.department_id
                  ORDER BY products_sold DESC'
                  )

# Calculate department popularity proportion against all departments
pie_data$proportion <- pie_data$products_sold / sum(pie_data$products_sold)

ggplot(pie_data, aes(x = "", y = proportion, fill = department)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  labs(
    title = "Products Sold by Department",
    x = NULL,
    y = NULL,
    fill = "Department"
  ) +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  )

# Visualization 8: Stacked Bar Chart

# Aggregate by department and reorder status to calculate proportion
barchart_data <- sqldf('
  SELECT p.department,
         o.reordered,
         COUNT(*) AS product_count,
         COUNT(*) * 1.0 / SUM(COUNT(*)) OVER (PARTITION BY p.department) AS proportion
  FROM order_products_train o
  JOIN products_cleaned p ON o.product_id = p.product_id
  GROUP BY p.department, o.reordered
')

ggplot(barchart_data, aes(x = reorder(department, -proportion * (reordered == 1)), y = proportion, fill = factor(reordered))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("0" = "grey", "1" = "darkblue"), labels = c("Not Reordered", "Reordered")) +
  labs(
    title = "Reordered Products Across Departments",
    x = "Department",
    y = "Proportion",
    fill = "Reorder Status"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Visualization 9: Scatterplot

hourly_sales <- "SELECT 
    order_hour_of_day, 
    COUNT(DISTINCT user_id) AS unique_users
  FROM 
    orders_cleaned
  GROUP BY 
    order_hour_of_day
  ORDER BY 
    order_hour_of_day"
sqldf(hourly_sales)


#ggplot2 Scatterplot visualization
#Finding amount of sales per hour each day
hourly_orders_dow <- orders_cleaned %>%
  group_by(order_hour_of_day, order_dow) %>%
  summarise(unique_users = n_distinct(user_id)) %>%
  mutate(weekend = ifelse(order_dow %in% c(0, 6), "Weekend", "Weekday"))

ggplot(hourly_orders_dow, aes(x = order_hour_of_day, y = unique_users)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +
  geom_smooth(method = "loess", color = "cyan", se = FALSE) +  #Smoothed trend line
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24),
    labels = c("12 AM", "6 AM", "12 PM", "6 PM", "12 AM")
  ) +
  labs(
    title = "Number of Sales Each Hour",
    x = "Hour of the Day",
    y = "Number of Unique Users"
  ) +
  theme_minimal()

# Visualization 10: Line Chart

top_hours_by_day <- "SELECT day_of_week, order_hour_of_day, unique_customers
  FROM (
    SELECT 
      CASE order_dow
        WHEN 0 THEN 'Sunday'
        WHEN 1 THEN 'Monday'
        WHEN 2 THEN 'Tuesday'
        WHEN 3 THEN 'Wednesday'
        WHEN 4 THEN 'Thursday'
        WHEN 5 THEN 'Friday'
        WHEN 6 THEN 'Saturday'
      END AS day_of_week,
      order_hour_of_day,
      COUNT(DISTINCT user_id) AS unique_customers,
      ROW_NUMBER() OVER (PARTITION BY order_dow ORDER BY COUNT(DISTINCT user_id) DESC) AS rank
    FROM 
      orders_cleaned
    GROUP BY 
      order_dow, order_hour_of_day
  ) AS ranked_hours
  WHERE rank <= 2"
sqldf(top_hours_by_day)

#Finding amount of customers per hour each day
hourly_customers <- orders_cleaned %>% 
  group_by(order_dow, order_hour_of_day) %>%
  summarise(unique_users = n_distinct(user_id))

#Line Chart creation
ggplot(hourly_customers, aes(x = order_hour_of_day, y = unique_users, color = factor(order_dow))) +
  #Add transparent shaded areas
  geom_rect(aes(xmin = 6, xmax = 12, ymin = -Inf, ymax = Inf, fill = "Morning"), alpha = 0.1, inherit.aes = FALSE) +  #Morning
  geom_rect(aes(xmin = 12, xmax = 18, ymin = -Inf, ymax = Inf, fill = "Afternoon"), alpha = 0.1, inherit.aes = FALSE) +  #Afternoon
  geom_line(size = 1.2) +  # Customer activity lines
  scale_fill_manual(
    name = "Time of Day",
    values = c("Morning" = "lightblue", "Afternoon" = "lightyellow")
  ) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18, 24),
    labels = c("12 AM", "6 AM", "12 PM", "6 PM", "12 AM")
  ) +
  labs(
    title = "Day-to-Day Customer Activity",
    x = "Hour of the Day",
    y = "Number of Customers",
    color = "Day of the Week"
  ) +
  scale_color_brewer(palette = "Set1", labels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12))

# Visualization 11: Histogram

top_item_per_department <- "SELECT department, product_name
FROM (
  SELECT 
  p.department, 
  p.product_name, 
  COUNT(o.product_id) AS total_orders,
  ROW_NUMBER() OVER (PARTITION BY p.department ORDER BY COUNT(o.product_id) DESC) AS rank
  FROM 
  order_products_train o
  JOIN 
  products_cleaned p 
  ON 
  o.product_id = p.product_id
  GROUP BY 
  p.department, p.product_name
) AS subquery
WHERE rank = 1"
sqldf(top_item_per_department)

#ggplot2 histogram visualization
top_items_by_department <- order_products_train %>%
  group_by(product_id) %>%
  summarise(total_orders = n()) %>%
  left_join(products_cleaned, by = "product_id") %>%
  group_by(department) %>%
  arrange(department, desc(total_orders)) %>%
  slice_head(n = 10) %>%  #top 10 products per department
  ungroup()
#facet-wrapped plot by department, showing most sold items by department
ggplot(top_items_by_department, aes(x = reorder(product_name, total_orders), y = total_orders)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  facet_wrap(~ department, scales = "free_y") +  # Allow independent x-axis scales
  labs(
    title = "Top 10 Selling Products in Each Department",
    x = "Product Name",
    y = "Number of Orders"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6),   # Adjust product name text size
    axis.text.x = element_text(size = 8),   # Adjust x-axis tick label size
    strip.text = element_text(face = "bold", size = 10)  # Department labels styling
  )

# Visualization 12: Treemap

# create new table to compare sales in each department
department_sales <- products_cleaned %>% 
  left_join(order_products_train, by = "product_id") %>%
  count(department)
sales_by_department <- "SELECT department, n
      FROM department_sales
      ORDER BY n DESC"
sqldf(sales_by_department)

#treemap creation
ggplot(department_sales, aes(area = n, fill = department, label = department)) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", color = "white", place = "centre", grow = TRUE) +
  labs(title = "Department Sales") +
  theme_minimal()


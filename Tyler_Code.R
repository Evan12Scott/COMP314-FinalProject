library(sqldf)
library(tidyverse)
# Ty
departments <- read.csv('departments.csv')
aisles <- read.csv('aisles.csv')
order_products_train <- read.csv('order_products__train.csv')
orders <- read.csv('orders.csv')
products <- read.csv('products.csv')
orders_cleaned <- na.omit(orders)
products_cleaned <- products %>%
  left_join(departments, by = "department_id") %>%
  left_join(aisles, by = "aisle_id") %>%
  select(-department_id, -aisle_id)

#sqldf query to see how many sales by the hour (scatterplot)

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
hourly_orders_dow <- order_data %>%
  group_by(order_hour_of_day, order_dow) %>%
  summarise(unique_users = n_distinct(user_id)) %>%
  mutate(weekend = ifelse(order_dow %in% c(0, 6), "Weekend", "Weekday"))

ggplot(hourly_orders, aes(x = order_hour_of_day, y = unique_users)) +
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





# Query to find the top 2 hours(for sake of output length) with the most unique customers for each day of the week (Line Chart)
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




# SQL query to find the top product per department (Histogram)
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




#SQL query 11 to find each departments sales (Treemap)
# create new table to compare sales in each department
department_sales <- products_cleaned %>% 
  left_join(order_products_train, by = "product_id") %>%
  count(department)
sales_by_department <- "SELECT department, n
      FROM department_sales
      ORDER BY n DESC"
sqldf(sales_by_department)


#ggplot2 Treemap creation
library(treemapify)

#treemap creation
ggplot(department_sales, aes(area = n, fill = department, label = department)) +
  geom_treemap() +
  geom_treemap_text(fontface = "bold", color = "white", place = "centre", grow = TRUE) +
  labs(title = "Department Sales") +
  theme_minimal()



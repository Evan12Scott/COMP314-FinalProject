# Ty
departments <- read.csv('departments.csv')
aisles <- read.csv('aisles.csv')
order_products_prior <- read.csv('order_products__prior.csv')
order_products_train <- read.csv('order_products__train.csv')
orders <- read.csv('orders.csv')
products <- read.csv('products.csv')

#Scatterplot creation
#Grouping by each hour and creating unique user_id to see combine every order
hourly_orders <- orders_cleaned %>%
  group_by(order_hour_of_day) %>%
  summarise(unique_users = n_distinct(user_id))

ggplot(hourly_orders, aes(x = order_hour_of_day, y = unique_users)) +
  geom_point(alpha = 0.7, color = "blue", size = 3) +
  geom_smooth(method = "loess", color = "cyan", se = FALSE) +
  scale_x_continuous(
    breaks = c(0, 6, 12, 18), # Set breaks at every 6 hours
    labels = c("12 AM","6 AM", "12 PM", "6 PM")) + 
  labs(title = "Purchases by the Hour",
       x = "Hour of the Day",
       y = "Number of Customers") +
  theme_minimal()


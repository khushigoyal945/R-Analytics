# Load libraries
library(dplyr)      
library(ggplot2)    
library(readr)      

# Load the Dataset
retail_store_inventory1 <-read.csv("C:/Users/khush/OneDrive/Desktop/retail_store_inventory.csv")
View(retail_store_inventory1)

#View first 10 rows
View(head(retail_store_inventory1,10))

#View last 10 rows
View(tail(retail_store_inventory1,10))

# view structure
str(data)

# Check for missing values
colSums(is.na(data))

# Summary statistics for numerical columns
summary_stats <-retail_store_inventory1 %>%
  summarise(
    Avg_Units_Sold = mean(`Units.Sold`, na.rm = TRUE),
    Median_Units_Sold = median(`Units.Sold`, na.rm = TRUE),
    Avg_Price = mean(Price, na.rm = TRUE),
    Avg_Discount = mean(Discount, na.rm = TRUE)
  )
print(summary_stats)
View(summary_stats)

#1. Sales by Category
category_sales <-retail_store_inventory1%>%
  group_by(Category) %>%
  summarise(Total_Units_Sold = sum(`Units.Sold`, na.rm = TRUE))
View(category_sales)
##2.
ggplot(category_sales, aes(x = Category, y = Total_Units_Sold, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Units Sold by Category",
       x = "Product Category",
       y = "Total Units Sold") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#3. Regional Sales Performance
region_sales <-retail_store_inventory1 %>%
  group_by(Region) %>%
  summarise(Total_Units_Sold = sum(`Units.Sold`, na.rm = TRUE))
View(region_sales)

##4.
ggplot(region_sales, aes(x = Region, y = Total_Units_Sold, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Units Sold by Region",
       x = "Region",
       y = "Total Units Sold") +
  theme_minimal()

#5. Inventory Level Analysis
inventory_analysis <-retail_store_inventory1 %>%
  group_by(Category) %>%
  summarise(Avg_Inventory = mean(`Inventory.Level`, na.rm = TRUE))
View(inventory_analysis)

##6.
ggplot(inventory_analysis, aes(x = Category, y = Avg_Inventory, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Inventory Level by Category",
       x = "Product Category",
       y = "Average Inventory Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##7.
ggplot(heatmap_data, aes(x = Region, y = Category, fill = Avg_Units_Sold)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Average Units Sold by Region and Category",
       x = "Region",
       y = "Product Category",
       fill = "Avg Units Sold") +
  theme_minimal()

#8.Time Trend of Units Sold (if Date is available)
retail_store_inventory1$Date <- as.Date(retail_store_inventory1$Date)  
time_trend <-retail_store_inventory1 %>%
  group_by(Date) %>%
  summarise(Total_Units_Sold = sum(`Units.Sold`, na.rm = TRUE))
View(time_trend)

#9. Top Performing Categories
top_categories <-retail_store_inventory1%>%
  group_by(Category) %>%
  summarise(Total_Units_Sold = sum(`Units.Sold`, na.rm = TRUE)) %>%
  arrange(desc(Total_Units_Sold)) 
View(head(top_categories))

##10.
ggplot(top_categories, aes(x = reorder(Category, Total_Units_Sold), y = Total_Units_Sold, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Performing Categories",
       x = "Category",
       y = "Total Units Sold") +
  coord_flip() +
  theme_minimal()

#11.Competitor Pricing vs Sales
ggplot(retail_store_inventory1, aes(x = `Competitor.Pricing`, y = `Units.Sold`)) +
  geom_point(color = "darkorange", alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Competitor Pricing vs Units Sold",
       x = "Competitor Pricing",
       y = "Units Sold") +
  theme_minimal()

##12.
ggplot(retail_store_inventory1, aes(y = Seasonality, x = Price, fill = Seasonality)) +
  geom_boxplot() +
  labs(title = "Box Plot:Price by Seasonality ",
       x = "Seasonality",
       y = "Price") +
  theme_minimal()

##13.
retail_store_inventory1$Date <- as.Date(retail_store_inventory1$Date)
time_trend <-retail_store_inventory1%>%
  group_by(Date) %>%
  summarise(Total_Units_Sold = sum(`Units.Sold`, na.rm = TRUE))

##14.
ggplot(time_trend, aes(x = Date, y = Total_Units_Sold)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Line Chart: Trend of Units Sold Over Time",
       x = "Date",
       y = "Total Units Sold") +
  theme_minimal()

##15.
ggplot(retail_store_inventory1, aes(x = Price, y = `Units.Sold`, color = Region)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Region) +
  labs(title = "Faceted Scatter Plot: Price vs Units Sold by Region",
       x = "Price",
       y = "Units Sold") +
  theme_minimal()

##16.
ggplot(retail_store_inventory1, aes(x = Price, fill = Category)) +
  geom_density(alpha = 0.6) +
  labs(title = "Density Plot: Price Distribution by Category",
       x = "Price",
       y = "Density") +
  theme_minimal()

#17. Prepare data for the pie chart
pie_data <- retail_store_inventory1 %>%
  group_by(Region) %>%
  summarise(Total_Units_Sold = sum(`Units.Sold`, na.rm = TRUE)) %>%
  mutate(Percentage = Total_Units_Sold / sum(Total_Units_Sold) * 100)
View(pie_data)

#18. Create the pie chart
ggplot(pie_data, aes(x = "", y = Percentage, fill = Region)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label=paste(round(Percentage,0),"%")),
            position=position_stack(vjust=0.5))+
  labs(title = "Pie Chart: Proportion of Units Sold by Region",
       x = NULL,
       y = NULL) +
  theme_void() +
  theme(legend.position = "right")

##19.
ggplot(retail_store_inventory1, aes(x = `Units.Sold`)) +
  geom_histogram(binwidth = 10, fill = "green", color = "black") +
  labs(title = "Histogram: Distribution of Units Sold",
       x = "Units Sold",
       y = "Frequency") +
  theme_minimal()

#20. Scatter Plot 4: Inventory Level vs Demand Forecast
ggplot(retail_store_inventory1, aes(x = `Inventory.Level`, y = `Demand.Forecast`)) +
  geom_point(color = "pink", alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Scatter Plot: Inventory Level vs Demand Forecast",
       x = "Inventory Level",
       y = "Demand Forecast") +
  theme_minimal()

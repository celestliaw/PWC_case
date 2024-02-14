install.packages("forecast")
library(forecast)
# read csv file with merged datasets
sales_data

#make date column suitable for analysis
sales_data$order_purchase_timestamp<- as.Date(sales_data$order_purchase_timestamp)
#aggregated the sales data by grouping it based on 'product_category_name_english' and 'order_purchase_timestamp,' summing up the 'order_item_id' for each group. 
sales_data_aggregated <- aggregate(order_item_id ~ product_category_name_english + order_purchase_timestamp, data = sales_data, sum)

#time series object creation
ts_data <- ts(sales_data_aggregated $order_item_id, frequency = 365.25)
#time series plotting
plot(ts_data, main = "Time Series of Product Categories", ylab = "Total Quantity", xlab = "Purchase Date")
#forecast model
forecast_model <- forecast(auto.arima(ts_data), h = 12)
#forecast plotting
plot(forecast_model, main = "Forecasting Product Categories", ylab = "Total Quantity", xlab = "Purchase Date")
forecast_values <- forecast_model$mean
#extracted the forecasted values and organized them into a dataframe ('forecast_data') with product categories and their associated forecasted quantities.
forecast_data <- data.frame(
  product_category_name_english = unique(sales_data_aggregated$product_category_name_english),
  forecast_values = forecast_values[1:length(unique(sales_data_aggregated$product_category_name_english))]
)

max_category <- forecast_data[which.max(forecast_data$forecast_values), ]
print(max_category)

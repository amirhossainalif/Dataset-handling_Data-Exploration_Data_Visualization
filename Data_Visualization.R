install.packages("ggplot2")
install.packages("e1071")
library(e1071)
library(ggplot2)


dataset <- read.csv("C:/Users/Amir Hossain Alif/Desktop/Data science/Project/Dataset-handleing_Data-Exploration_Data_Visualization/ecommerce_dataset_updated.csv")

hist(dataset$Final_Price.Rs., 
     main = "Histogram of Final Price", 
     xlab = "Final Price (Rs.)", 
     col = "lightblue", 
     border = "black")


ggplot(dataset, aes(x = Final_Price.Rs..)) +
  geom_histogram(aes(y = ..density..), binwidth = 50, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1) +
  labs(title = "Line Histogram of Final Price", x = "Final Price (Rs.)", y = "Density")


price_skew <- skewness(dataset$Final_Price.Rs.., na.rm = TRUE)
skew_direction <- ifelse(price_skew > 0, "Positive Skew", "Negative Skew")

ggplot(dataset, aes(x = Final_Price.Rs..)) +
  geom_freqpoly(binwidth = 50, color = "blue", size = 1) +
  labs(
    title = paste("Line Histogram of Final Price with", skew_direction),
    x = "Final Price (Rs.)",
    y = "Count"
  )


ggplot(dataset, aes(x = Price..Rs.., y = Final_Price.Rs..)) +
  geom_point(color = "blue", alpha = 0.7) +
  labs(title = "Scatter Plot of (Price vs Final Price)", x = "Price (Rs.)", y = "Final Price (Rs.)")


ggplot(dataset, aes(x = Category, y = Final_Price.Rs.., fill = Category)) +
  geom_violin(trim = TRUE) +
  labs(title = "Violin Plot of Final Price by Category", x = "Category", y = "Final Price (Rs.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


dataset$Purchase_Date <- as.Date(dataset$Purchase_Date, format = "%d-%m-%Y")

ggplot(dataset, aes(x = Purchase_Date, y = Final_Price.Rs..)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Line Graph of Final Price Over Time", x = "Purchase Date", y = "Final Price (Rs.)")


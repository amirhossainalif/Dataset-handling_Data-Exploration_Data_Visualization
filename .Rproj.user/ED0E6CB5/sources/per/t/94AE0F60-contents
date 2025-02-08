
dataset <- read.csv("C:/Users/Amir Hossain Alif/Desktop/Data science/Project/Dataset-handleing_Data-Exploration_Data_Visualization/ecommerce_dataset_updated.csv")

Pearson_correlation <- cor(dataset$Price..Rs., dataset$Final_Price.Rs., method = "pearson")
print(Pearson_correlation)

Spearman_correlation <- cor(dataset$Price..Rs., dataset$Final_Price.Rs., method = "spearman")
print(Spearman_correlation)

Kendall_correlation <- cor(dataset$Price..Rs., dataset$Final_Price.Rs., method = "kendall")
print(Kendall_correlation)


dataset$Category <- as.factor(dataset$Category)
dataset$Final_Price.Rs. <- as.numeric(dataset$Final_Price.Rs.)

anova_result <- aov(Final_Price.Rs. ~ Category, data = dataset)
summary(anova_result)

table_data <- table(dataset$Category, dataset$Payment_Method)
chi_square_result <- chisq.test(table_data)
print(chi_square_result)


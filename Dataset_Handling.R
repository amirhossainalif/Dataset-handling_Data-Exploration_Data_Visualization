install.packages("dplyr")
install.packages("visdat")
install.packages("ggplot2")
library(dplyr)
library(visdat)
library(ggplot2)

mydata<-read.csv("C:/Users/Amir Hossain Alif/Desktop/Data science/Project/Dataset-handleing_Data-Exploration_Data_Visualization/Dataset1.csv", header = TRUE, sep = ",")
mydata

colSums(is.na(mydata))
rowSums(is.na(mydata))
which(is.na(mydata$Age))
is.na(mydata)


remove_data <- na.omit(mydata)
remove_data


mydata$Study.Hours[is.na(mydata$Study.Hours)] <- median(mydata$Study.Hours, na.rm = TRUE)
colSums(is.na(mydata))


vis_dat(mydata)
vis_miss(mydata)
vis_guess(mydata)


set.seed(42) 
minority_class <- mydata %>% filter(Gender == "Female") 
majority_class <- mydata %>% filter(Gender == "Male") 

majority_sample <- majority_class %>% sample_n(nrow(minority_class))

balanced_data_undersampling <- rbind(majority_sample, minority_class)
table(balanced_data_undersampling$Gender)


set.seed(42) 
oversampled_minority <- minority_class %>% sample_n(nrow(majority_class), replace = TRUE)

balanced_data_oversampling <- rbind(majority_class, oversampled_minority)

table(balanced_data_oversampling$Gender)


distinct(mydata)
u_data <- unique(mydata[, c("Gender", "Age","Academic.Pressure","Study.Satisfaction","Sleep.Duration","Dietary.Habits","Have.you.ever.had.suicidal.thoughts..","Study.Hours","Financial.Stress","Family.History.of.Mental.Illness","Depression")])
dim(mydata) 
dim(u_data)


filter_data <- data.frame(mydata$Gender == "Male")
filter(filter_data)


mydata$Family.History.of.Mental.Illness_values <- factor(mydata$Family.History.of.Mental.Illness,
                                                         levels = c("No","Yes"),
                                                         labels = c(0,1))
str(mydata$Family.History.of.Mental.Illness_values)  
table(mydata$Family.History.of.Mental.Illness)  
table(mydata$Family.History.of.Mental.Illness_values)


Study.Hours_normalized <- (mydata$Study.Hours - min(mydata$Study.Hours)) / (max(mydata$Study.Hours) - min(mydata$Study.Hours))
summary(Study.Hours_normalized)
hist(Study.Hours_normalized) 


ggplot(mydata, aes(x = Age, y = 1)) +
  geom_jitter(height = 0.1, color = "blue", alpha = 0.7, size = 2) +
  theme_minimal() +
  labs(
    title = "Dot Plot of Age Showing Concentrations",
    x = "Age",
    y = ""
  ) +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank())

Q1 <- quantile(mydata$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(mydata$Age, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

mydata$Age[mydata$Age < lower_bound | mydata$Age > upper_bound] <- median(mydata$Age, na.rm = TRUE)

summary(mydata$Age)
boxplot(mydata$Age)


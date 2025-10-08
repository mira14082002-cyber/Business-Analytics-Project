install.packages("ggplot2")
library(ggplot2)
install.packages("VIM",dependencies = T)
library("VIM")
install.packages("corrgram")
library("corrgram")
install.packages("mice")
library("mice")
install.packages('MCDA')
library('MCDA')


### PART 1


## Data Understanding 
# data collection
data = read.csv("order_july24.csv", header=TRUE)
dim(data)
colnames(data)
head(data)
test_data = read.csv("new_customer24.csv", header=TRUE)
dim(test_data)
colnames(test_data)
head(test_data)


## Data Exploration: descriptive statistics
summary(data[c(1,2,4,6)])
#spend
mean(data$spend, na.rm=TRUE)
median(data$spend, na.rm=TRUE)
names(sort(-table(data$spend)))[1]
max(data$spend, na.rm=TRUE) - min(data$spend, na.rm=TRUE)
var(data$spend, na.rm=TRUE)
sd(data$spend, na.rm=TRUE)
#past_spend
mean(data$past_spend, na.rm=TRUE)
median(data$past_spend, na.rm=TRUE)
names(sort(-table(data$past_spend)))[1]
max(data$past_spend, na.rm=TRUE) - min(data$past_spend, na.rm=TRUE)
var(data$past_spend, na.rm=TRUE)
sd(data$past_spend, na.rm=TRUE)
#age
mean(data$age, na.rm=TRUE)
median(data$age, na.rm=TRUE)
names(sort(-table(data$age)))[1]
max(data$age, na.rm=TRUE) - min(data$age, na.rm=TRUE)
var(data$age, na.rm=TRUE)
sd(data$age, na.rm=TRUE)
#time_web
mean(data$time_web, na.rm=TRUE)
median(data$time_web, na.rm=TRUE)
names(sort(-table(data$time_web)))[1]
max(data$time_web, na.rm=TRUE) - min(data$time_web, na.rm=TRUE)
var(data$time_web, na.rm=TRUE)
sd(data$time_web, na.rm=TRUE)
#ad_channel
table(data$ad_channel)
table(data$ad_channel)/length(data$ad_channel)
#voucher
table(data$voucher)
table(data$voucher)/length(data$voucher)

## Data Exploration: visualisation
ggplot(data)+geom_histogram(aes(spend), binwidth = NULL, fill = "#D8BFD8", color = "#800080", bin = NULL)
ggplot(data)+geom_histogram(aes(past_spend), binwidth = NULL, fill = "#D8BFD8", color = "#800080", bin = NULL)
ggplot(data)+geom_histogram(aes(age), binwidth = NULL, fill = "#D8BFD8", color = "#800080", bin = NULL)
ggplot(data)+geom_histogram(aes(time_web), binwidth = NULL, fill = "#D8BFD8", color = "#800080", bin = NULL)
ggplot(data, aes(x = spend, y = age)) +
  geom_point(aes(color = age, size = spend), alpha = 0.7) +
  scale_color_gradient(low = "#800080", high = "#D8BFD8") +
  labs(title = "Scatter Plot of Spend vs Age",
       x = "Spend (GBP)",
       y = "Age (years)") +
  theme_minimal()
ggplot(data, aes(x = spend, y = time_web)) +
  geom_point(aes(color = time_web, size = spend), alpha = 0.7) +
  scale_color_gradient(low = "#800080", high = "#D8BFD8") +
  labs(title = "Scatter Plot of Spend vs Time Spent on the Website",
       x = "Spend (GBP)",
       y = "Time Spent on the Website (seconds)") +
  theme_minimal()
ggplot(data, aes(x = spend, y = past_spend)) +
  geom_point(aes(color = time_web, size = spend), alpha = 0.7) +
  scale_color_gradient(low = "#800080", high = "#D8BFD8") +
  labs(title = "Scatter Plot of Spend vs Past Spending",
       x = "Spend (GBP)",
       y = "Past Spending (GBP)") +
  theme_minimal()

## Data Preparation
#handling missing data
sum(!complete.cases(data))
aggr(data, numbers=TRUE, prop=FALSE)
missdata <- data
missdata$missing <-as.numeric(!complete.cases(data))
imi <- mice (data, m = 20, maxit = 20, method = 'pmm', seed = 500)
mi <- complete(imi)
aggr(mi, numbers=TRUE, prop=FALSE)
summary(mi)
any(is.na(mi))
#checking correlation
write.csv(mi, "order_july24_imputed.csv", row.names = FALSE)
imputed_data <- read.csv("order_july24_imputed.csv", header=TRUE)
cor(imputed_data)
corrgram(mi)
#assumptions
ggplot(data=data) + geom_point(aes(age,spend,color=time_web)) +
  labs(title = "Age and Spending Plot with Time Spent on the Website Information", caption =
         "Data from PhysicalSound",tag = "Figure 1",x = "Age",y = "Spending")
ggplot(data=data) + geom_point(aes(age,spend,color=ad_channel)) +
  labs(title = "Age and Spendng Plot with Ad Channel Information", caption =
         "Data from PhysicalSound",tag = "Figure 2",x = "Age",y = "Spending")


## Modelling
model <- lm(spend~ ., data=imputed_data)
summary(model)
#assumptions
data$residuals <- residuals(model)
data$fitted <- fitted(model)
ggplot(data, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()
data$order <- 1:nrow(data)
ggplot(data, aes(x = order, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Order of Observations",
       x = "Order of Observations",
       y = "Residuals") +
  theme_minimal()


## Evaluation
print(summary(model)$r.squared)
print(summary(model)$adj.r.squared)
summary(model)$coefficients


## Training or Test Set
prediction <- predict(model, newdata = test_data)
prediction
test_data$predicted_spend <- prediction
ggplot(test_data, aes(x = age, y = predicted_spend, color = voucher)) +
  geom_point() +
  labs(title = "Predicted Spending for New Customers",
       x = "Age",
       y = "Predicted Spending",
       color = "Voucher") +
  theme_minimal()
ggplot(test_data, aes(x = time_web, y = predicted_spend, color = voucher)) +
  geom_point() +
  labs(title = "Predicted Spending for New Customers",
       x = "Time Spent on the Website",
       y = "Predicted Spending",
       color = "Voucher") +
  theme_minimal()





### PART 2


## Data Collection
data <- read.csv("Robot_Info.csv", header=TRUE)
dim(data)
colnames(data)
data


## Data Preparation

# creating table
dataTable <- data.frame(t(data))
colnames(dataTable) <- c("Carrying_Capacity", "Battery_Size", "Speed", 
                         "Mobility", "Aesthetic", "Cost_Per_Unit", "Reliability")
dataTable <- dataTable[-1, ]  
performanceTable <- data.frame(lapply(dataTable, as.numeric))  
performanceTable[is.na(performanceTable)] <- 0  
performanceTable
dim(performanceTable)

# assigning weights
## weights for scenario 1??
pairwise_matrix <- matrix(c(
  1, 5, 7, 7, 7, 9, 9,  
  1/5, 1, 5, 5, 7, 7, 9,  
  1/7, 1/5, 1, 1, 5, 7, 7,  
  1/7, 1/5, 1, 1, 5, 7, 7,  
  1/7, 1/7, 1/5, 1/5, 1, 5, 7,  
  1/9, 1/7, 1/7, 1/7, 1/5, 1, 5,  
  1/9, 1/9, 1/7, 1/7, 1/7, 1/5, 1), nrow = 7, byrow = TRUE) 

criteria <- c("Carrying_Capacity", "Battery_Size", "Speed", 
              "Mobility", "Aesthetic", "Cost_Per_Unit", "Reliability")
colnames(pairwise_matrix) <- criteria
rownames(pairwise_matrix) <- criteria
column_sums <- colSums(pairwise_matrix)
normalized_matrix <- sweep(pairwise_matrix, 2, column_sums, FUN = "/")
weights_1 <- rowMeans(normalized_matrix)
weights_1 <- as.numeric(weights_1)
names(weights_1) <- criteria
weights_1
length(weights_1)
## weights for scenario 2
weightsforcriteria <- c(0.15,0.20,0.10,0.10,0.05,0.20,0.20)
weights_2 <- data.frame(criteria, weightsforcriteria)
weights_2 <- data.frame(t(weights_2))
weights_2 <- weights_2[-1, ]
colnames(weights_2) <- criteria
weights_2 <- as.numeric(weightsforcriteria)
weights_2
length(weights_2)

# minimise maximise
criteriaMinMax <- c("max", "max", "max", "max", "max", "min", "max")
names(criteriaMinMax) <- colnames(dataTable)
criteriaMinMax

result_1 <- TOPSIS(performanceTable, weights_1, criteriaMinMax)
result_1
barplot(result_1, main="TOPSIS Score for Robots",
        xlab="Robots",
        names.arg=rownames(dataTable))

result_2 <- TOPSIS(performanceTable, weights_2, criteriaMinMax)
result_2
barplot(result_2, main="TOPSIS Score for Robots",
        xlab="Robots",
        names.arg=rownames(dataTable))









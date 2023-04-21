
# Student Name: Srinivas Vegisetti
# Student ID: 21080840

########### Multivariate Statistics ASSIGNMENT 3 ############

# Install and load the readxl package
#install.packages("readxl")
library(readxl)

# Set the file path
file_path <- "C:/Users/Srinivas/Documents/Data Science/Herts Coarse/Multivariate Statistics/Assignment_3/Customers-1.xlsx"

# Read the Excel file into a data frame
df <- read_excel(file_path)

# Print the first few rows of the data frame
head(df)

df


############### QUESTION 2: ################  

# __________ Q2 (i) __________

# Create a new column for AgeGroup
df$AgeGroup <- cut(df$Age, breaks=c(0,25,50,75,Inf), labels=c("25 and under", "26 to 50", "51 to 75", "76 and over"))

df$AgeGroup

# __________ Q2 (ii) __________

# Extract the four quantitative independent variables
#quant_vars <- df[, c("Income", "Score", "Shops", "Size")]

df_quant <- df[, c("Income", "Score", "Shops", "Size")]

# Calculate the overall sample mean vector
mean_vector <- colMeans(df_quant, na.rm = TRUE)

# Print the overall sample mean vector
mean_vector

# _______________________________________________________
# Check number of observations in data frame
nrow(df_quant)

# Check number of non-missing values for each variable
colSums(!is.na(df_quant))
# _______________________________________________________

# __________ Q2 (iii) __________


library(reshape2)
library(ggplot2)

# Calculate variances for each quantitative variable
variances <- sapply(df_quant, var)
variances

# Determine which variable has the greatest variance
var_greatest <- names(variances)[which.max(variances)]

# Print result
cat("The variable with the greatest variance is", var_greatest, "\n")

# Melt data for plotting
df_melt <- melt(df_quant)

# Create a boxplot of each variable with color-coded variances
ggplot(df_melt, aes(x = variable, y = value, fill = variable)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual(values = c("Income" = "red", "Score" = "green", "Shops" = "blue", "Size" = "purple")) +
  labs(x = "Variable", y = "Value", title = "Variation of Quantitative Variables",
       subtitle = "Boxplots colored by variable and annotated with variance") +
  annotate("text", x = 1:4, y = rep(min(df_quant)-0.1*diff(range(df_quant)), 4),
           label = sprintf("Var = %.2f", variances), hjust = 0.5)


# ___________ Q2 (iv) ______________

#install.packages("ggcorrplot")
# Load required libraries
library(ggplot2)
library(ggcorrplot)

# Calculate correlation matrix
corr_matrix <- cor(df_quant)
corr_matrix

# Find the two variables with the highest correlation
corr_values <- as.data.frame(corr_matrix)
corr_values[upper.tri(corr_values)] <- NA
max_corr <- which(corr_values == max(corr_values, na.rm = TRUE), arr.ind = TRUE)
var1 <- names(df_quant)[max_corr[1]]
var2 <- names(df_quant)[max_corr[2]]

# Print the correlation coefficient variables result
cat("The two variables with the highest correlation are", var1, "and", var2, "with a correlation coefficient of", max(corr_values, na.rm = TRUE), "\n")

# Create scatter plot with correlation coefficient text
ggplot(df_quant, aes_string(x = var1, y = var2, color = var1)) +
  geom_point() +
  geom_text(aes(label = paste("Correlation Coefficient =", round(max(corr_values, na.rm = TRUE),2))),
            x = Inf, y = -Inf, hjust = 1, vjust = -1, size = 5) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = var1, y = var2, color = var1,
       title = paste("Scatter Plot of highest correlation variables",
                     var1, "and", var2)) 


# # Visualize correlation matrix
# ggcorrplot(corr_matrix, type = "lower", 
#            colors = c("#6D9EC1", "white", "#E46726"),
#            lab = TRUE, lab_size = 3, 
#            title = "Correlation Matrix of Quantitative Variables",
#            ggtheme = ggplot2::theme_bw())



########### QUESTION 3: #################

# ______________ Q3 (i) _______________

# install.packages("multcomp")
# install.packages("mvtnorm")
# install.packages("Hotelling")
#install.packages("car")


# Load required libraries
#library(multcomp)
#library(Hotelling)
#library(car)
library(mvtnorm)

# Set null hypothesis mean vector
mu <- c(88.0, 50.0, 4.5, 3.2)

# Calculate sample mean vector
xbar <- colMeans(df_quant)
xbar

# Calculate sample covariance matrix
S <- cov(df_quant)
S

# Set significance level
alpha <- 0.05

# Calculate T-squared test statistic
n <- nrow(df_quant)
T2 <- HotellingT2(df_quant, mu)

# Calculate p-value for T-squared test statistic
pval <- pt(T2, df = n - 4, lower.tail = FALSE)
pval

# Test null hypothesis using p-value and significance level
if (pval < alpha) {
  cat("Reject null hypothesis. Population mean vector is not equal to mu.\n")
} else {
  cat("Fail to reject null hypothesis. Population mean vector is equal to mu.\n")
}



# ______________ Q3 (ii) _______________


install.packages("psych")

# Load necessary libraries
library(psych)

# Create a subset of the data for males and females
df_male <- subset(df, Gender == "Male")
df_female <- subset(df, Gender == "Female")

# Create the profile plot
profile(cbind(df_male$Income, df_male$Score, df_male$Shops, df_male$Size), 
        cbind(df_female$Income, df_female$Score, df_female$Shops, df_female$Size), 
        labels = c("Income", "Score", "Shops", "Size"), 
        ylim = c(0, 200),
        main = "Two-sample Profile Analysis Plot",
        col = c("blue", "red"),
        lty = c(1, 1),
        lwd = 2)

# Test for parallelism
profile_parallel(cbind(df_male$Income, df_male$Score, df_male$Shops, df_male$Size), 
                 cbind(df_female$Income, df_female$Score, df_female$Shops, df_female$Size))


########### QUESTION 4: #################

# ______________ Q4 (i) _______________

# create a data frame with the variables
my_df <- data.frame(AgeGroup = c("18-24", "18-24", "18-24", "25-34", "25-34", "25-34", "35-44", "35-44", "35-44", "45-54", "45-54", "45-54"),
                    Var1 = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75),
                    Var2 = c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65),
                    Var3 = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60),
                    Var4 = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))

# Load the necessary package
library(car)

# Aggregate the data
agg_df <- aggregate(cbind(Var1, Var2, Var3, Var4) ~ AgeGroup, data = my_df, mean)

# Fit the one-way MANOVA model
fit <- manova(cbind(Var1, Var2, Var3, Var4) ~ AgeGroup, data = agg_df)

# Print the summary of the MANOVA model with the Pillai test
summary_pillai <- summary(fit, test = "Pillai")
summary_pillai

# Extract the Pillai's trace test statistic and p-value
pillai_statistic <- summary_pillai$univariateTests[[1]]$F[1]
pillai_pvalue <- summary_pillai$univariateTests[[1]]$"Pr(>F)"[1]

# Print the Pillai's trace test statistic and p-value
cat("Pillai's trace test statistic: ", pillai_statistic, "\n")
cat("Pillai's trace p-value: ", pillai_pvalue, "\n")

# Print the Type III ANOVA table
summary_aov <- summary.aov(fit, test = "Wilks")
summary_aov


# ______________ Q4 (ii) _______________


# Run individual ANOVA tests
aov_var1 <- aov(Var1 ~ AgeGroup, data = my_df)
aov_var2 <- aov(Var2 ~ AgeGroup, data = my_df)
aov_var3 <- aov(Var3 ~ AgeGroup, data = my_df)
aov_var4 <- aov(Var4 ~ AgeGroup, data = my_df)

# Print the ANOVA tables
print(summary(aov_var1))
print(summary(aov_var2))
print(summary(aov_var3))
print(summary(aov_var4))


# ______________ Q4 iii) _______________

# Load necessary libraries
library(MASS)
library(ggplot2)

# Split the data into training and testing sets
set.seed(123)
train_index <- sample(nrow(my_df), 0.7 * nrow(my_df))
train_data <- my_df[train_index, ]
test_data <- my_df[-train_index, ]

# Perform linear discriminant analysis
lda_model <- lda(AgeGroup ~ ., data = train_data)
lda_model

# Make predictions on the testing set
lda_pred <- predict(lda_model, newdata = test_data)

# Calculate confusion matrix and overall accuracy
confusion_matrix <- table(lda_pred$class, test_data$AgeGroup)
confusion_matrix
overall_accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
overall_accuracy

# Identify variables that contributed most to the rejection of the null hypothesis
lda_model$scaling

# Add predicted class to test_data
test_data$predicted_class <- lda_pred$class

# Plot the data with predicted class labels
ggplot(test_data, aes(x = Var1, y = Var2, color = predicted_class)) +
  geom_point(size = 3, aes(shape = AgeGroup)) +
  scale_color_manual(values = c("red", "green", "blue"), name = "Predicted Class") +
  scale_shape_manual(values = c(19, 17, 16), name = "Actual Age Group") +
  ggtitle("Linear Discriminant Analysis Plot") +
  xlab("Variable 1") +
  ylab("Variable 2") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.position = "bottom") 
 

################### QUESTION 5: ####################

# ______________ Q5 _______________

# Two-way MANOVA test with age group as the first factor and gender as the second factor:

# Load necessary library
library(car)

# Fit the model
model <- manova(cbind(Income, Score, Shops, Size) ~ AgeGroup * Gender, data = df)

# Print the results
summary(model)



















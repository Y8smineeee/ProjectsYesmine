########################PROJECT 1 : STUDENT MATH###############################

                   ######## Loading The Dataset ###########

#Load Data and Identify empty cells and replace them with NA
Student <-read.table(file=file.choose(),header=TRUE,sep=",",dec=".",na.strings = c(""," ","NA", "N/A"))

                   ######## Data Description  ###########
##Fix the dataset
# Change column names to fix the labels
colnames(Student) <- c("Line_nb","student_id","school","sex","age",
"family_size", "parent_status", "mother_education","travel_time", "study_time",
"class_failures","school_support","family_support","extra_paid_classes","higher_ed","free_time","health", "absences","final_grade")

library(dplyr)
Student <- select(Student, -Line_nb)

## Understand the data
summary(Student)
dim(Student)
str(Student)
colnames(Student)

## Check for duplicates : 
duplicates_all <- duplicated(Student) # no duplicates
rsum(duplicates_all)
 
## Visualize the categorical columns
#Get the names of categorical columns
categorical_columns <- names(Student)[sapply(Student, is.character)]

#install.packages("patchwork")
#install.packages("purrr")
library(ggplot2)
library(patchwork)
library(purrr)

bar_plots <- lapply(names(Student), function(var) {
    if (is.factor(Student[[var]]) | is.character(Student[[var]])) {
      ggplot(Student, aes(x = .data[[var]], fill = .data[[var]])) +
        geom_bar() +
        labs(title = paste("bar plot of", var), x = "Catagory", y = "Frequency")
    } else NULL
  })
bar_plots <- Filter(Negate(is.null), bar_plots)
combined_plots <- reduce(bar_plots, `+`)
print(combined_plots)


                   ######## Data Preparation  ###########

########## Deal with outliers ##########

## view the outliers in the numerical features
numeric_columns <- names(Student)[sapply(Student, is.numeric)]
numeric_columns

## Gather numeric columns into long format
library(ggplot2)
library(tidyr)
student_long <- gather(Student, key = "variable", value = "value", all_of(numeric_columns))

ggplot(student_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "The Outliers Of The Numeric Columns") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Handle the outliers for each numeric column
for (col in numeric_columns) {
  # Identify outliers using Tukey's method
  outliers <- boxplot.stats(Student[[col]])$out
  # Replace outliers with NA
  Student[outliers, col] <- NA
}

####### Deal With missing Values #######

## Identify the columns with the missing values NA
columns_with_nulls <- colnames(Student)[colSums(is.na(Student)) > 0]
print(columns_with_nulls)

##View the Missing Values
#install.packages("visdat")
library(visdat)

vis_miss(Student)

## Find the columns with missing values exceeding 50%
columns_to_be_drop <- c()
for (col_name in columns_with_nulls)
{
 percentage_na_in_column <- sum(is.na(Student[[col_name]]))/nrow(Student)*100
 if (percentage_na_in_column > 50){
columns_to_be_drop <- c(columns_to_be_drop, col_name)
 }}
print(columns_to_be_drop)
## --> All columns have less than 50% missing values, so we won't drop any.

### Imputing the NA's ###

sum(is.na(Student))
## Imputing missing values using knn 
library(VIM)
# Perform kNN imputation for columns with missing values
imputed_data <- kNN(Student[columns_with_nulls], k = 5)
# Replace the columns with imputed values in the original data frame
Student[columns_with_nulls] <- imputed_data
sum(is.na(Student))
## --> We have no missing values left

##### Data Encoding #######

## Encode with the method One-hot  Encoding 
#install.packages("caret")
library(caret)

categorical_columns
Nominal_columns <- c("school","sex","parent_status","school_support","family_support","extra_paid_classes","higher_ed","family_size")
Nominal_columns
# Initialize encoded_data with original data
encoded_data <- Student
# One-hot encode each categorical column
for (col in Nominal_columns) {
  # Use dummyVars from caret to create dummy variables
  dummy_model <- dummyVars(~ ., data = encoded_data[, col, drop = FALSE])
  encoded_data <- cbind(encoded_data, predict(dummy_model, newdata = encoded_data))
}
# Remove original categorical columns
encoded_data <- encoded_data[, !(names(encoded_data) %in% Nominal_columns)]

summary(encoded_data)

## Encode with the method Ordinal  Encoding 
library(dplyr)

encoded_data$mother_education <- as.numeric(factor(encoded_data$mother_education,levels=c("none","primary education (4th grade)","5th to 9th grade","secondary education","higher education")))
encoded_data$travel_time <- as.numeric(factor(encoded_data$travel_time,levels=c("<15 min.", "15 to 30 min.", "30 min. to 1 hour",">1 hour"))) 
encoded_data$study_time <- as.numeric(factor(encoded_data$study_time,levels=c("<2 hours","2 to 5 hours","5 to 10 hours",">10 hours")))
str(encoded_data)
sum(is.na(encoded_data))
print(Student$mother_education)
print(encoded_data$mother_education)
# Print the resulting data frame
#print(encoded_data)
str(encoded_data)


                 ######### Univariate Analysis  ###########

## Normalization
minimums <- apply(encoded_data, MARGIN = 2, FUN = min)  # obtaining the minimums per variable
maximums <- apply(encoded_data, MARGIN = 2, FUN = max)  # obtaining the  maximums per variable
scaled<-as.data.frame(scale(encoded_data, center = minimums, scale = maximums - minimums))
summary(scaled)

par(mfrow=c(3,3))
hist(scaled$age, prob = TRUE, main = "Age", xlab = "age", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$age),col="#e63946")

hist(scaled$mother_education , prob = TRUE, main = "mother_education ", xlab = "mother_education ", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$mother_education ),col="#e63946")

hist(scaled$travel_time , prob = TRUE, main = "travel_time ", xlab = "travel_time ", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$travel_time ),col="#e63946")


hist(scaled$study_time  , prob = TRUE, main = "study_time ", xlab = "study_time ", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$study_time),col="#e63946")

hist(scaled$class_failures, prob = TRUE, main = "class_failures ", xlab = "class_failures ", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$class_failures ),col="#e63946")

hist(scaled$free_time, prob = TRUE, main = "free_time ", xlab = "free_time", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$free_time ),col="#e63946")

hist(scaled$health , prob = TRUE, main = "health ", xlab = "health ", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$health  ),col="#e63946")

hist(scaled$absences, prob = TRUE, main = "absences", xlab = "absences ", ylab = "final_grade", col = "#457B9D")
lines(density(scaled$absences),col="#e63946")

## Shapiro method
shapiro.test(scaled$age)
shapiro.test(scaled$mother_education)
shapiro.test(scaled$travel_time)
shapiro.test(scaled$study_time)
shapiro.test(scaled$class_failures)
shapiro.test(scaled$free_time)
shapiro.test(scaled$health)
shapiro.test(scaled$absences)
 
# --> p_value<0.05 : reject H0 et accept H1.
# --> None of these variables follows the normal law.
str(scaled)

                 #########  Bivariate analysis  ###########

#install.packages("corrplot")
library(corrplot)

par(mfrow=c(1,1))
corrplot(cor(scaled[,],method="s"), type = "lower", method = "number") #matrice de correlation
#if the correlation value is between -0.5 and 0.5 then weak correlation
#if correlation =0 then no relationship between the values

#By correlation:
#H0:r=0 => 2 independant variables
#H1:r<>0 => 2 dependant variables

cor.test(scaled$final_grade,scaled$study_time,method="spearman")
#p-value=0.08428>0.05 accept H0
#0.08697543 dependant

cor.test(scaled$final_grade,scaled$schoolGP,method="spearman")
#p-value=0.2003>0.05 accept H0
#0.06457126 dependant

cor.test(scaled$final_grade,scaled$age,method="spearman")
#p-value=0.000305<0.05 accept H1
#-0.1807644 dependant

cor.test(scaled$final_grade,scaled$travel_time,method="spearman")
#p-value=0.01601 <0.05 accept H1
#-0.1211335 dependant

cor.test(scaled$final_grade,scaled$mother_education,method="spearman")
#p-value=6.794e-0.6 <<0.05 accept H1
#0.224243 dependant

cor.test(scaled$final_grade,scaled$extra_paid_classesyes,method="spearman")
#p-value=0.1319>0.05 accept H0
#0.07594291 dependant

cor.test(scaled$final_grade,scaled$higher_edyes,method="spearman")
#p-value = 0.0005781<0.05 accept H1
#0.1724179 dependant

cor.test(scaled$final_grade,scaled$parent_statusApart,method="spearman")
#p-value = 0.3803 >0.05 accept H0
#0.04426248 dependant

cor.test(scaled$final_grade,scaled$free_time,method="spearman")
#p-value = 0.7766 >0.05 accept H0
#-0.0143197 dependant

cor.test(scaled$final_grade,scaled$health,method="spearman")
#p-value = 0.3435 accept H0
#-0.04778951 dependant

cor.test(scaled$final_grade,scaled$class_failures,method="spearman")
# p-value = 1.323e-13 <<<0.05 accept H1
#-0.3610318 dependant

cor.test(scaled$final_grade,scaled$absences,method="spearman")
#p-value = 0.5132>0.05 accept H0
#0.03299192 dependant

cor.test(scaled$final_grade,scaled$sexM,method="spearman")
#p-value = 0.04166<0.05 accept H1
#0.102543 dependant

                 #########  Statistical -- HAC  ###########
## Split the label column
summary(scaled)
scaled_cl<-scaled
scaled_cl
scaled_grade<- scaled$final_grade
scaled_cl$final_grade<- NULL
str(scaled_cl)


m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")
library(cluster)
library(factoextra)
## Function to compute coefficient
ac <- function(x) {
  agnes(scaled_cl, method = x)$ac
}
#install.packages("tidyverse")
library(tidyverse)

map_dbl(m, ac)
#average    single  complete      ward 
#0.7746137 0.6836824 0.8303977 0.955687
#so we use ward method

## HAC 
dist_mat <- dist(scaled_cl, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 2)
plot(hclust_avg)
rect.hclust(hclust_avg , k = 2, border = 2:6)
abline(h = 2, col = 'red')

# Add cluster membership to the original dataset
Student_clusters <- cbind(Student, Cluster = cut_avg)
# Summary statistics for each cluster
summary_by_cluster <- aggregate(. ~ Cluster, data = Student_clusters, FUN = summary)
summary_by_cluster
#---> Cluster 1 : 164 , Cluster 2 : 231

#T-test for a numeric variable between clusters
t_test_result <- t.test(final_grade ~ Cluster, data = Student_clusters)
print(t_test_result)
#--->p_value=0.3409>0.05 : we accept H0 the null Hypothesis.
#based on the final_grade variable,there is no statistically
#significant difference between the two clusters.

###Splitting into train and test sets
#install.packages(c("caret", "MASS"))
library(MASS)

set.seed(123)

train_data <-scaled[index, ]
test_data <- scaled[-index, ]


#### Linear Regression

model=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes,data=train_data)
summary(model)
resid(model)

model2=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes,data=train_data)
summary(model2)

model3=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart,data=train_data)
summary(model3)

model4=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart-study_time,data=train_data)
summary(model4)

model5=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart-study_time-higher_edyes  ,data=train_data)
summary(model5)

model6=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart-study_time-higher_edyes-free_time,data=train_data)
summary(model6)

model7=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart-study_time-higher_edyes-free_time-travel_time,data=train_data)
summary(model7)

model8=model7=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart-study_time-higher_edyes-free_time-travel_time-family_supportyes,data=train_data)
summary(model8)

model9=lm(final_grade~study_time+age+mother_education+travel_time+class_failures+free_time+health+absences+parent_statusApart+school_supportyes+family_supportyes+extra_paid_classesyes+higher_edyes-extra_paid_classesyes-parent_statusApart-study_time-higher_edyes-free_time-travel_time-family_supportyes-absences,data=train_data)
summary(model9)

##                  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        0.57695    0.05144  11.215  < 2e-16 ***
#age               -0.13674    0.07583  -1.803   0.0725 .  
#mother_education   0.10284    0.04951   2.077   0.0387 *  
#class_failures    -0.28355    0.05988  -4.735 3.52e-06 ***
#health            -0.07969    0.03750  -2.125   0.0345 *  
#school_supportyes -0.07540    0.03830  -1.969   0.0500 *  

## Evaluattion
AIC(model) # -53.55753
AIC(model2) # -55.41563
AIC(model3) # -57.07268
AIC(model4) # -58.56792
AIC(model5) # -60.16352
AIC(model6) # -61.45641
AIC(model7) # -62.6252
AIC(model8) # -62.6252
AIC(model9) # -62.28161

## --> The smaller the AIC value, the better the model fit.

#install.packages('pROC')
library(pROC)

predictions <- predict(model4,newdata = test_data, type = "response")
roc_curve <- multiclass.roc(test_data$final_grade, predictions)
auc(roc_curve)

predictions <- predict(model6,newdata = test_data, type = "response")
roc_curve <- multiclass.roc(test_data$final_grade, predictions)
auc(roc_curve)
